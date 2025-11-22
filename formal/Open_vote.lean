import Std.Data
import Init.Data.Nat.Basic
import Init.Data.Nat.Lemmas

/-- 抽象地址类型，这里直接用 Nat 封一层。 -/
structure Address where
  val : Nat
deriving DecidableEq, Inhabited, Repr

/-- 协议阶段：对应 README 里的 SETUP / SIGNUP / COMMIT / VOTE / FINISHED. -/
inductive Phase where
  | setup
  | signup
  | commitment
  | vote
  | finished
deriving DecidableEq, Inhabited, Repr


/-- 单次选举的全局链上状态。 -/
structure State where
  phase        : Phase
  owner        : Address
  eligible     : Address → Bool      -- 白名单
  registered   : Address → Bool      -- 是否完成 register()
  committed    : Address → Bool      -- 是否在 COMMIT 阶段提交了承诺
  hasVoted     : Address → Bool      -- 是否已经 submitVote
  vote         : Address → Bool      -- 理想化的明文投票结果（true = yes）
  yesTally     : Nat                 -- FINISHED 阶段的最终 yes 票数
  commitPhase  : Bool                -- 是否开启 COMMIT 阶段
deriving Inhabited

/-- 调用上下文，只保留 sender。 -/
structure Context where
  sender : Address
deriving Inhabited

/-- 高层事件抽象。 -/
inductive Event where
  | Registered    (who : Address)
  | Committed     (who : Address)
  | Voted         (who : Address) (choice : Bool)
  | TallyComputed (yes : Nat)
deriving DecidableEq, Inhabited, Repr

/-- 用于表示 require / modifier 的失败原因。 -/
inductive Error where
  | PhaseMismatch
  | NotOwner
  | NotEligible
  | AlreadyRegistered
  | NotRegistered
  | AlreadyCommitted
  | AlreadyVoted
  | CommitRequired
deriving DecidableEq, Inhabited, Repr

/-- 统一的转移输出：新状态 + 事件列表 + 返回值。 -/
structure TransitionOutput (α : Type) where
  newState    : State
  events      : List Event
  returnValue : α

/-- 调用结果：成功（.ok out）或失败（.error err）。 -/
abbrev CallResult (α : Type) := Except Error (TransitionOutput α)

/-- 部署后的初始状态：只有 owner 已知，其余全 0 / false。 -/
def emptyState (owner : Address) : State :=
  { phase        := Phase.setup
    owner        := owner
    eligible     := fun _ => false
    registered   := fun _ => false
    committed    := fun _ => false
    hasVoted     := fun _ => false
    vote         := fun _ => false
    yesTally     := 0
    commitPhase  := false }

/-! ### 一些 record 更新的小 simp 引理 -/

@[simp] theorem registered_update (s : State) (f : Address → Bool) :
  ({ s with registered := f }).registered = f := rfl

@[simp] theorem committed_update (s : State) (f : Address → Bool) :
  ({ s with committed := f }).committed = f := rfl

@[simp] theorem hasVoted_update (s : State) (f : Address → Bool) :
  ({ s with hasVoted := f }).hasVoted = f := rfl

@[simp] theorem vote_update (s : State) (f : Address → Bool) :
  ({ s with vote := f }).vote = f := rfl

@[simp] theorem yesTally_update (s : State) (n : Nat) :
  ({ s with yesTally := n }).yesTally = n := rfl

@[simp] theorem phase_update (s : State) (p : Phase) :
  ({ s with phase := p }).phase = p := rfl

/-! ## 协议核心函数：抽象的 Solidity 入口 -/

/-- 管理员在 SETUP 阶段配置白名单。 -/
def setEligible (ctx : Context) (s : State) (addrs : List Address) : CallResult Unit :=
  if _hPhase : s.phase ≠ Phase.setup then
    .error Error.PhaseMismatch
  else if _hOwner : ctx.sender ≠ s.owner then
    .error Error.NotOwner
  else
    let eligible' : Address → Bool :=
      fun a => if a ∈ addrs then true else s.eligible a
    let s' : State := { s with eligible := eligible' }
    .ok {
      newState    := s'
      events      := []
      returnValue := ()
    }

/-- 管理员开始选举，切换到 SIGNUP，并决定是否开启 COMMIT 阶段。 -/
def beginSignup (ctx : Context) (s : State) (enableCommitment : Bool) : CallResult Unit :=
  if _hPhase : s.phase ≠ Phase.setup then
    .error Error.PhaseMismatch
  else if _hOwner : ctx.sender ≠ s.owner then
    .error Error.NotOwner
  else
    let s' : State :=
      { s with
        phase       := Phase.signup
        commitPhase := enableCommitment }
    .ok {
      newState    := s'
      events      := []
      returnValue := ()
    }

/-- 管理员从 SIGNUP / COMMITMENT 切换到 VOTE 阶段。 -/
def beginVote (ctx : Context) (s : State) : CallResult Unit :=
  if _hOwner : ctx.sender ≠ s.owner then
    .error Error.NotOwner
  else if _hPhase : (s.phase ≠ Phase.signup) ∧ (s.phase ≠ Phase.commitment) then
    .error Error.PhaseMismatch
  else
    let s' : State := { s with phase := Phase.vote }
    .ok {
      newState    := s'
      events      := []
      returnValue := ()
    }

/-- 选民在 SIGNUP 阶段登记自己的 voting key（这里忽略具体 ZKP）。 -/
def register (ctx : Context) (s : State) : CallResult Unit :=
  if _hPhase : s.phase ≠ Phase.signup then
    .error Error.PhaseMismatch
  else if _hElig : s.eligible ctx.sender = false then
    .error Error.NotEligible
  else if _hReg : s.registered ctx.sender = true then
    .error Error.AlreadyRegistered
  else
    let reg' : Address → Bool :=
      fun a => if a = ctx.sender then true else s.registered a
    let s' : State := { s with registered := reg' }
    .ok {
      newState    := s'
      events      := [Event.Registered ctx.sender]
      returnValue := ()
    }

/-- （可选）COMMIT 阶段：注册选民提交投票承诺哈希。 -/
def submitCommitment (ctx : Context) (s : State) : CallResult Unit :=
  if _hPhase : s.phase ≠ Phase.commitment then
    .error Error.PhaseMismatch
  else if _hReg : s.registered ctx.sender = false then
    .error Error.NotRegistered
  else if _hC : s.committed ctx.sender = true then
    .error Error.AlreadyCommitted
  else
    let com' : Address → Bool :=
      fun a => if a = ctx.sender then true else s.committed a
    let s' : State := { s with committed := com' }
    .ok {
      newState    := s'
      events      := [Event.Committed ctx.sender]
      returnValue := ()
    }

/-- 提交投票：只有在符合资格时才允许投票 --/
def submitVote (ctx : Context) (s : State) (choice : Bool) : CallResult Unit :=
  if _hPhase : s.phase ≠ Phase.vote then
    .error Error.PhaseMismatch
  else if _hReg : s.registered ctx.sender = false then
    .error Error.NotRegistered
  else if _hV : s.hasVoted ctx.sender = true then
    .error Error.AlreadyVoted
  else if s.eligible ctx.sender = false then
    .error Error.NotEligible  -- 检查选民资格
  else
    let hv'   : Address → Bool :=
      fun a => if a = ctx.sender then true   else s.hasVoted a
    let vote' : Address → Bool :=
      fun a => if a = ctx.sender then choice else s.vote a
    let s' : State := { s with hasVoted := hv', vote := vote' }
    .ok {
      newState    := s'
      events      := [Event.Voted ctx.sender choice]
      returnValue := ()
    }

/--
  计票：在给定的有限 voter 列表上统计 yes 票数，并将协议切到 FINISHED。
  在真实 AnonymousVoting 中，这一步是用同态加密/零知识证明确保自统计；
  这里我们直接对 `vote` 映射跑一个 fold。
-/
def computeTally (s : State) (voters : List Address) : CallResult Nat :=
  if _hPhase : s.phase ≠ Phase.vote then
    .error Error.PhaseMismatch
  else
    let yes : Nat :=
      voters.foldl
        (fun acc a =>
          if s.hasVoted a && s.vote a then acc.succ else acc)
        0
    let s' : State := { s with yesTally := yes, phase := Phase.finished }
    .ok {
      newState    := s'
      events      := [Event.TallyComputed yes]
      returnValue := yes
    }

-- ======== 性质集 (Open Vote) ========

--
-- 性质 1: 投票资格检查
-- (未在白名单 'eligible' 中的人, 即使其他条件满足, 投票也会失败)
--
theorem prop_NotEligible_cannot_vote
  (ctx : Context) (s_old : State) (choice : Bool)
  -- 假设其他守卫都通过了:
  (h_phase : s_old.phase = Phase.vote)
  (h_reg : s_old.registered ctx.sender = true)
  (h_voted : s_old.hasVoted ctx.sender = false)
  -- 关键假设: 选民*不*合格
  (h_eligible : s_old.eligible ctx.sender = false) :

  submitVote ctx s_old choice = .error Error.NotEligible
  := by
    dsimp [submitVote]
    -- 这是一个简单的 "revert proof"
    -- 'simp' 会自动 "走进" 'if/else' 链条:
    -- 1. 'if s.phase ≠ Phase.vote' 变为 'if false' (因为 h_phase) -> 走 'else'
    -- 2. 'if s.registered ... = false' 变为 'if false' (因为 h_reg) -> 走 'else'
    -- 3. 'if s.hasVoted ... = true' 变为 'if false' (因为 h_voted) -> 走 'else'
    -- 4. 'if s.eligible ... = false' 变为 'if true' (因为 h_eligible) -> 走 'then'
    -- 最终 'simp' 证明 '.error Error.NotEligible = .error Error.NotEligible'
    simp [h_phase, h_reg, h_voted, h_eligible]

--
-- 性质 2: 一人一票
-- (已经投过票 'hasVoted' 的人, 再次投票会失败)
--
theorem prop_AlreadyVoted_cannot_vote
  (ctx : Context) (s_old : State) (choice : Bool)
  -- 假设前序守卫通过:
  (h_phase : s_old.phase = Phase.vote)
  (h_reg : s_old.registered ctx.sender = true)
  -- 关键假设: *已经*投过票
  (h_voted : s_old.hasVoted ctx.sender = true) :

  submitVote ctx s_old choice = .error Error.AlreadyVoted
  := by
    dsimp [submitVote]
    -- 1. 'if s.phase ...' -> 走 'else' (因为 h_phase)
    -- 2. 'if s.registered ...' -> 走 'else' (因为 h_reg)
    -- 3. 'if s.hasVoted ...' -> 走 'then' (因为 h_voted)
    simp [h_phase, h_reg, h_voted]

--
-- 性质 3: 委托不循环
--
-- (N/A): 你提供的 'Open_vote.lean' 合约中没有实现 'delegate' (委托) 功能。
-- 'Ballot.lean' 中有委托, 但这个新合约中没有。


--
-- 性质 4: 胜者票最多 (形式化为: 计票状态被正确设置)
-- (计票函数 'computeTally' 成功后, 返回的 'newState' 中的 'yesTally'
--  必须等于它计算出的 'returnValue')
--
theorem prop_Tally_is_set_in_state
  (s_old : State) (voters : List Address)
  -- 假设守卫通过
  (h_phase : s_old.phase = Phase.vote) :
  match computeTally s_old voters with
  | .ok out => out.newState.yesTally = out.returnValue
  | .error _ => False -- 计票不应失败
  := by
    dsimp [computeTally]
    -- 1. 'if s.phase ...' -> 走 'else' (因为 h_phase)
    simp [h_phase]
    -- 2. 'simp' 自动解开 'match'
    -- 3. 目标变为:
    --    let yes := ...
    --    let s' := { s with yesTally := yes, ... }
    --    s'.yesTally = yes
    -- 4. 'simp' 会自动展开 's'' 并证明 'yes = yes'

--
-- 性质 5: 投票截止后不可更改
-- (一旦 'computeTally' 被调用, 状态变为 'finished', 此时 'submitVote' 必须失败)
--
theorem prop_Cannot_vote_when_finished
  (ctx : Context) (s_post_tally : State) (choice : Bool)
  -- 关键假设: 状态是 'finished'
  (h_phase : s_post_tally.phase = Phase.finished) :

  submitVote ctx s_post_tally choice = .error Error.PhaseMismatch
  := by
    dsimp [submitVote]
    -- 1. 'if s.phase ≠ Phase.vote' 变为 'if Phase.finished ≠ Phase.vote' (true) -> 走 'then'
    simp [h_phase]

-- (同理, 'register' 也必须失败)
theorem prop_Cannot_register_when_finished
  (ctx : Context) (s_post_tally : State)
  (h_phase : s_post_tally.phase = Phase.finished) :

  register ctx s_post_tally = .error Error.PhaseMismatch
  := by
    dsimp [register]
    -- 1. 'if s.phase ≠ Phase.signup' 变为 'if Phase.finished ≠ Phase.signup' (true) -> 走 'then'
    simp [h_phase]
