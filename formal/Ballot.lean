-- SPDX-License-Identifier: GPL-3.0
-- 形式化 Lean 4 代码 (Ballot 投票合约)

import Init.Data.Nat.Basic
import Init.Data.Nat.Lemmas
import Init.Data.Array.Basic

-- ======== 1. 基础定义 ========

structure Address where
  val : String
deriving DecidableEq, Inhabited, Repr

def zeroAddress : Address := ⟨""⟩
abbrev UInt := Nat
abbrev Bytes32 := String -- 简化 'bytes32'

-- 选民结构
structure Voter where
  weight : UInt   -- 权重
  voted : Bool    -- 是否已投票
  delegate : Address -- 委托给谁
  vote : UInt      -- 投了哪个提案 ID
deriving Inhabited, Repr

-- 提案结构
structure Proposal where
  name : Bytes32
  voteCount : UInt
deriving Inhabited, Repr

-- ======== 2. 状态、事件与错误 ========

-- Ballot 合约的状态结构体
structure State where
  chairperson : Address
  voters : Address → Voter
  proposals : Array Proposal
deriving Inhabited

-- 交易上下文
structure Context where
  sender : Address

-- Ballot 合约自定义错误 (用于 Revert)
inductive BallotError where
  | NotChairperson      -- (性质1/5)
  | AlreadyVoted        -- (性质2)
  | NoVotingWeight      -- (性质1)
  | HasVotingWeight     -- (性质1)
  | InvalidProposal
  | SimpleSelfDelegation -- (性质3)
deriving DecidableEq, Inhabited

-- ======== 3. 函数式模型辅助类型 ========

-- 成功执行的输出
structure TransitionOutput (α : Type) where
  newState : State
  -- (Ballot 合约没有事件)
  returnValue : α

-- 函数调用的结果
abbrev CallResult (α :Type) := Except BallotError (TransitionOutput α)

-- ======== 4. 构造函数 ========

def constructor (ctx : Context) (_proposalNames : Array Bytes32) : State :=
  let initialProposals := _proposalNames.map (fun name => { name := name, voteCount := 0 })
  let initialVoters := fun (a : Address) =>
    if a == ctx.sender then
      -- 主席获得 1 票
      { weight := 1, voted := false, delegate := zeroAddress, vote := 0 }
    else
      { weight := 0, voted := false, delegate := zeroAddress, vote := 0 }

  {
    chairperson := ctx.sender,
    voters := initialVoters,
    proposals := initialProposals
  }

-- ======== 5. 核心函数 (函数式转换) ========

 --
-- 函数: giveRightToVote(voter)
--
def giveRightToVote (ctx : Context) (s_old : State) (voterAddr : Address) : Except BallotError (TransitionOutput Unit) := do

  let voter := s_old.voters voterAddr

  -- 守卫 G_1.1: 只有主席
  if ctx.sender != s_old.chairperson then
    throw BallotError.NotChairperson

  -- 守卫 G_1.2: 尚未投票
  if voter.voted then
    throw BallotError.AlreadyVoted

  -- 守卫 G_1.3: 权重必须为 0
  if voter.weight != 0 then
    throw BallotError.HasVotingWeight

  -- 动作 F_1:
  let s_new := { s_old with
    voters := fun a =>
      if a == voterAddr then { voter with weight := 1 }
      else s_old.voters a
  }

  return { newState := s_new, returnValue := () }

--
-- 函数: vote(proposalId)
--
def vote (ctx : Context) (s_old : State) (proposalId : UInt) : Except BallotError (TransitionOutput Unit) := do

  let voter := s_old.voters ctx.sender

  -- 守卫 G_2.1: 必须有权重
  if voter.weight = 0 then
    throw BallotError.NoVotingWeight

  -- 守卫 G_2.2: 未投票
  if voter.voted then
    throw BallotError.AlreadyVoted

  -- 守卫 G_2.3: 提案 ID 有效
  if proposalId >= s_old.proposals.size then
    throw BallotError.InvalidProposal

  -- 动作 F_2:
  -- [修复] 使用 .set! 和 [!] 语法
  let s_new := { s_old with
    voters := fun a =>
      if a == ctx.sender then { voter with voted := true, vote := proposalId }
      else s_old.voters a,
    proposals := s_old.proposals.set! proposalId
      { (s_old.proposals[proposalId]!) with
        voteCount := (s_old.proposals[proposalId]!).voteCount + voter.weight
      }
  }

  return { newState := s_new, returnValue := () }

--
-- 函数: delegate(to)
-- (注意: 此模型简化了 'while' 循环, 仅模拟单跳委托)
--
def delegate (ctx : Context) (s_old : State) (toAddr : Address) : Except BallotError (TransitionOutput Unit) := do

  let senderVoter := s_old.voters ctx.sender
  let delegateeVoter := s_old.voters toAddr

  -- 守卫 G_3.1: 未投票
  if senderVoter.voted then
    throw BallotError.AlreadyVoted

  -- 守卫 G_3.2: 禁止简单自委托
  if toAddr == ctx.sender then
    throw BallotError.SimpleSelfDelegation

  -- 动作 F_3: (更新发送者)
  let s_mid := { s_old with
    voters := fun a =>
      if a == ctx.sender then { senderVoter with voted := true, delegate := toAddr }
      else s_old.voters a
  }

  -- 动作 F_3: (更新接收者或提案)
  let s_new :=
    -- (性质3) 如果被委托人已投票, 将票数加到提案
    if delegateeVoter.voted then
      let proposalId := delegateeVoter.vote

      -- [修复] 必须检查被委托人投票的 proposalId 是否也有效
      if proposalId >= s_mid.proposals.size then
        -- 理论上这不应该发生 (如果 'vote' 函数是唯一修改 'voter.vote' 的方式)
        -- 但为了安全, 我们添加一个检查
        s_mid -- 保持状态不变 (或者也可以 'throw BallotError.InvalidProposal')
      else
        -- [修复] 使用 .set! 和 [!] 语法
        { s_mid with
          proposals := s_mid.proposals.set! proposalId
            { (s_mid.proposals[proposalId]!) with
              voteCount := (s_mid.proposals[proposalId]!).voteCount + senderVoter.weight
            }
        }
    -- (性质3) 如果被委托人未投票, 将权重加给他
    else
      { s_mid with
        voters := fun a =>
          if a == toAddr then { delegateeVoter with weight := delegateeVoter.weight + senderVoter.weight }
          else s_mid.voters a
      }

  return { newState := s_new, returnValue := () }

-- ======== 6. (性质4) 计票函数 (View) ========

-- ======== 预设性质集 (Ballot) ========
-- 性质 1: 无权重者不得投票
theorem prop1_NoVotingWeight_cannot_vote
  (ctx : Context) (s_old : State) (proposalId : UInt)
  (h_no_weight : (s_old.voters ctx.sender).weight = 0) :
  vote ctx s_old proposalId = .error BallotError.NoVotingWeight := by
  dsimp [vote]
  -- 命中第一层 guard
  simp [h_no_weight]
  rfl

-- 性质 1 & 5: 非主席不能赋权
theorem prop1_NotChairperson_cannot_giveRight
  (ctx : Context) (s_old : State) (voterAddr : Address)
  (h_not_chairperson : ctx.sender != s_old.chairperson) :
  giveRightToVote ctx s_old voterAddr = .error BallotError.NotChairperson := by
  dsimp [giveRightToVote]
  -- 命中第一层 guard
  simp [h_not_chairperson]
  rfl

-- 性质 2: 已投者再次投票必拒 (需补充：权重非 0，否则会先报 NoVotingWeight)
theorem prop2_AlreadyVoted_cannot_vote
  (ctx : Context) (s_old : State) (proposalId : UInt)
  (h_has_weight : (s_old.voters ctx.sender).weight ≠ 0)
  (h_voted : (s_old.voters ctx.sender).voted = true) :
  vote ctx s_old proposalId = .error BallotError.AlreadyVoted := by
  -- 先排除“权重=0”的分支
  by_cases h0 : (s_old.voters ctx.sender).weight = 0
  · exact False.elim (h_has_weight h0)
  · dsimp [vote]
    -- 第一层 (weight=0) 走 else；第二层 (voted=true) 命中 AlreadyVoted
    simp [h0, h_voted]
    rfl


-- 性质 2: 已投者不得再次委托
theorem prop2_AlreadyVoted_cannot_delegate
  (ctx : Context) (s_old : State) (toAddr : Address)
  (h_voted : (s_old.voters ctx.sender).voted = true) :
  delegate ctx s_old toAddr = .error BallotError.AlreadyVoted := by
  dsimp [delegate]
  -- 命中第一层 guard
  simp [h_voted]
  rfl

/-- 简单的最大票数下标（空数组时返回 0） -/
def winningProposal (s : State) : Nat :=
  if h : s.proposals.size = 0 then
    0
  else
    let n := s.proposals.size
    -- 线性扫描 argmax
    let rec go (i best : Nat) (bestVal : UInt) : Nat :=
      if hi : i < n then
        let vi := (s.proposals[i]!).voteCount
        if vi > bestVal then go (i+1) i vi else go (i+1) best bestVal
      else best
    have h0 : 0 < n := Nat.pos_of_ne_zero h
    go 1 0 (s.proposals[0]!).voteCount


