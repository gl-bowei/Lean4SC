import Init.Data.Nat.Basic
import Init.Data.Nat.Lemmas

/-
  ERC-20 在 Lean 里的一个“干净版”建模：
  * Address 有 DecidableEq, 不再强制用 BEq / (==)
  * 状态 State, 上下文 Context, 事件 / 错误 / TransitionOutput / CallResult
  * transfer / approve / transferFrom 都采用 Prop 版 if 守卫
  * 总量守恒定理：transfer / transferFrom 都不改变 totalSupply
-/

-- ======== 1. 基础定义 ========

structure Address where
  val : String
deriving DecidableEq, Inhabited, Repr

def zeroAddress : Address := ⟨""⟩
abbrev UInt := Nat

-- ======== 2. 状态、事件与错误 ========

structure State where
  totalSupply : UInt
  balanceOf   : Address → UInt
  allowance   : Address → Address → UInt
deriving Inhabited

structure Context where
  sender : Address

inductive ERC20Event where
  | Transfer (from_ : Address) (to_ : Address) (amount : UInt)
  | Approval (owner : Address) (spender : Address) (amount : UInt)
deriving DecidableEq, Inhabited

inductive ERC20Error where
  | InsufficientBalance
  | InsufficientAllowance
  | InvalidRecipientZeroAddress
  | Overflow
deriving DecidableEq, Inhabited

structure TransitionOutput (α : Type) where
  newState   : State
  events     : List ERC20Event
  returnValue : α

abbrev CallResult (α : Type) := Except ERC20Error (TransitionOutput α)

-- ======== 3. 构造函数 ========

def constructor (initialHolder : Address) (initialSupply : UInt) : State :=
  { totalSupply := initialSupply
  , balanceOf   := fun a => if a = initialHolder then initialSupply else 0
  , allowance   := fun _ _ => 0
  }

-- ======== 4. 核心函数：transfer / approve / transferFrom ========

/-
  这里的一个关键设计：所有“require”都用 Prop if：

    if h0  : recipient = zeroAddress then error ...
    else if hb  : amount ≤ s.balanceOf ctx.sender then ...
    else error InsufficientBalance

  而不是 Bool ==；这样在证明里可以直接用 h_not_zero : recipient ≠ zeroAddress
  和 h_balance : amount ≤ ... 通过 `simp [transfer, h_not_zero, h_balance, ...]` 把分支化掉。
-/

def transfer (ctx : Context) (s : State) (recipient : Address) (amount : UInt)
  : CallResult Bool :=
  if recipient = zeroAddress then
    .error ERC20Error.InvalidRecipientZeroAddress
  else if ¬ (amount ≤ s.balanceOf ctx.sender) then
    .error ERC20Error.InsufficientBalance
  else if ¬ (s.balanceOf recipient + amount ≥ s.balanceOf recipient) then
    .error ERC20Error.Overflow
  else if ctx.sender = recipient then
    .ok {
      newState   := s,
      events     := [ERC20Event.Transfer ctx.sender recipient amount],
      returnValue := true
    }
  else
    let s_new : State :=
      { s with
        balanceOf := fun a =>
          if a = ctx.sender then s.balanceOf a - amount
          else if a = recipient then s.balanceOf a + amount
          else s.balanceOf a }
    .ok {
      newState   := s_new,
      events     := [ERC20Event.Transfer ctx.sender recipient amount],
      returnValue := true
    }


def approve
  (ctx : Context) (s : State) (spender : Address) (amount : UInt)
  : CallResult Bool :=
  let s_new : State :=
    { s with
      allowance := fun owner sp =>
        if owner = ctx.sender ∧ sp = spender then amount
        else s.allowance owner sp
    }
  .ok {
    newState   := s_new,
    events     := [ERC20Event.Approval ctx.sender spender amount],
    returnValue := true
  }

def transferFrom
  (ctx : Context) (s : State) (src : Address) (recipient : Address) (amount : UInt)
  : CallResult Bool :=
  -- 守卫 G3.1：收款地址不能是 0
  if h0 : recipient = zeroAddress then
    .error ERC20Error.InvalidRecipientZeroAddress
  else
    -- 守卫 G3.2：src 余额足够
    if _hb : amount ≤ s.balanceOf src then
      -- 守卫 G3.3：津贴足够
      if _ha : amount ≤ s.allowance src ctx.sender then
        -- 守卫 G3.4：简单的“无溢出”
        if _ho : s.balanceOf recipient + amount ≥ s.balanceOf recipient then
          -- 先扣 allowance
          let s_mid : State :=
            { s with
              allowance := fun owner sp =>
                if owner = src ∧ sp = ctx.sender then s.allowance owner sp - amount
                else s.allowance owner sp
            }
          -- 再区分自转账 / 正常转账
          if _hself : src = recipient then
            .ok {
              newState   := s_mid,
              events     := [ERC20Event.Transfer src recipient amount],
              returnValue := true
            }
          else
            let s_new : State :=
              { s_mid with
                balanceOf := fun a =>
                  if a = src then s_mid.balanceOf a - amount
                  else if a = recipient then s_mid.balanceOf a + amount
                  else s_mid.balanceOf a
              }
            .ok {
              newState   := s_new,
              events     := [ERC20Event.Transfer src recipient amount],
              returnValue := true
            }
        else
          .error ERC20Error.Overflow
      else
        .error ERC20Error.InsufficientAllowance
    else
      .error ERC20Error.InsufficientBalance

-- ======== 5. 一些简单的 simp 引理（辅助证明） ========

@[simp] theorem totalSupply_update_balance
  (s : State) (f : Address → UInt) :
  ({ s with balanceOf := f }).totalSupply = s.totalSupply := rfl

@[simp] theorem allowance_update_balance
  (s : State) (f : Address → UInt) :
  ({ s with balanceOf := f }).allowance = s.allowance := rfl

@[simp] theorem totalSupply_update_allowance
  (s : State) (f : Address → Address → UInt) :
  ({ s with allowance := f }).totalSupply = s.totalSupply := rfl

@[simp] theorem balanceOf_update_allowance
  (s : State) (f : Address → Address → UInt) :
  ({ s with allowance := f }).balanceOf = s.balanceOf := rfl

-- ======== 6. 性质：transfer / transferFrom 不改变 totalSupply ========

/-
  这里的定理风格刻意选成你原来那种：

    match h_out : transfer ... with
    | .ok out   => out.newState.totalSupply = s.totalSupply
    | .error _  => False

  好处：
  * “不可能走 error” + “若成功则总量守恒” 一句话表达。
  * 证明时套路是：用前置条件把 transfer 的所有守卫 if 化简成最后一个分支，
    然后区分自转账 / 非自转账两种。
-/

theorem prop_totalSupply_constant_on_transfer
  (ctx : Context) (s : State) (recipient : Address) (amount : UInt)
  (h_not_zero    : recipient ≠ zeroAddress)
  (h_balance     : amount ≤ s.balanceOf ctx.sender)
  (h_no_overflow : s.balanceOf recipient + amount ≥ s.balanceOf recipient) :
  match h_out : transfer ctx s recipient amount with
  | .ok out   => out.newState.totalSupply = s.totalSupply
  | .error _  => False :=
by
  classical

  ------------------------------------------------------------------
  -- 第一步：构造一个“规格引理” hspec
  --   ∃ out, transfer = .ok out ∧ out.newState.totalSupply = s.totalSupply
  ------------------------------------------------------------------
  have hspec :
    ∃ out : TransitionOutput Bool,
      transfer ctx s recipient amount = .ok out
      ∧ out.newState.totalSupply = s.totalSupply :=
  by
    -- 分自转账 / 非自转账
    by_cases hself : ctx.sender = recipient
    · -- 自转账：newState 就是 s
      have h_balance_self : amount ≤ s.balanceOf recipient := by
        simpa [hself] using h_balance
        -- 先用hself把 ctx.sender 替换成 recipient，再用 h_balance
      refine ⟨
        { newState   := s,
          events     := [ERC20Event.Transfer ctx.sender recipient amount],
          returnValue := true },
        ?_, ?_⟩
      · -- 在当前前提下，transfer 直接化简成 .ok {...}
        simp [transfer, h_not_zero, h_balance_self, h_no_overflow, hself]
      · -- newState = s ⇒ totalSupply 显然不变
        rfl
    · -- 非自转账：只更新 balanceOf
      refine ⟨
        { newState :=
            { s with
              balanceOf := fun a =>
                if a = ctx.sender then s.balanceOf a - amount
                else if a = recipient then s.balanceOf a + amount
                else s.balanceOf a },
          events     := [ERC20Event.Transfer ctx.sender recipient amount],
          returnValue := true },
        ?_, ?_⟩
      · -- 用三个前提关掉所有 error 分支，再用 ¬hself 选中正常转账分支
        simp [transfer, h_not_zero, h_balance, h_no_overflow, hself]
      · -- record 更新只改 balanceOf，不改 totalSupply（用前面 @[simp] 引理）
        simp

  -- 从 hspec 中拿到一个具体 out₀，以及：
  -- (1) h_ok : transfer ctx s recipient amount = .ok out₀
  -- (2) hTS  : out₀.newState.totalSupply = s.totalSupply
  rcases hspec with ⟨out₀, h_ok, hTS⟩

  ------------------------------------------------------------------
  -- 第二步：对 match 里的 scrutinee  `transfer ctx s recipient amount`
  -- 做分类；用 h_ok 排除 error 分支，在 ok 分支上把 out = out₀ 替换掉，
  -- 最后用 hTS 收尾。
  ------------------------------------------------------------------
  cases h_out : transfer ctx s recipient amount with
  | error e =>
      -- 此时目标是 False；但我们知道 transfer 实际上等于 .ok out₀，
      -- 和当前 h_out : transfer = .error e 矛盾
      have : (Except.error e : CallResult Bool) = Except.ok out₀ :=
        h_out.symm.trans h_ok
      cases this   -- error ≠ ok，矛盾，分支关掉

  | ok out =>
      -- 此时目标是 out.newState.totalSupply = s.totalSupply
      -- 利用 h_ok 和 h_out 推出 out = out₀
      have h_eq : out = out₀ := by
        -- 从两条等式：
        --   h_out : transfer = .ok out
        --   h_ok  : transfer = .ok out₀
        -- 得到  .ok out = .ok out₀
        have h1 : (Except.ok out : CallResult Bool) = Except.ok out₀ :=
          h_out.symm.trans h_ok
        -- 构造子注入性：.ok out = .ok out₀ ⇒ out = out₀
        cases h1
        rfl
      -- 用 out = out₀ 把目标里的 out 换成 out₀
      subst h_eq
      -- 现在目标就是 out₀.newState.totalSupply = s.totalSupply，刚好是 hTS
      exact hTS
