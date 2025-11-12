import Init.Data.Nat.Basic
import Init.Data.Nat.Lemmas

-- ======== 1. 基础定义 ========

structure Address where
  val : String
deriving DecidableEq, Inhabited, Repr

-- 只保留这一个实例：与命题相等对齐
instance : BEq Address where
  beq a b := decide (a = b)

namespace Address
@[simp] theorem beq_eq_decide (a b : Address) :
  (a == b) = decide (a = b) := rfl

@[simp] theorem beq_true_iff_eq {a b : Address} :
  (a == b) = true ↔ a = b := by
  by_cases h : a = b <;> simp [beq_eq_decide, h]

@[simp] theorem beq_false_iff_ne {a b : Address} :
  (a == b) = false ↔ a ≠ b := by
  by_cases h : a = b <;> simp [beq_eq_decide, h]
end Address

def zeroAddress : Address := ⟨""⟩
abbrev UInt := Nat

-- ======== 2. 状态、事件与错误 ========

structure State where
  totalSupply : UInt
  balanceOf : Address → UInt
  allowance : Address → Address → UInt
deriving Inhabited

structure Context where
  sender : Address

inductive ERC20Event where
  | Transfer (transferSource : Address) (recipient : Address) (amount : UInt)
  | Approval (owner : Address) (spender : Address) (amount : UInt)
deriving DecidableEq, Inhabited

inductive ERC20Error where
  | InsufficientBalance
  | InsufficientAllowance
  | InvalidRecipientZeroAddress
  | Overflow
deriving DecidableEq, Inhabited

structure TransitionOutput (α : Type) where
  newState : State
  events : List ERC20Event
  returnValue : α

abbrev CallResult (α : Type) := Except ERC20Error (TransitionOutput α)

def constructor (initialHolder : Address) (initialSupply : UInt) : State :=
  {
    totalSupply := initialSupply,
    balanceOf := fun a => if a == initialHolder then initialSupply else 0,
    allowance := fun _ _ => 0
  }

-- ======== 5. 核心函数 ========

def transfer (ctx : Context) (s_old : State) (recipient : Address) (amount : UInt)
  : Except ERC20Error (TransitionOutput Bool) := do
  if recipient == zeroAddress then
    throw ERC20Error.InvalidRecipientZeroAddress
  if s_old.balanceOf ctx.sender < amount then
    throw ERC20Error.InsufficientBalance
  if s_old.balanceOf recipient + amount < s_old.balanceOf recipient then
    throw ERC20Error.Overflow
  if ctx.sender == recipient then
    return { newState := s_old,
             events := [ERC20Event.Transfer ctx.sender recipient amount],
             returnValue := true }
  let s_new := { s_old with
    balanceOf := fun a =>
      if a == ctx.sender then s_old.balanceOf a - amount
      else if a == recipient then s_old.balanceOf a + amount
      else s_old.balanceOf a }
  return { newState := s_new,
           events := [ERC20Event.Transfer ctx.sender recipient amount],
           returnValue := true }

def approve (ctx : Context) (s_old : State) (spender : Address) (amount : UInt)
  : Except ERC20Error (TransitionOutput Bool) := do
  let s_new := { s_old with
    allowance := fun owner sp =>
      if owner == ctx.sender ∧ sp == spender then amount
      else s_old.allowance owner sp }
  return { newState := s_new,
           events := [ERC20Event.Approval ctx.sender spender amount],
           returnValue := true }

def transferFrom (ctx : Context) (s_old : State) (src : Address) (recipient : Address) (amount : UInt)
  : Except ERC20Error (TransitionOutput Bool) := do
  if recipient == zeroAddress then
    throw ERC20Error.InvalidRecipientZeroAddress
  if s_old.balanceOf src < amount then
    throw ERC20Error.InsufficientBalance
  if s_old.allowance src ctx.sender < amount then
    throw ERC20Error.InsufficientAllowance
  if s_old.balanceOf recipient + amount < s_old.balanceOf recipient then
    throw ERC20Error.Overflow
  let s_mid := { s_old with
    allowance := fun owner sp =>
      if owner == src ∧ sp == ctx.sender then s_old.allowance owner sp - amount
      else s_old.allowance owner sp }
  if src == recipient then
    return { newState := s_mid,
             events := [ERC20Event.Transfer src recipient amount],
             returnValue := true }
  let s_new := { s_mid with
    balanceOf := fun a =>
      if a == src then s_mid.balanceOf a - amount
      else if a == recipient then s_mid.balanceOf a + amount
      else s_mid.balanceOf a }
  return { newState := s_new,
           events := [ERC20Event.Transfer src recipient amount],
           returnValue := true }

-- ======== 性质 1：transfer 不改 totalSupply ========

theorem prop1_totalSupply_constant_on_transfer
  (ctx : Context) (s_old : State) (recipient : Address) (amount : UInt)
  (h_not_zero : recipient ≠ zeroAddress)
  (h_balance : s_old.balanceOf ctx.sender ≥ amount) :
  match h_out : transfer ctx s_old recipient amount with
  | .ok out   => out.newState.totalSupply = s_old.totalSupply
  | .error _  => False := by
  -- 守卫 2：余额足够（把 ≥ 当作 ≤ 用）
  have hb : ¬ s_old.balanceOf ctx.sender < amount :=
    Nat.not_lt_of_le (show amount ≤ s_old.balanceOf ctx.sender from h_balance)
  -- 守卫 3：在 ℕ 上恒成立（不回绕）
  have ho : ¬ s_old.balanceOf recipient + amount < s_old.balanceOf recipient :=
    Nat.not_lt_of_le (Nat.le_add_right _ _)
  -- 守卫 1（布尔版）：收款人不是 0 地址
  have hz : (recipient == zeroAddress) = false := by
    simpa [Address.beq_false_iff_ne] using h_not_zero

  -- 分自转账 / 非自转账
  by_cases hsr : ctx.sender = recipient
  · ----------------------------------------------------------------
    -- 自转账：直接返回 s_old
    -- 布尔版“自转账为真”
    have hsr_true : (ctx.sender == recipient) = true := by
      simpa [Address.beq_true_iff_eq, hsr]

    -- 先固定期望输出，避免 {…} 的类型推断失败
    let out₀ : TransitionOutput Bool :=
      { newState := s_old,
        events := [ERC20Event.Transfer ctx.sender recipient amount],
        returnValue := true }

    -- 自转账分支里的 hcall 只改这几行
    have hcall : transfer ctx s_old recipient amount = Except.ok out₀ := by
      dsimp [transfer]
      -- 三个守卫的命题证据
      have hz0 : recipient ≠ zeroAddress := h_not_zero
      have hb' : ¬ s_old.balanceOf ctx.sender < amount := hb
      have ho' : ¬ s_old.balanceOf recipient + amount < s_old.balanceOf recipient := ho
      -- 依次关掉三层 if，再走自转账 then 分支；把 do/bind 一并化简
      simpa [ if_neg hz0, if_neg hb', if_neg ho', if_pos hsr, out₀,
              Bind.bind, Except.bind, Except.pure, pure ]


    -- 用 hcall 消掉目标里的 match
    cases htr : transfer ctx s_old recipient amount with
    | error e =>
        -- 得到 .error = .ok，矛盾
        have hc := hcall
        simp [htr] at hc

    | ok out =>
      have hc := hcall
      simp [htr] at hc
      cases hc
      -- 把目标从 `match h_out : Except.ok out₀ with …` 改成等价的简单式
      change out₀.newState.totalSupply = s_old.totalSupply
      -- 展开 out₀（自转账时 newState = s_old），目标即 rfl
      simp [out₀]


  · ----------------------------------------------------------------
    -- 非自转账：只改 balanceOf
    -- 布尔版“自转账为假”
    have hsr_false : (ctx.sender == recipient) = false := by
      simpa [Address.beq_false_iff_ne] using hsr

    let out₁ : TransitionOutput Bool :=
      { newState :=
          { s_old with
            balanceOf := fun a =>
              if a = ctx.sender then s_old.balanceOf a - amount
              else if a = recipient then s_old.balanceOf a + amount
              else s_old.balanceOf a },
        events := [ERC20Event.Transfer ctx.sender recipient amount],
        returnValue := true }

    have hcall : transfer ctx s_old recipient amount = Except.ok out₁ := by
      dsimp [transfer]
      simp [  hb, ho,    -- 三道守卫 + 非自转账（布尔）
             Bind.bind, Except.bind, Except.pure, pure ]
      simpa [if_neg h_not_zero, if_neg hsr, out₁]

    cases htr : transfer ctx s_old recipient amount with
    | error e =>
        -- .error = .ok，矛盾
        have hc := hcall
        simp [htr] at hc

    | ok out =>
        -- .ok out = .ok out₁ ⇒ 替换 out，收尾
        have hc := hcall
        simp [htr] at hc
        cases hc
        -- totalSupply 字段未改
        simpa [out₁]

theorem prop1_totalSupply_constant_on_approve
  (ctx : Context) (s_old : State) (spender : Address) (amount : UInt) :
  match h_out : approve ctx s_old spender amount with
  | .ok out   => out.newState.totalSupply = s_old.totalSupply
  | .error _  => False := by

  -- 1. 定义期望的输出
  let out_approve : TransitionOutput Bool :=
    { newState :=
        { s_old with
          allowance := fun owner sp =>
            if owner = ctx.sender ∧ sp = spender then amount
            else s_old.allowance owner sp },
      events := [ERC20Event.Approval ctx.sender spender amount],
      returnValue := true }

  -- 2. 证明 hcall：approve 函数总是返回这个 out_approve
  have hcall : approve ctx s_old spender amount = Except.ok out_approve := by
    dsimp [approve]
    -- 'do' 块只有一个 'return' 语句，它被翻译为 'pure'
    -- 'pure' 在 Except monad 中就是 'Except.ok'
    -- 'simpa' 会展开 'out_approve' 并确认它们是相同的 (rfl)
    simpa [pure, Except.pure, out_approve]

  -- 3. 使用 hcall 解开 match
  cases htr : approve ctx s_old spender amount with
  | error e =>
      -- 证明 .error = .ok 导致矛盾
      have hc := hcall
      simp [htr] at hc

  | ok out =>
      -- 证明 .ok out = .ok out_approve
      have hc := hcall
      simp [htr] at hc
      -- 'hc' 变为 'out = out_approve'，'cases' 将 'out' 替换为 'out_approve'
      cases hc
      -- 目标变为 'out_approve.newState.totalSupply = s_old.totalSupply'
      -- 展开 'out_approve'
      simp [out_approve]
      -- 目标变为 '{ s_old with allowance := ... }.totalSupply = s_old.totalSupply'
      -- 'simp' 发现 'totalSupply' 字段未被修改，证明完成。

-- ======== 性质 3：余额转移正确性 ========

theorem prop3_transfer_correctness
  (ctx : Context) (s_old : State) (recipient : Address) (amount : UInt)
  -- 假设为非自转账
  (h_not_self : ctx.sender ≠ recipient)
  -- 假设所有守卫都通过
  (h_not_zero : recipient ≠ zeroAddress)
  (h_balance : s_old.balanceOf ctx.sender ≥ amount):

  -- 目标：证明 'transfer' 返回了 *精确* 的 'ok' 结果
  let s_new := { s_old with
    balanceOf := fun a =>
      if a == ctx.sender then s_old.balanceOf a - amount
      else if a == recipient then s_old.balanceOf a + amount
      else s_old.balanceOf a
  }
  transfer ctx s_old recipient amount = .ok {
    newState := s_new,
    events := [ERC20Event.Transfer ctx.sender recipient amount],
    returnValue := true
  }
  := by
    -- 1. 准备 Nat 相关的守卫 (Prop 形式)
    --    (Address 相关的守卫 h_not_zero 和 h_not_self 我们直接使用)
    have hg2_prop : ¬ s_old.balanceOf ctx.sender < amount :=
      Nat.not_lt_of_le h_balance
    have hg3_prop : ¬ s_old.balanceOf recipient + amount < s_old.balanceOf recipient :=
      Nat.not_lt_of_le (Nat.le_add_right _ _)

    -- 2. 展开 LHS (transfer) 和 RHS (let s_new)
    dsimp (config := { zeta := true }) [transfer]

    -- 3. 'simp'
    --    - 'Bind.bind' etc. 展开 'do' 块
    --    - 'Address.beq_false_iff_ne' 等 @[simp] 引理会
    --      自动将 'if recipient == zeroAddress' 转为 'if recipient = zeroAddress'
    --    - 'h_not_zero', 'h_not_self': 'simp' 现在可以直接使用这些 Prop 假设
    --      来 "剪除" 'if recipient = zeroAddress' 分支
    --    - 'hg2_prop', 'hg3_prop': 'simp' 需要知道如何使用 '¬ P' (Prop)
    --      来 "剪除" 'if P then ...' (这里的 'P' 是一个 'decidable' 命题)
    --    - 'if_neg': 这个引理就是 'simp' 需要的桥梁
    simp [ Bind.bind, Except.bind, Except.pure, pure,
           h_not_zero,
           h_not_self,
           hg2_prop,
           hg3_prop ]


-- ======== 性质 5：自转账特殊处理 ========

theorem prop5_self_transfer_noop
  (ctx : Context) (s_old : State) (amount : UInt)
  -- 假设前提条件满足
  (h_not_zero : ctx.sender ≠ zeroAddress)
  (h_balance : s_old.balanceOf ctx.sender ≥ amount)
  (h_no_overflow : s_old.balanceOf ctx.sender + amount ≥ s_old.balanceOf ctx.sender) :

  -- 目标：证明 'transfer' 返回了 'ok s_old' (状态未变)
  transfer ctx s_old ctx.sender amount = .ok {
    newState := s_old, -- 状态不变
    events := [ERC20Event.Transfer ctx.sender ctx.sender amount],
    returnValue := true
  }
  := by
    -- 1. 准备 Nat 相关的守卫 (Prop 形式)
    have hg2_prop : ¬ s_old.balanceOf ctx.sender < amount :=
      Nat.not_lt_of_le h_balance
    -- 注意: 这里的 'recipient' 是 'ctx.sender'
    have hg3_prop : ¬ s_old.balanceOf ctx.sender + amount < s_old.balanceOf ctx.sender :=
      Nat.not_lt_of_le h_no_overflow

    -- 2. 准备 "action" (自转账) 的 Prop 假设
    --    'simp' 会自动使用 'rfl' (自反性) 来证明这一点
    have h_self : ctx.sender = ctx.sender := rfl

    -- 3. 展开 LHS (transfer)
    dsimp [transfer]

    -- 4. 'simp'
    --    - 'h_not_zero', 'hg2_prop', 'hg3_prop' 用于 'if_neg'
    --    - 'h_self' 用于 'if_pos'
    --    - 'Address' 命名空间中的 @[simp] 引理会自动启用
    simp [ Bind.bind, Except.bind, Except.pure, pure,
           h_not_zero,
           hg2_prop,
           hg3_prop,]

-- ======== 性质 6：授权机制一致性 ========

-- "(a) ...不足则交易失败"
theorem prop6a_transferFrom_reverts_on_insufficient_allowance
  (ctx : Context) (s_old : State) (src : Address) (recipient : Address) (amount : UInt)
  -- 假设前两个守卫通过
  (h_not_zero : recipient ≠ zeroAddress)
  (h_balance : s_old.balanceOf src ≥ amount)
  -- 假设津贴不足 (第3个守卫失败)
  (h_allowance : s_old.allowance src ctx.sender < amount) :

  transferFrom ctx s_old src recipient amount = .error ERC20Error.InsufficientAllowance
  := by
    -- 1. 准备 Nat 相关的守卫 (Prop 形式)
    have hg2_prop : ¬ s_old.balanceOf src < amount :=
      Nat.not_lt_of_le h_balance

    -- 2. 展开 'transferFrom'
    dsimp [transferFrom]

    -- 3. 'simp'
    --    - 'h_not_zero' 和 'hg2_prop' 用于 'if_neg' (通过守卫)
    --    - 'h_allowance' 用于 'if_pos' (触发 'throw')
    simp [ Bind.bind, Except.bind, Except.pure, pure,
           h_not_zero,
           hg2_prop,
           h_allowance ]


-- "(b) ...自动扣减...许可额度"
theorem prop6b_transferFrom_deducts_allowance
  (ctx : Context) (s_old : State) (src : Address) (recipient : Address) (amount : UInt)
  -- 假设所有守卫都通过
  (h_not_zero : recipient ≠ zeroAddress)
  (h_balance : s_old.balanceOf src ≥ amount)
  (h_allowance : s_old.allowance src ctx.sender ≥ amount)
  (h_no_overflow : s_old.balanceOf recipient + amount ≥ s_old.balanceOf recipient) :

  match h_out : transferFrom ctx s_old src recipient amount with
  | .ok out => out.newState.allowance src ctx.sender = s_old.allowance src ctx.sender - amount
  | .error _ => False
  := by
    -- 策略: 与 prop1_totalSupply_constant_on_transferFrom *完全相同*

    -- 1. 准备 Nat 相关的 Prop 守卫 (Address 相关的我们直接用)
    have hg2 : ¬ s_old.balanceOf src < amount :=
      Nat.not_lt_of_le h_balance
    have hg3 : ¬ s_old.allowance src ctx.sender < amount :=
      Nat.not_lt_of_le h_allowance
    have hg4 : ¬ s_old.balanceOf recipient + amount < s_old.balanceOf recipient :=
      Nat.not_lt_of_le (Nat.le_add_right _ _)

    -- 2. 对核心逻辑进行 'by_cases' (自转账 vs 非自转账)
    by_cases hsr : src = recipient
    · ----------------------------------------------------------------
      -- 自转账 (src = recipient)

      -- 3a. 定义期望的输出 (out₀)
      let s_mid := { s_old with
        allowance := fun owner sp =>
          if owner = src ∧ sp = ctx.sender then s_old.allowance owner sp - amount
          else s_old.allowance owner sp }
      let out₀ : TransitionOutput Bool :=
        { newState := s_mid,
          events := [ERC20Event.Transfer src recipient amount],
          returnValue := true }

      -- 4a. 证明 hcall
      have hcall : transferFrom ctx s_old src recipient amount = Except.ok out₀ := by
        dsimp [transferFrom]
        -- [修复]
        -- 喂给 'simp' *Prop* 假设
        -- 'if_neg' 用于 4 个守卫 (hg2, hg3, hg4, h_not_zero)
        -- 'if_pos' 用于 1 个 action (hsr)
        simp [ Bind.bind, Except.bind, Except.pure, pure,
               h_not_zero, hg2, hg3, hg4]           -- 展开定义
        simp [if_pos hsr, out₀, s_mid]               -- 自转账分支
      -- 5a. 使用 hcall 解开 match
      cases htr : transferFrom ctx s_old src recipient amount with
      | error e => have hc := hcall; simp [htr] at hc
      | ok out =>
          have hc := hcall; simp [htr] at hc; cases hc
          -- 目标: out₀.newState.allowance ... = ...
          simp [out₀, s_mid]

    · ----------------------------------------------------------------
      -- 非自转账 (src ≠ recipient)

      -- 3b. 定义期望的"中间"和"最终"状态
      let s_mid := { s_old with
        allowance := fun owner sp =>
          if owner = src ∧ sp = ctx.sender then s_old.allowance owner sp - amount
          else s_old.allowance owner sp }
      let s_new := { s_mid with
        balanceOf := fun a =>
          if a = src then s_mid.balanceOf a - amount
          else if a = recipient then s_mid.balanceOf a + amount
          else s_mid.balanceOf a }
      let out₁ : TransitionOutput Bool :=
        { newState := s_new,
          events := [ERC20Event.Transfer src recipient amount],
          returnValue := true }

      -- 4b. 证明 hcall
      have hcall : transferFrom ctx s_old src recipient amount = Except.ok out₁ := by
        dsimp [transferFrom]
        -- [修复]
        -- 'if_neg' 用于所有 5 个 'if' (4个守卫 + 1个 'hsr')
        simp [ Bind.bind, Except.bind, Except.pure, pure,
               if_neg,
               h_not_zero, hg2, hg3, hg4, -- 4 守卫
               hsr,                       -- 1 action (hsr 是 '¬ src = recipient')
               out₁, s_mid, s_new ]       -- 展开定义

      -- 5b. 使用 hcall 解开 match
      cases htr : transferFrom ctx s_old src recipient amount with
      | error e => have hc := hcall; simp [htr] at hc
      | ok out =>
          have hc := hcall; simp [htr] at hc; cases hc
          -- 目标: out₁.newState.allowance ... = ...
          simp [out₁, s_new, s_mid]
