import Lean
import Std

-- ==============================================================================
-- 1. 基础类型与错误定义
-- ==============================================================================

abbrev Address := Nat
abbrev Amount := Nat

def BPS_DENOMINATOR : Nat := 10000

inductive Error where
  | InsufficientAllowance
  | InsufficientInput
  | ContractStopped
  | SlippageToleranceExceeded
  | InvalidTokenAddress
  | Unauthorized
  deriving Repr, DecidableEq

inductive Event where
  | Approved (owner : Address) (spender : Address) (value : Amount)
  | ZapInExecuted (sender : Address) (inAmount : Amount) (outAmount : Amount)
  | ContractStatusChanged (isStopped : Bool)
  deriving Repr, DecidableEq

-- ==============================================================================
-- 2. 外部世界建模
-- ==============================================================================

structure MarketState where
  reserveIn : Amount
  reserveOut : Amount
  deriving Repr, DecidableEq

structure Context where
  sender : Address
  market : MarketState
  deriving Repr, DecidableEq

def get_amm_out (market : MarketState) (dx : Amount) : Amount :=
  let x := market.reserveIn
  let y := market.reserveOut
  if x + dx == 0 then 0 else
  (y * dx) / (x + dx)

-- ==============================================================================
-- 3. 合约状态与结构
-- ==============================================================================

structure State where
  stopped : Bool
  goodwill : Amount
  allowances : Address → Address → Amount
  poolToken : Address
  owner : Address

structure TransferAction where
  fromAddr : Address
  toAddr   : Address
  token    : Address
  amount   : Amount
  deriving Repr, DecidableEq

structure TransitionOutput where
  newState : State
  events   : List Event
  transfers : List TransferAction

inductive CallResult where
  | success (out : TransitionOutput)
  | failure (err : Error)

-- ==============================================================================
-- 4. 合约逻辑实现
-- ==============================================================================

def setStopped (newStatus : Bool) (ctx : Context) (s : State) : CallResult :=
  if ctx.sender ≠ s.owner then
    CallResult.failure Error.Unauthorized
  else
    let newState := { s with stopped := newStatus }
    CallResult.success { newState := newState, events := [Event.ContractStatusChanged newStatus], transfers := [] }

def approveToken (spender : Address) (amount : Amount) (ctx : Context) (s : State) : CallResult :=
  if spender = 0 then
    CallResult.failure Error.InvalidTokenAddress
  else
    let newAllowances := fun owner spdr =>
      if owner = ctx.sender ∧ spdr = spender then amount else s.allowances owner spdr
    CallResult.success { newState := { s with allowances := newAllowances }, events := [Event.Approved ctx.sender spender amount], transfers := [] }

def ZapInWithERC20_Dynamic (inToken : Address) (inAmount : Amount) (minLiquidity : Amount)
                           (ctx : Context) (s : State) : CallResult :=
  if s.stopped then
    CallResult.failure Error.ContractStopped
  else if inAmount = 0 then
    CallResult.failure Error.InsufficientInput
  else if s.allowances ctx.sender inToken < inAmount then
    CallResult.failure Error.InsufficientAllowance
  else
    let actualOut := get_amm_out ctx.market inAmount
    if actualOut < minLiquidity then
      CallResult.failure Error.SlippageToleranceExceeded
    else
      let newAllowances := fun owner spdr =>
        if owner = ctx.sender ∧ spdr = inToken then
          s.allowances owner spdr - inAmount
        else s.allowances owner spdr

      let newState := { s with allowances := newAllowances }

      let actionPull : TransferAction := { fromAddr := ctx.sender, toAddr := 999, token := inToken, amount := inAmount }
      let actionPush : TransferAction := { fromAddr := 999, toAddr := ctx.sender, token := s.poolToken, amount := actualOut }

      CallResult.success {
        newState  := newState,
        events    := [Event.ZapInExecuted ctx.sender inAmount actualOut],
        transfers := [actionPull, actionPush]
      }

-- ==============================================================================
-- 5. 形式化验证：完备性与鲁棒性定理
-- ==============================================================================

def calc_loss_bps (expected : Amount) (actual : Amount) : Amount :=
  if actual >= expected then 0
  else (expected - actual) * BPS_DENOMINATOR / expected

def construct_safe_min_liquidity (amountIn : Amount) (toleranceBps : Amount) : Amount :=
  let numerator := amountIn * (BPS_DENOMINATOR - toleranceBps)
  if numerator == 0 then 0 else
  (numerator + BPS_DENOMINATOR - 1) / BPS_DENOMINATOR

-- [修复版引理]: 证明 ceil(n/d)*d >= n
theorem ceil_div_mul_ge (n d : Nat) (h_d_pos : d > 0) :
  ((n + d - 1) / d) * d >= n :=
by
  match n with
  | 0 => simp
  | n' + 1 =>
    -- 1. 简化左边 (LHS)
    have h_lhs : ((n' + 1 + d - 1) / d) * d = (n' / d + 1) * d := by
      rw [Nat.add_comm n' 1, Nat.add_assoc, Nat.add_comm 1, Nat.add_sub_cancel]
      rw [Nat.add_div_right _ h_d_pos]
    rw [h_lhs]

    -- 2. 展开左边乘法
    rw [Nat.add_mul, Nat.one_mul]

    -- 3. 聚焦右边 (RHS)，利用带余除法重写 n'
    -- 使用 conv 策略只在 RHS 进行重写，避免干扰 LHS
    conv => rhs; rw [← Nat.div_add_mod n' d]

    -- 4. 此时目标: n'/d * d + d >= n'%d + d*(n'/d) + 1
    -- 利用乘法交换律对齐项: d*(n'/d) = n'/d * d
    rw [Nat.mul_comm d (n' / d)]

    -- 5. 消去两边相同的项 (d * (n'/d))
    rw [Nat.add_assoc]
    apply Nat.add_le_add_left

    -- 6. 剩余目标: d >= n'%d + 1
    -- 这等价于 n'%d < d，根据取模性质恒成立
    exact Nat.succ_le_of_lt (Nat.mod_lt _ h_d_pos)

theorem slippage_mechanism_is_robust
  (inToken : Address)
  (amountIn : Amount)
  (toleranceBps : Amount)
  (market : MarketState)
  (s : State)
  (h_pos : amountIn > 0)
  (h_tol : toleranceBps <= BPS_DENOMINATOR) :

  let minLiq := construct_safe_min_liquidity amountIn toleranceBps
  let ctx : Context := { sender := 1, market := market }
  let result := ZapInWithERC20_Dynamic inToken amountIn minLiq ctx s

  match result with
  | CallResult.failure _ => True
  | CallResult.success _ =>
      let actualOut := get_amm_out market amountIn
      calc_loss_bps amountIn actualOut <= toleranceBps :=
by
  dsimp
  -- 展开所有定义
  dsimp [ZapInWithERC20_Dynamic, calc_loss_bps, construct_safe_min_liquidity]

  by_cases hs : s.stopped = true
  · simp [hs]

  by_cases hA0 : amountIn = 0
  · simp [hs, hA0]

  by_cases hAllow : s.allowances 1 inToken < amountIn
  · simp [hs, hA0, hAllow]

  let actualOut := get_amm_out market amountIn
  let N := amountIn * (BPS_DENOMINATOR - toleranceBps)

  -- Guard 4: actualOut < minLiq ?
  by_cases hSlip :
      actualOut <
        (if (N == 0) = true then 0
         else (N + BPS_DENOMINATOR - 1) / BPS_DENOMINATOR)
  · ----------------------------------------------------------------
    -- pos 分支：触发滑点保护，应当化简到 True
    ----------------------------------------------------------------
    have hSlip_pos' :
        get_amm_out market amountIn <
          (if amountIn * (BPS_DENOMINATOR - toleranceBps) = 0 then 0
           else (amountIn * (BPS_DENOMINATOR - toleranceBps)
                   + BPS_DENOMINATOR - 1) / BPS_DENOMINATOR) := by
      -- 把 let 形式 actualOut/N 展回原表达式 + 把 (==0)=true 变成 =0
      simpa [actualOut, N] using hSlip

    -- 现在目标里的最后一个 if 就能被 simp 选中并化到 failure -> True
    simp [hs, hA0, hAllow, hSlip_pos']

  · ----------------------------------------------------------------
    -- neg 分支：不触发滑点保护，应当化简到 success 的 loss 目标
    ----------------------------------------------------------------
    have hSlip_neg' :
        ¬ get_amm_out market amountIn <
          (if amountIn * (BPS_DENOMINATOR - toleranceBps) = 0 then 0
           else (amountIn * (BPS_DENOMINATOR - toleranceBps)
                   + BPS_DENOMINATOR - 1) / BPS_DENOMINATOR) := by
      simpa [actualOut, N] using hSlip

    -- 用 hSlip_neg' 把 if/match 全部化简掉，得到你想要的 loss 不等式目标
    simp [hs, hA0, hAllow, hSlip_neg']

    ----------------------------------------------------------------
    -- 1) 由 guard4 的否定得到 actualOut ≥ minLiq（beq 形状）
    ----------------------------------------------------------------
    have h_ge_min0 :
        actualOut ≥
          (if (N == 0) = true then 0
           else (N + BPS_DENOMINATOR - 1) / BPS_DENOMINATOR) :=
      Nat.le_of_not_lt hSlip

        ----------------------------------------------------------------
    -- 2) 关键中间结论：actualOut * denom ≥ N
    ----------------------------------------------------------------
    have h_O_ge_N : actualOut * BPS_DENOMINATOR ≥ N := by
      by_cases hN0 : N = 0
      · rw [hN0]
        exact Nat.zero_le _
      · -- N ≠ 0
        by_cases hb : (N == 0) = true
        · -- hb -> N = 0，与 hN0 矛盾
          have hb_iff : (N == 0) = true ↔ N = 0 := by
            simpa using (Nat.beq_eq_true N 0)
          have : N = 0 := (hb_iff.mp hb)
          exact False.elim (hN0 this)
        · -- hb : (N==0)=false，于是 minLiq 退化成 ceil(...)
          have h_ge_ceil :
              actualOut ≥ (N + BPS_DENOMINATOR - 1) / BPS_DENOMINATOR := by
            simpa [hb] using h_ge_min0

          have h_ceil :=
            ceil_div_mul_ge N BPS_DENOMINATOR (by decide)

          have h_mul :
              ((N + BPS_DENOMINATOR - 1) / BPS_DENOMINATOR) * BPS_DENOMINATOR
                ≤ actualOut * BPS_DENOMINATOR :=
            Nat.mul_le_mul_right BPS_DENOMINATOR h_ge_ceil

          exact Nat.le_trans h_ceil h_mul
    ----------------------------------------------------------------
    -- 3) 拆 calc_loss_bps 的 if（只用 by_cases）
    ----------------------------------------------------------------
    by_cases h_profit : actualOut ≥ amountIn
    · -- 盈利分支，loss = 0 ≤ toleranceBps 恒成立
      have h_le : amountIn ≤ actualOut := h_profit
      -- if amountIn ≤ actualOut then 0 else ...  会被化简成 0
      simp [actualOut, h_le]
    · -- 亏损分支，loss = ((amountIn - actualOut)*denom)/amountIn
      have h_not_le : ¬ amountIn ≤ actualOut := by
        intro hle
        exact h_profit hle
      -- if 会被化简成 else 分支，得到亏损公式目标
      simp [actualOut, h_not_le]
       -- ===== 下面基本保持你原来的数学链 =====
      apply Nat.div_le_of_le_mul
      rw [Nat.mul_sub_right_distrib]
      apply Nat.sub_le_of_le_add

      apply Nat.le_trans (m := N + amountIn * toleranceBps)
      · -- denom*amountIn ≤ N + amountIn*toleranceBps
        dsimp [N]
        rw [Nat.mul_sub_left_distrib]
        apply Nat.le_add_of_sub_le
        exact Nat.le_refl _
      · -- 证明 N + A*T <= A*T + 10000*O
        have h1 :
            N + amountIn * toleranceBps
              ≤ actualOut * BPS_DENOMINATOR + amountIn * toleranceBps :=
          Nat.add_le_add_right h_O_ge_N (amountIn * toleranceBps)

        -- 交换 RHS 的加法顺序，并把 actualOut 展回 get_amm_out ...
        simpa [Nat.add_comm, Nat.add_left_comm, Nat.add_assoc, actualOut] using h1

/--
  定理：公平交换 (Fair Exchange)
  如果 ZapIn 调用成功，那么生成的动作列表中，必须包含一笔
  "从调用者 (sender) 转出 inAmount 到合约" 的扣款动作。
-/
theorem zap_in_enforces_payment
  (inToken : Address) (amountIn : Amount) (minLiquidity : Amount)
  (ctx : Context) (s : State) (out : TransitionOutput) :
  -- 前提: 调用成功
  ZapInWithERC20_Dynamic inToken amountIn minLiquidity ctx s = CallResult.success out →
  -- 结论: 存在一笔 Pull 动作，且发送方是用户，金额是 amountIn
  ∃ act, act ∈ out.transfers ∧
         act.fromAddr = ctx.sender ∧
         act.token = inToken ∧
         act.amount = amountIn :=
by
  intro h_success
  -- 1. 展开函数定义
  dsimp [ZapInWithERC20_Dynamic] at h_success

  -- 2. 拆解 Guard 条件
  -- 如果 Guard 失败，结果是 failure，与 h_success (success) 矛盾，自动排除
  split at h_success <;> try contradiction
  split at h_success <;> try contradiction
  split at h_success <;> try contradiction

  -- 3. 拆解滑点检查
  split at h_success
  · -- Case: Failure (滑点过高) -> 矛盾
    contradiction
  · -- Case: Success
    -- 此时 h_success 等式成立，out 等于我们构造的结构体
    -- 我们通过 injection 提取出 transfers 字段
    injection h_success with h_eq
    rw [← h_eq]
    simp

/--
  定理：授权扣减一致性 (Allowance Deduction)
  如果交易成功，新状态中用户的 allowance 必须等于 旧 allowance 减去 输入金额。
-/
theorem zap_in_reduces_allowance
  (inToken : Address) (amountIn : Amount) (minLiquidity : Amount)
  (ctx : Context) (s : State) (out : TransitionOutput) :
  -- 前提 1: 调用成功
  ZapInWithERC20_Dynamic inToken amountIn minLiquidity ctx s = CallResult.success out →
  -- 前提 2: 初始授权足够 (虽然由成功隐含，但作为数学前提列出更严谨)
  s.allowances ctx.sender inToken >= amountIn →
  -- 结论: allowance 减少
  out.newState.allowances ctx.sender inToken = s.allowances ctx.sender inToken - amountIn :=
by
  intro h_success h_allowance_enough
  dsimp [ZapInWithERC20_Dynamic] at h_success

  -- 拆解 Guards
  split at h_success <;> try contradiction
  split at h_success <;> try contradiction
  split at h_success <;> try contradiction

  -- 拆解滑点检查
  split at h_success
  · contradiction
  · -- Case: Success
    injection h_success with h_eq
    -- 我们关注的是 newState 字段
    -- h_eq 告诉我们 out = { newState := ..., ... }
    -- 所以 out.newState 就是代码中构造的那个 record
    rw [← h_eq]
    simp

/--
  定理：输出原子性 (Output Atomicity)
  交易成功意味着必然有一笔 Push 动作，将计算出的市场产出转账给用户。
-/
theorem zap_in_guarantees_output_action
  (inToken : Address) (amountIn : Amount) (minLiquidity : Amount)
  (ctx : Context) (s : State) (out : TransitionOutput) :
  ZapInWithERC20_Dynamic inToken amountIn minLiquidity ctx s = CallResult.success out →
  -- 结论: 存在一笔 Push 动作
  let expectedOut := get_amm_out ctx.market amountIn
  ∃ act, act ∈ out.transfers ∧
         act.toAddr = ctx.sender ∧
         act.token = s.poolToken ∧
         act.amount = expectedOut :=
by
  intro h_success
  dsimp [ZapInWithERC20_Dynamic] at h_success

  -- 排除 Guards
  split at h_success <;> try contradiction
  split at h_success <;> try contradiction
  split at h_success <;> try contradiction

  -- 排除滑点失败
  split at h_success
  · contradiction
  · -- Case Success
    injection h_success with h_eq
    rw [← h_eq]
    simp

