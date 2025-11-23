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
-- 2. 外部世界建模 (修复：增加 Token 定义)
-- ==============================================================================

structure MarketState where
  tokenIn    : Address -- [修复 1] 明确池子接收的 Token
  tokenOut   : Address
  reserveIn  : Amount
  reserveOut : Amount
  deriving Repr, DecidableEq

structure Context where
  sender : Address
  market : MarketState
  -- [修复 2] 引入 Oracle 信息以计算 Fair Value (解决维度问题)
  fairPrice      : Amount
  pricePrecision : Amount
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

-- [修复 2] 计算理论预期产出 (解决 WBTC -> WETH 精度问题)
-- 这是前端计算逻辑的一部分，用于生成 minLiquidity
def calc_expected_out (amountIn : Amount) (price : Amount) (prec : Amount) : Amount :=
  if prec == 0 then 0 else (amountIn * price) / prec

-- 修复后的 ZapIn 函数
def ZapInWithERC20_Dynamic (inToken : Address) (inAmount : Amount) (minLiquidity : Amount)
                           (ctx : Context) (s : State) : CallResult :=
  if s.stopped then
    CallResult.failure Error.ContractStopped
  else if inAmount = 0 then
    CallResult.failure Error.InsufficientInput
  else if s.allowances ctx.sender inToken < inAmount then
    CallResult.failure Error.InsufficientAllowance
  -- [修复 1] 校验传入的 token 是否匹配市场
  else if inToken ≠ ctx.market.tokenIn then
    CallResult.failure Error.InvalidTokenAddress
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

def construct_safe_min_liquidity (expectedOut : Amount) (toleranceBps : Amount) : Amount :=
  let numerator := expectedOut * (BPS_DENOMINATOR - toleranceBps)
  if numerator == 0 then 0 else
  (numerator + BPS_DENOMINATOR - 1) / BPS_DENOMINATOR

-- [保持原样] 证明 ceil(n/d)*d >= n
theorem ceil_div_mul_ge (n d : Nat) (h_d_pos : d > 0) :
  ((n + d - 1) / d) * d >= n :=
by
  match n with
  | 0 => simp
  | n' + 1 =>
    have h_lhs : ((n' + 1 + d - 1) / d) * d = (n' / d + 1) * d := by
      rw [Nat.add_comm n' 1, Nat.add_assoc, Nat.add_comm 1, Nat.add_sub_cancel]
      rw [Nat.add_div_right _ h_d_pos]
    rw [h_lhs]
    rw [Nat.add_mul, Nat.one_mul]
    conv => rhs; rw [← Nat.div_add_mod n' d]
    rw [Nat.mul_comm d (n' / d)]
    rw [Nat.add_assoc]
    apply Nat.add_le_add_left
    exact Nat.succ_le_of_lt (Nat.mod_lt _ h_d_pos)

-- [主定理] 鲁棒性证明
-- 修复：基于 expectedOut 计算 minLiq，并证明亏损有界
theorem slippage_mechanism_is_robust
  (inToken : Address)
  (amountIn : Amount)
  (toleranceBps : Amount)
  (market : MarketState)
  (s : State)
  -- 引入 Oracle 参数
  (fairPrice : Amount)
  (pricePrecision : Amount)
  (h_pos : amountIn > 0)
  (h_tol : toleranceBps <= BPS_DENOMINATOR)
  -- [前提]: Oracle 精度正常，Input Token 正确
  (h_prec_pos : pricePrecision > 0)
  (h_token_valid : inToken = market.tokenIn) :

  -- 1. 前端计算逻辑变更：先算 expectedOut
  let expectedOut := calc_expected_out amountIn fairPrice pricePrecision
  let minLiq := construct_safe_min_liquidity expectedOut toleranceBps

  let ctx : Context := { sender := 1, market := market, fairPrice := fairPrice, pricePrecision := pricePrecision }
  let result := ZapInWithERC20_Dynamic inToken amountIn minLiq ctx s

  match result with
  | CallResult.failure _ => True
  | CallResult.success _ =>
      let actualOut := get_amm_out market amountIn
      -- 证明目标：基于 ExpectedOut 的亏损 <= Tolerance
      calc_loss_bps expectedOut actualOut <= toleranceBps :=
by
  dsimp
  -- 展开所有定义
  dsimp [ZapInWithERC20_Dynamic, calc_loss_bps, construct_safe_min_liquidity, calc_expected_out]

  -- Guard 1: stopped
  by_cases hs : s.stopped = true
  · simp [hs]

  -- Guard 2: amount = 0
  by_cases hA0 : amountIn = 0
  · simp [hs, hA0]

  -- Guard 3: allowance
  by_cases hAllow : s.allowances 1 inToken < amountIn
  · simp [hs, hA0, hAllow]

  -- Guard 4 [新增]: Token 校验
  -- 注意：我们利用 h_token_valid 前提来证明 failure 分支是不可能的
  by_cases hToken : inToken ≠ market.tokenIn
  · -- 如果 inToken 不等于 market.tokenIn，这与 h_token_valid 矛盾
    have : inToken = market.tokenIn := h_token_valid
    contradiction

  -- 准备变量
  let actualOut := get_amm_out market amountIn
  let expectedOut := (amountIn * fairPrice) / pricePrecision
  let N := expectedOut * (BPS_DENOMINATOR - toleranceBps)

  -- Guard 5: actualOut < minLiq ?
  -- 这里的逻辑与之前完全一致，只是 amountIn 换成了 expectedOut
  by_cases hSlip :
      actualOut <
        (if (N == 0) = true then 0
         else (N + BPS_DENOMINATOR - 1) / BPS_DENOMINATOR)
  · ----------------------------------------------------------------
    -- pos 分支：触发滑点保护 -> True
    ----------------------------------------------------------------
    -- 利用 simpa 把复杂的 if 结构化简
    -- pos 分支
    have hSlip_pos' :
        get_amm_out market amountIn <
          (if expectedOut * (BPS_DENOMINATOR - toleranceBps) = 0 then 0
           else (expectedOut * (BPS_DENOMINATOR - toleranceBps)
                   + BPS_DENOMINATOR - 1) / BPS_DENOMINATOR) := by
      simpa [actualOut, N, expectedOut] using hSlip

    have hprec_ne0 : pricePrecision ≠ 0 := Nat.ne_of_gt h_prec_pos

    have hSlip_pos'' :
        get_amm_out market amountIn <
          (if
              (amountIn * fairPrice / pricePrecision) *
                  (BPS_DENOMINATOR - toleranceBps) = 0 then
            0
           else
            ((amountIn * fairPrice / pricePrecision) *
                    (BPS_DENOMINATOR - toleranceBps) +
                  BPS_DENOMINATOR - 1) / BPS_DENOMINATOR) := by
      simpa [expectedOut, hprec_ne0] using hSlip_pos'

    simp [hs, hA0, hAllow, hToken, hprec_ne0, hSlip_pos'']


  · ----------------------------------------------------------------
    -- neg 分支：交易成功
    ----------------------------------------------------------------
    have hSlip_neg' :
        ¬ get_amm_out market amountIn <
          (if expectedOut * (BPS_DENOMINATOR - toleranceBps) = 0 then 0
           else (expectedOut * (BPS_DENOMINATOR - toleranceBps)
                   + BPS_DENOMINATOR - 1) / BPS_DENOMINATOR) := by
      simpa [actualOut, N, expectedOut] using hSlip

    -- 这里的 simp 会进入 match 的 success 分支
    simp [hs, hA0, hAllow, hToken]

    ----------------------------------------------------------------
    -- 1) 提取核心不等式：actualOut >= minLiq
    ----------------------------------------------------------------
    have h_ge_min0 :
        actualOut ≥
          (if (N == 0) = true then 0
           else (N + BPS_DENOMINATOR - 1) / BPS_DENOMINATOR) :=
      Nat.le_of_not_lt hSlip

    ----------------------------------------------------------------
    -- 2) 证明 actualOut * denom >= N (利用 ceil 引理)
    ----------------------------------------------------------------
    have h_O_ge_N : actualOut * BPS_DENOMINATOR ≥ N := by
      by_cases hN0 : N = 0
      · rw [hN0]; exact Nat.zero_le _
      · by_cases hb : (N == 0) = true
        · have hb_iff : (N == 0) = true ↔ N = 0 := by simpa using (Nat.beq_eq_true N 0)
          exact False.elim (hN0 (hb_iff.mp hb))
        · have h_ge_ceil : actualOut ≥ (N + BPS_DENOMINATOR - 1) / BPS_DENOMINATOR := by
            simpa [hb] using h_ge_min0
          have h_ceil := ceil_div_mul_ge N BPS_DENOMINATOR (by decide)
          have h_mul := Nat.mul_le_mul_right BPS_DENOMINATOR h_ge_ceil
          exact Nat.le_trans h_ceil h_mul

    ----------------------------------------------------------------
    -- 3) 分情况讨论盈亏 (基于 expectedOut)
    ----------------------------------------------------------------

    -- 先把 pricePrecision ≠ 0 用出来，干掉 goal 里的 if(pricePrecision=0)
    have hprec_ne0 : pricePrecision ≠ 0 := Nat.ne_of_gt h_prec_pos

    -- 把 slippage 未触发假设改成「带 if(pricePrecision=0) 的同形版本」
    have hSlip_goal :
        ¬ get_amm_out market amountIn <
          (if
              (if pricePrecision = 0 then 0 else amountIn * fairPrice / pricePrecision) *
                (BPS_DENOMINATOR - toleranceBps) = 0 then
            0
           else
            ((if pricePrecision = 0 then 0 else amountIn * fairPrice / pricePrecision) *
                (BPS_DENOMINATOR - toleranceBps) +
              BPS_DENOMINATOR - 1) / BPS_DENOMINATOR) := by
      -- 用 hprec_ne0 把 if(pricePrecision=0) 化成 else，
      -- 然后 expectedOut 折回来
      simpa [expectedOut, hprec_ne0] using hSlip_neg'

    -- 现在 goal 的外层 if/match 与 hSlip_goal 同形，能被消掉
    simp [hprec_ne0]

        -- 现在 goal 只剩最后一个 slippage if/match，
    -- 需要一个与它“同形”的 ¬cond 来消掉它

    have hSlip_final :
        ¬ get_amm_out market amountIn <
          (if amountIn * fairPrice / pricePrecision *
                (BPS_DENOMINATOR - toleranceBps) = 0 then
              0
           else
              (amountIn * fairPrice / pricePrecision *
                    (BPS_DENOMINATOR - toleranceBps)
                 + BPS_DENOMINATOR - 1) / BPS_DENOMINATOR) := by
      -- hSlip_neg' 里是 expectedOut*(D-T)，把 expectedOut 展开即可
      simpa [expectedOut] using hSlip_neg'

    -- 给 cond 起个名字，强制 simp 用 ¬cond 把 ite/match 化到 success
    let cond : Prop :=
      get_amm_out market amountIn <
        (if amountIn * fairPrice / pricePrecision *
              (BPS_DENOMINATOR - toleranceBps) = 0 then
            0
         else
            (amountIn * fairPrice / pricePrecision *
                  (BPS_DENOMINATOR - toleranceBps)
               + BPS_DENOMINATOR - 1) / BPS_DENOMINATOR)

    have hcond : ¬ cond := by
      simpa [cond] using hSlip_final

    -- 这一句会把你现在看到的 match/if 整个消掉，直接进入 success 的 loss 目标
    simp [cond, hcond]


    -- 现在目标变成：
    -- ⊢ (if expectedOut ≤ actualOut then 0
    --      else (expectedOut - actualOut) * BPS_DENOMINATOR / expectedOut) ≤ toleranceBps
    let E : Nat := amountIn * fairPrice / pricePrecision
    let O : Nat := get_amm_out market amountIn

    -- 从你的 let 里对齐：expectedOut = E, actualOut = O
    have hE : expectedOut = E := by rfl
    have hO : actualOut = O := by rfl
    by_cases h_profit : O ≥ E
    · -- 盈利/无损分支
      have h_le_goal : E ≤ O := h_profit
      -- 先把 if 化成真支 -> 0
      simp [E, O, h_le_goal]

    · -- 亏损分支
      have h_not_le_goal : ¬ E ≤ O := by
        intro hle
        exact h_profit hle
      -- 先把 if 化成假支，露出 /E 的目标
      simp [E, O, h_not_le_goal]

      -- 现在目标是：
      -- ⊢ (E - O) * BPS_DENOMINATOR / E ≤ toleranceBps

      apply Nat.div_le_of_le_mul
      rw [Nat.mul_sub_right_distrib]
      apply Nat.sub_le_of_le_add

      -- 下面沿用你之前的链（把 expectedOut/actualOut 用 E/O 代入即可）
      apply Nat.le_trans (m := N + E * toleranceBps)
      · -- 证明 10000*E ≤ N + E*T
        -- N = expectedOut*(D-T)，而 expectedOut=E
        dsimp [N]
        -- 这里把 expectedOut 展成 E
        simp [hE, E]
        rw [Nat.mul_sub_left_distrib]
        apply Nat.le_add_of_sub_le
        exact Nat.le_refl _
      · -- 证明 N + E*T ≤ 10000*O + E*T
        -- h_O_ge_N : actualOut*D ≥ N  且 actualOut=O
        have h1 :
            N + E * toleranceBps ≤ O * BPS_DENOMINATOR + E * toleranceBps :=
          Nat.add_le_add_right (by simpa [hO, O] using h_O_ge_N) (E * toleranceBps)
        simpa [Nat.add_comm, Nat.add_left_comm, Nat.add_assoc, O] using h1

--- 公平交换定理 (复用结构)

theorem zap_in_enforces_payment
  (inToken : Address) (amountIn : Amount) (minLiquidity : Amount)
  (ctx : Context) (s : State) (out : TransitionOutput) :
  ZapInWithERC20_Dynamic inToken amountIn minLiquidity ctx s = CallResult.success out →
  ∃ act, act ∈ out.transfers ∧
         act.fromAddr = ctx.sender ∧
         act.token = inToken ∧
         act.amount = amountIn :=
by
  intro h_success
  dsimp [ZapInWithERC20_Dynamic] at h_success
  -- 依次拆解所有 Guard，包括新增的 Token Valid
  split at h_success <;> try contradiction
  split at h_success <;> try contradiction
  split at h_success <;> try contradiction
  split at h_success <;> try contradiction -- 新增的 Token Check
  split at h_success
  · contradiction -- Slippage Fail
  · injection h_success with h_eq
    rw [← h_eq]; simp


-- 授权扣减一致性 (复用结构)
theorem zap_in_reduces_allowance
  (inToken : Address) (amountIn : Amount) (minLiquidity : Amount)
  (ctx : Context) (s : State) (out : TransitionOutput) :
  ZapInWithERC20_Dynamic inToken amountIn minLiquidity ctx s = CallResult.success out →
  s.allowances ctx.sender inToken >= amountIn →
  out.newState.allowances ctx.sender inToken = s.allowances ctx.sender inToken - amountIn :=
by
  intro h_success h_allowance_enough
  dsimp [ZapInWithERC20_Dynamic] at h_success
  split at h_success <;> try contradiction
  split at h_success <;> try contradiction
  split at h_success <;> try contradiction
  split at h_success <;> try contradiction -- 新增的 Token Check
  split at h_success
  · contradiction
  · injection h_success with h_eq
    rw [← h_eq]; simp


-- 输出原子性 (复用结构)
theorem zap_in_guarantees_output_action
  (inToken : Address) (amountIn : Amount) (minLiquidity : Amount)
  (ctx : Context) (s : State) (out : TransitionOutput) :
  ZapInWithERC20_Dynamic inToken amountIn minLiquidity ctx s = CallResult.success out →
  let expectedOut := get_amm_out ctx.market amountIn
  ∃ act, act ∈ out.transfers ∧
         act.toAddr = ctx.sender ∧
         act.token = s.poolToken ∧
         act.amount = expectedOut :=
by
  intro h_success
  dsimp [ZapInWithERC20_Dynamic] at h_success
  split at h_success <;> try contradiction
  split at h_success <;> try contradiction
  split at h_success <;> try contradiction
  split at h_success <;> try contradiction -- 新增的 Token Check
  split at h_success
  · contradiction
  · injection h_success with h_eq
    rw [← h_eq]; simp
