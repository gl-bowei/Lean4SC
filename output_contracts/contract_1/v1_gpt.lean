import Init.Data.Nat.Basic
import Init.Data.Nat.Lemmas
import Std  -- 新增：用于 Std.Format

namespace ZapInFSM

-- =============================================================================
-- 0. 基础抽象类型
-- =============================================================================

abbrev Address := Nat
abbrev Amount  := Nat

/-- 合约自身地址（抽象） -/
abbrev ContractAddr : Address := 0

/-- 输入代币类型：ETH 或 ERC20(地址) -/
inductive Token where
  | eth
  | erc20 (addr : Address)
  deriving Repr, DecidableEq, Inhabited

/-- 交易路径/路由（抽象为有限类型） -/
abbrev Route := Nat

-- =============================================================================
-- 1. FSM 的“阶段状态” S 以及初始状态 s₀
-- =============================================================================

inductive Phase where
  | ActiveIdle
  | Paused
  | ZapRequested
  | Swapped
  | LiquidityAdded
  deriving Repr, DecidableEq, Inhabited

-- =============================================================================
-- 2. Error / Event / Context
-- =============================================================================

inductive Error where
  | InsufficientAllowance
  | InsufficientInput
  | ContractStopped
  | SlippageToleranceExceeded
  | InvalidTokenAddress
  | Unauthorized
  | InvalidInputToken
  | InvalidPool
  | InvalidRoute
  | InvalidState
  deriving Repr, DecidableEq

inductive Event where
  | Approved      (owner spender : Address) (token : Address) (value : Amount)
  | ZapInExecuted (sender : Address) (pool : Address) (lpOut : Amount)
  | PausedEvt     (caller : Address)
  | ActivatedEvt  (caller : Address)
  deriving Repr, DecidableEq

structure Context where
  sender : Address
  value  : Amount          -- msg.value (ETH)
  deriving Repr, Inhabited

-- =============================================================================
-- 3. State (C) ：合约存储变量 + FSM 阶段字段
-- =============================================================================

/-- Zap-in 主流程中临时挂起的信息（跨阶段携带） -/
structure PendingZap where
  sender      : Address
  inputToken  : Token
  amountNet   : Amount
  pool        : Address
  tradeData   : Route
  minLP       : Amount
  slippageTol : Amount
  underlyings : List Token := []
  lpOut       : Amount := 0
  deriving Repr, DecidableEq, Inhabited

/-- 合约状态：包含 FSM Phase 和关键状态变量 -/
structure State where
  phase          : Phase
  goodwill       : Amount
  stopped        : Bool
  exchange2Token : Address → Address
  tokenAddrs     : Address → Bool     -- 支持的 ERC20 token 地址集合 (谓词)
  curvePools     : Address → Bool     -- 支持的 Curve 池集合 (谓词)
  owner          : Address
  allowance      : Address → Address → Address → Amount
  contractBal    : Token → Amount     -- 合约持有的资产（抽象）
  pending        : Option PendingZap
  deriving Inhabited

-- =============================================================================
-- 4. 输出结构 / 调用结果
-- =============================================================================

structure TransitionOutput where
  new_state : State
  events    : List Event
  transfers : List (Address × Amount)

instance : Inhabited TransitionOutput :=
  ⟨{ new_state := default, events := [], transfers := [] }⟩

/-- 新增：手写一个“哑”的 Repr，避免要求 State 可 repr -/
instance : Repr TransitionOutput where
  reprPrec _ _ := Std.Format.text "<TransitionOutput>"

inductive CallResult where
  | success (out : TransitionOutput)
  | failure (err : Error)
  deriving Repr

-- =============================================================================
-- 5. helper（守卫 G / 抽象动作 F）
-- =============================================================================

def ok (s : State) (evs : List Event := []) (trs : List (Address × Amount) := []) : CallResult :=
  CallResult.success { new_state := s, events := evs, transfers := trs }

def fail (e : Error) : CallResult :=
  CallResult.failure e

/-- goodwill 计费：按 1/10000 基点抽成 -/
def calcFee (goodwill amountIn : Amount) : Amount :=
  (goodwill * amountIn) / 10000

/-- token 是否被支持（ETH 总是允许；ERC20 需在 tokenAddrs 中） -/
def supportedToken (s : State) (t : Token) : Bool :=
  match t with
  | .eth => true
  | .erc20 a => s.tokenAddrs a

/-- pool 是否被支持 -/
def supportedPool (s : State) (pool : Address) : Bool :=
  s.curvePools pool

/-- allowance 是否足够（仅 ERC20 有意义） -/
def allowanceOK (s : State) (owner : Address) (t : Token) (amt : Amount) : Bool :=
  match t with
  | .eth => true
  | .erc20 a => decide (s.allowance owner ContractAddr a ≥ amt)

/-- 交易路由合法性（抽象占位，可 refine） -/
def routeValid (_tradeData : Route) (_ex2tok : Address → Address) : Bool := true

/-- 滑点约束（抽象占位） -/
def slippageOK (_amtNet _tol : Amount) : Bool := true

-- =============================================================================
-- 6. 非可计算抽象动作（swap / 入池等）
-- =============================================================================

noncomputable section

axiom getUnderlyings : Address → List Token
axiom addLiquiditySim : Address → List Token → Amount

-- =============================================================================
-- 7. FSM 转移函数（每个函数：先 G，再 F）
-- =============================================================================

def pause (ctx : Context) (s : State) : CallResult :=
  if s.phase ≠ Phase.ActiveIdle then
    fail Error.InvalidState
  else if s.stopped then
    fail Error.ContractStopped
  else if ctx.sender ≠ s.owner then
    fail Error.Unauthorized
  else
    let s' := { s with stopped := true, phase := Phase.Paused }
    ok s' [Event.PausedEvt ctx.sender] []

def activate (ctx : Context) (s : State) : CallResult :=
  if s.phase ≠ Phase.Paused then
    fail Error.InvalidState
  else if !s.stopped then
    ok s [] []
  else if ctx.sender ≠ s.owner then
    fail Error.Unauthorized
  else
    let s' := { s with stopped := false, phase := Phase.ActiveIdle }
    ok s' [Event.ActivatedEvt ctx.sender] []

def approveToken (tokenAddr spender : Address) (value : Amount)
    (ctx : Context) (s : State) : CallResult :=
  if s.phase ≠ Phase.ActiveIdle then
    fail Error.InvalidState
  else if s.stopped then
    fail Error.ContractStopped
  else if !s.tokenAddrs tokenAddr then
    fail Error.InvalidTokenAddress
  else
    let newAllowance :=
      fun owner spender' token' =>
        if owner = ctx.sender ∧ spender' = spender ∧ token' = tokenAddr then
          value
        else
          s.allowance owner spender' token'
    let s' := { s with allowance := newAllowance }
    ok s' [Event.Approved ctx.sender spender tokenAddr value] []

def zapInStart (inputToken : Token) (amountIn : Amount) (pool : Address)
    (tradeData : Route) (minLP slippageTol : Amount)
    (ctx : Context) (s : State) : CallResult :=
  if s.phase ≠ Phase.ActiveIdle then
    fail Error.InvalidState
  else if s.stopped then
    fail Error.ContractStopped
  else if !supportedPool s pool then
    fail Error.InvalidPool
  else if amountIn = 0 then
    fail Error.InsufficientInput
  else if !supportedToken s inputToken then
    fail Error.InvalidInputToken
  else if !allowanceOK s ctx.sender inputToken amountIn then
    fail Error.InsufficientAllowance
  else
    let ethOk : Bool :=
      match inputToken with
      | .eth => decide (ctx.value = amountIn)
      | .erc20 _ => true
    if !ethOk then
      fail Error.InsufficientInput
    else
      let fee       := calcFee s.goodwill amountIn
      let amountNet := amountIn - fee
      let pz : PendingZap :=
        { sender := ctx.sender
          inputToken := inputToken
          amountNet := amountNet
          pool := pool
          tradeData := tradeData
          minLP := minLP
          slippageTol := slippageTol }
      let s' := { s with phase := Phase.ZapRequested, pending := some pz }
      ok s' [] []

def performSwap (ctx : Context) (s : State) : CallResult :=
  if s.phase ≠ Phase.ZapRequested then
    fail Error.InvalidState
  else if s.stopped then
    fail Error.ContractStopped
  else
    match s.pending with
    | none => fail Error.InvalidState
    | some pz =>
      if pz.sender ≠ ctx.sender then
        fail Error.Unauthorized
      else if !routeValid pz.tradeData s.exchange2Token then
        fail Error.InvalidRoute
      else if !slippageOK pz.amountNet pz.slippageTol then
        fail Error.SlippageToleranceExceeded
      else
        let under := getUnderlyings pz.pool
        let pz' := { pz with underlyings := under }
        let s'  := { s with phase := Phase.Swapped, pending := some pz' }
        ok s' [] []

def addLiquidity (ctx : Context) (s : State) : CallResult :=
  if s.phase ≠ Phase.Swapped then
    fail Error.InvalidState
  else if s.stopped then
    fail Error.ContractStopped
  else
    match s.pending with
    | none => fail Error.InvalidState
    | some pz =>
      if pz.sender ≠ ctx.sender then
        fail Error.Unauthorized
      else
        let lpOut := addLiquiditySim pz.pool pz.underlyings
        if lpOut < pz.minLP then
          fail Error.SlippageToleranceExceeded
        else
          let pz' := { pz with lpOut := lpOut }
          let s'  := { s with phase := Phase.LiquidityAdded, pending := some pz' }
          ok s' [] []

def returnLP (ctx : Context) (s : State) : CallResult :=
  if s.phase ≠ Phase.LiquidityAdded then
    fail Error.InvalidState
  else if s.stopped then
    fail Error.ContractStopped
  else
    match s.pending with
    | none => fail Error.InvalidState
    | some pz =>
      if pz.sender ≠ ctx.sender then
        fail Error.Unauthorized
      else if pz.lpOut = 0 then
        fail Error.InsufficientInput
      else
        let evs := [Event.ZapInExecuted pz.sender pz.pool pz.lpOut]
        let trs := [(pz.sender, pz.lpOut)]
        let s'  := { s with phase := Phase.ActiveIdle, pending := none }
        ok s' evs trs

end

-- =============================================================================
-- 8. 初始状态
-- =============================================================================

def initState : State :=
  { phase := Phase.ActiveIdle
    goodwill := 0
    stopped := false
    exchange2Token := fun a => a
    tokenAddrs := fun _ => false
    curvePools := fun _ => false
    owner := 1
    allowance := fun _ _ _ => 0
    contractBal := fun _ => 0
    pending := none }

end ZapInFSM
