import Lean

-- ==============================================================================
-- 1. 通用类型定义 (Universal Type Definitions)
-- ==============================================================================

-- 使用 abbrev 继承 Nat 的所有特性（算术、比较、字面量），解决 synthesizer 错误
abbrev Address := Nat
abbrev Amount := Nat

-- 错误类型 (Error Types) - 对应 FSM 守卫失败的情况
inductive Error where
  | InsufficientAllowance     -- 授权不足
  | InsufficientInput         -- 输入金额无效
  | ContractStopped           -- 合约已暂停
  | SlippageToleranceExceeded -- 滑点过高
  | InvalidTokenAddress       -- 地址无效
  | Unauthorized              -- 权限不足
  | InvalidInputToken         -- 输入代币不支持
  deriving Repr, DecidableEq

-- 事件类型 (Events) - 对应副作用中的日志
inductive Event where
  | Approved (owner : Address) (spender : Address) (value : Amount)
  | ZapInExecuted (sender : Address) (inAmount : Amount) (outPoolTokens : Amount)
  | ContractStatusChanged (isStopped : Bool)
  deriving Repr, DecidableEq

-- ==============================================================================
-- 2. 状态与上下文结构 (State & Context Structures)
-- ==============================================================================

-- 调用上下文 (Context) - 模拟 msg.sender, msg.value
structure Context where
  sender : Address
  value : Amount
  deriving Repr, DecidableEq

-- 合约状态 (State) - 对应 FSM 的状态元组
structure State where
  stopped : Bool
  goodwill : Amount
  -- 映射实现为函数: Address -> Address -> Amount
  allowances : Address → Address → Amount
  exchange2Token : Address → Address
  curvePool : Address
  -- 为了简化，这里不 derive Repr/DecidableEq，因为包含函数类型

-- 转移输出 (TransitionOutput) - 包含新状态和副作用列表
structure TransitionOutput where
  new_state : State
  events : List Event
  transfers : List (Address × Amount) -- (接收者, 金额)

-- 调用结果 (CallResult) - 区分成功与失败
inductive CallResult where
  | success (out : TransitionOutput)
  | failure (err : Error)

-- ==============================================================================
-- 3. 辅助逻辑与 FSM 动作 (Helper Functions & Logic)
-- ==============================================================================

-- 模拟 Swap 逻辑 (F3 Action)
-- 注意：Nat 的除法是整数除法，自动向下取整
def simulate_swap (_inToken : Address) (inAmount : Amount) (_s : State) : Option Amount :=
  if inAmount > 0 then
    -- 模拟兑换：99% 的转化率
    some (inAmount * 99 / 100)
  else
    none

-- ==============================================================================
-- 4. 状态转移函数 (State Transition Functions)
-- ==============================================================================

------------------------------------------------------------------------------
-- 函数: approveToken
-- FSM: Idle -> Approval -> Idle
------------------------------------------------------------------------------
def approveToken (spender : Address) (amount : Amount) (ctx : Context) (s : State) : CallResult :=
  -- [Guard 1]: 检查 spender 地址是否为 0 (使用显式类型转换确保匹配)
  if spender = (0 : Address) then
    CallResult.failure Error.InvalidTokenAddress
  else
    -- [Action]: 状态更新 (Functional Update Pattern)
    -- 更新 allowances 映射：仅当 owner 和 spender 匹配时改变值，否则保持原值
    let newAllowances := fun (owner : Address) (spdr : Address) =>
      if owner = ctx.sender ∧ spdr = spender then
        amount
      else
        s.allowances owner spdr

    let newState := { s with allowances := newAllowances }

    -- [Side Effects]: 产生事件
    let events := [Event.Approved ctx.sender spender amount]

    CallResult.success {
      new_state := newState,
      events := events,
      transfers := [] -- 无转账
    }

------------------------------------------------------------------------------
-- 函数: ZapInWithERC20
-- FSM: Idle -> Processing -> Provision -> Finalized -> Idle
------------------------------------------------------------------------------
def ZapInWithERC20 (inToken : Address) (inAmount : Amount) (minLiquidity : Amount) (ctx : Context) (s : State) : CallResult :=
  -- [Guard 1]: 合约是否暂停
  if s.stopped then
    CallResult.failure Error.ContractStopped
  -- [Guard 2]: 输入金额检查
  else if inAmount = 0 then
    CallResult.failure Error.InsufficientInput
  -- [Guard 3]: 授权额度检查 (直接调用状态中的函数映射)
  else if s.allowances ctx.sender inToken < inAmount then
    CallResult.failure Error.InsufficientAllowance
  else
    -- [Action F3]: 模拟代币兑换
    match simulate_swap inToken inAmount s with
    | none => CallResult.failure Error.InvalidInputToken
    | some swapAmount =>
      -- [Action F4]: 模拟计算获得的 LP token (假设再折损 10%)
      let poolTokens := swapAmount * 9 / 10

      -- [Guard 4]: 滑点检查 (Slippage Check)
      if poolTokens < minLiquidity then
        CallResult.failure Error.SlippageToleranceExceeded
      else
        -- [Action F5]: 状态更新与副作用
        -- 1. 更新 goodwill (纯算术计算)
        let newGoodwill := s.goodwill + (inAmount / 1000)
        let newState := { s with goodwill := newGoodwill }

        -- 2. 构造转账列表 (使用标准元组语法)
        let transfers : List (Address × Amount) := [(ctx.sender, poolTokens)]

        -- 3. 构造事件列表
        let events := [Event.ZapInExecuted ctx.sender inAmount poolTokens]

        CallResult.success {
          new_state := newState,
          events := events,
          transfers := transfers
        }

------------------------------------------------------------------------------
-- 函数: setStopped
-- FSM: Any -> Stopped / Stopped -> Idle
------------------------------------------------------------------------------
def setStopped (newStoppedStatus : Bool) (ctx : Context) (s : State) : CallResult :=
  -- [Guard]: 权限检查 (硬编码管理员地址为 1)
  -- 因为 Address 是 Nat，这里直接用 ≠ 比较是可判定的
  if ctx.sender ≠ (1 : Address) then
    CallResult.failure Error.Unauthorized
  else
    -- [Action]: 更新 stopped 状态
    let newState := { s with stopped := newStoppedStatus }
    let events := [Event.ContractStatusChanged newStoppedStatus]

    CallResult.success {
      new_state := newState,
      events := events,
      transfers := []
    }

-- ==============================================================================
-- 漏洞攻击演示 (Vulnerability Exploit)
-- ==============================================================================

-- 1. 定义攻击场景
def attacker : Address := 666
def token : Address := 10
def contract : Address := 999
def amount : Amount := 1000

-- 2. 构造初始状态：攻击者拥有足够的授权
def vulnerableState : State := {
  stopped := false,
  goodwill := 0,
  allowances := fun owner spender => if owner = attacker ∧ spender = token then 2000 else 0, -- 授权 2000
  exchange2Token := fun _ => 0,
  curvePool := 0
}

def attackContext : Context := {
  sender := attacker,
  value := 0
}

