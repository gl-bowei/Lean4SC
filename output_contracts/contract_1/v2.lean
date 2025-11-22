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

-- [修改 1]: 定义更完善的转账动作结构，以区分 "Pull" (用户->合约) 和 "Push" (合约->用户)
structure TransferAction where
  fromAddr : Address
  toAddr   : Address
  token    : Address
  amount   : Amount
  deriving Repr, DecidableEq

-- [修改 2]: 更新 Output 结构以使用新的 Action
structure TransitionOutput_Fixed where
  new_state : State
  events : List Event
  transfers : List TransferAction -- 使用新的结构

-- 调用结果 (CallResult) - 区分成功与失败
inductive CallResult where
  | success (out : TransitionOutput_Fixed)
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
-- [修改 3]: 修复后的 ZapIn 函数
def ZapInWithERC20_Fixed (inToken : Address) (inAmount : Amount) (minLiquidity : Amount)
                         (ctx : Context) (s : State) : CallResult:=
  -- Guards 保持不变
  if s.stopped then
    CallResult.failure Error.ContractStopped
  else if inAmount = 0 then
    CallResult.failure Error.InsufficientInput
  else if s.allowances ctx.sender inToken < inAmount then
    CallResult.failure Error.InsufficientAllowance
  else
    match simulate_swap inToken inAmount s with
    | none => CallResult.failure Error.InvalidInputToken
    | some swapAmount =>
      let poolTokens := swapAmount * 9 / 10

      if poolTokens < minLiquidity then
        CallResult.failure Error.SlippageToleranceExceeded
      else
        -- [修复 A]: 状态更新 - 扣除用户的 Allowance
        -- 这防止了重放攻击 (Replay Attack)
        let newAllowances := fun (owner : Address) (spdr : Address) =>
          if owner = ctx.sender ∧ spdr = inToken then
            s.allowances owner spdr - inAmount
          else
            s.allowances owner spdr

        let newState := { s with
          goodwill := s.goodwill + (inAmount / 1000),
          allowances := newAllowances -- <--- 关键修复: 应用新的 allowance
        }

        -- [修复 B]: 动作列表 - 包含 "Pull" 和 "Push"
        -- 1. Pull: 把钱从用户那里拿过来 (TransferFrom)
        let actionPull : TransferAction := {
          fromAddr := ctx.sender,
          toAddr := 999, -- 假设 999 是合约地址
          token := inToken,
          amount := inAmount
        }
        -- 2. Push: 把 LP Token 给用户
        let actionPush : TransferAction := {
          fromAddr := 999,
          toAddr := ctx.sender,
          token := s.curvePool, -- 假设这是 LP Token 地址
          amount := poolTokens
        }

        let transfers := [actionPull, actionPush]
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

--------------------------------------
--- 性质集
--- ==============================================================================

--- prop 1 公平交换定理 (Fair Exchange ）
theorem zap_in_enforces_payment
  (inToken : Address) (inAmount : Amount) (minLiquidity : Amount)
  (ctx : Context) (s : State) (out : TransitionOutput_Fixed) :
  -- 前提：调用成功
  ZapInWithERC20_Fixed inToken inAmount minLiquidity ctx s = CallResult.success out →
  -- 结论：转账列表中必须包含一笔 Pull (用户 -> 合约) 的操作
  let expectedPull := {
    fromAddr := ctx.sender,
    toAddr   := 999,      -- 假设合约地址是 999
    token    := inToken,
    amount   := inAmount
  : TransferAction }
  expectedPull ∈ out.transfers :=
by
  sorry


--- prop 2 授权扣减一致性
theorem zap_in_reduces_allowance
  (inToken : Address) (inAmount : Amount) (minLiquidity : Amount)
  (ctx : Context) (s : State) (out : TransitionOutput_Fixed) :
  -- 前提 1: 调用成功
  ZapInWithERC20_Fixed inToken inAmount minLiquidity ctx s = CallResult.success out →
  -- 前提 2: 初始授权必须足够 (由成功隐含，但显式写出有助于理解)
  s.allowances ctx.sender inToken >= inAmount →
  -- 结论：新状态的授权 = 旧状态授权 - 输入金额
  out.new_state.allowances ctx.sender inToken = s.allowances ctx.sender inToken - inAmount :=
by
  sorry

--- prop3 滑点与输出原子性 (Slippage & Output Atomicity)
theorem zap_in_guarantees_slippage
  (inToken : Address) (inAmount : Amount) (minLiquidity : Amount)
  (ctx : Context) (s : State) (out : TransitionOutput_Fixed) :
  ZapInWithERC20_Fixed inToken inAmount minLiquidity ctx s = CallResult.success out →
  -- 结论：存在一笔 Push 动作，满足接收者是用户且金额达标
  ∃ (act : TransferAction),
    act ∈ out.transfers ∧
    act.toAddr = ctx.sender ∧
    act.token = s.curvePool ∧
    act.amount ≥ minLiquidity :=
by
  sorry


--- 问题：没有办法保护经济意义上的攻击
