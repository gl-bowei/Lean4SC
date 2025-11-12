-- FSM_Fibonacci.lean
-- 规范模型 (The "Spec")

import Init.Data.Nat.Basic
import Init.Data.String

abbrev UInt := Nat
abbrev Bytes := List UInt  -- 'bytes' 的简单抽象
abbrev Bytes4 := List UInt -- 'bytes4' 的简单抽象

structure Address where
  val : String
deriving DecidableEq, Inhabited

--
-- =========================================================
--         第 1 部分: FSM 元组 <S, C, I, O>
-- =========================================================
--

-- FSM $S$ (有限状态集)
-- (这个合约只有一个状态, 它在构造后始终是 Active)
inductive FibonacciControlState where
  | Active
deriving DecidableEq, Inhabited

-- FSM $C$ (合约变量)
structure FibonacciStateFSM where
  controlState : FibonacciControlState -- (总是 .Active)
  fibonacciLibrary : Address
  calculatedFibNumber : UInt
  start : UInt
  withdrawalCounter : UInt
  balance : UInt                 -- (抽象 'address(this).balance')
deriving Inhabited

-- FSM $I$ (输入变量)
structure FibonacciContext where
  sender : Address
  data : Bytes                   -- (来自 msg.data)
  delegatecall_success : Bool    -- (来自 'delegatecall' 的 "预言机")
deriving DecidableEq, Inhabited

-- FSM $O$ (输出/事件)
inductive FibonacciOutputEvent where
  | SendEth (to : Address) (amount : UInt)
deriving DecidableEq, Inhabited

-- FSM 触发器 (Triggers)
inductive FibonacciTrigger where
  | withdraw
  | fallback
deriving DecidableEq, Inhabited

--
-- =========================================================
--         第 2 部分: 挑战的核心 - 建模 'delegatecall'
-- =========================================================
--
-- 为了精确建模 'FibonacciBalance' 的 FSM,
-- 我们 *必须* 在 FSM 规范中 "内联" 'FibonacciLib' 的逻辑。
--

-- 'FibonacciLib.fibonacci(n)' 逻辑的 FSM 副本
-- (我们使用 'partial' 来告诉 Lean 我们不关心这个辅助函数的终止性证明)
partial def internal_fibonacci (n : UInt) (_start : UInt) : UInt :=
  if n == 0 then _start
  else if n == 1 then _start + 1
  else (internal_fibonacci (n - 1) _start) + (internal_fibonacci (n - 2) _start)

-- 抽象: 'FibonacciLib' 的函数选择器 (Selectors)
-- 我们将它们声明为 'opaque', 因为 FSM 不关心 'sha3' 的具体值,
-- 只关心它们是 *不同* 的常量。
opaque fibSig : Bytes4
opaque setStartSig : Bytes4
opaque setFibonacciSig : Bytes4

-- 抽象: 'msg.data' 解码器
opaque decode_bytes_selector (data : Bytes) : Bytes4
opaque decode_uint_payload (data : Bytes) : UInt

--
-- =========================================================
--         第 3 部分: 转换关系 (->) / 'step' 函数
-- =========================================================
--

-- 'step' 函数定义了 FSM 的所有转换
-- 它返回一个新状态和一组输出
def step (ctx : FibonacciContext) (state : FibonacciStateFSM) (trigger : FibonacciTrigger)
  : (FibonacciStateFSM × List FibonacciOutputEvent) :=

  -- S 只有一个状态 'Active', 所以我们直接 'match' 触发器
  match trigger with

  -- --- 转换 1: withdraw() ---
  | .withdraw =>
    -- 守卫 G_1 (来自 'require(fibonacciLibrary.delegatecall(...))')
    if !ctx.delegatecall_success then
      (state, []) -- 守卫失败, FSM revert (状态不变)
    else
      -- 动作 F_1
      let newCounter := state.withdrawalCounter + 1

      -- --- 开始 'delegatecall' 效果的精确建模 ---
      -- (我们 '内联' 了 'setFibonacci(newCounter)' 的逻辑)
      let n := newCounter
      let fib_n := internal_fibonacci n state.start
      let newCalculatedFib := fib_n
      -- --- 结束 'delegatecall' 效果 ---

      let amountToWithdraw := newCalculatedFib * 1000000000000000000 -- 1 ether

      -- 'msg.sender.transfer()' 的隐式守卫
      if state.balance < amountToWithdraw then
        (state, []) -- 守卫失败, FSM revert (状态不变)
      else
        -- 转换成功
        let newState := { state with
          withdrawalCounter := newCounter,
          calculatedFibNumber := newCalculatedFib,
          balance := state.balance - amountToWithdraw
        }
        let output := [FibonacciOutputEvent.SendEth ctx.sender amountToWithdraw]
        (newState, output)

-- --- 转换 2: fallback() ---
  | .fallback =>
    -- 守卫 G_2 (来自 'require(fibonacciLibrary.delegatecall(msg.data))')
    if !ctx.delegatecall_success then
      (state, []) -- 守卫失败, FSM revert (状态不变)
    else
      -- 动作 F_2 (对 'msg.data' 解码并 '内联' 'FibonacciLib' 的逻辑)
      let selector := decode_bytes_selector ctx.data

      -- (*** 修正开始 ***)
      -- 错误: 'let mut newState := state' (已删除)

      -- 路径 A: 攻击者调用 'setStart(uint _start)'
      if selector == setStartSig then
        let _start_val := decode_uint_payload ctx.data
        -- 修正: 在 'if' 分支内部创建并返回新状态
        let newState := { state with start := _start_val }
        (newState, []) -- 状态已更新, 无输出

      -- 路径 B: 攻击者调用 'setFibonacci(uint n)'
      else if selector == setFibonacciSig then
        let n_val := decode_uint_payload ctx.data
        let fib_n_val := internal_fibonacci n_val state.start
        -- 修正: 在 'else if' 分支内部创建并返回新状态
        let newState := { state with calculatedFibNumber := fib_n_val }
        (newState, []) -- 状态已更新, 无输出

      -- 路径 C: 调用了其他函数
      else
        (state, []) -- 'delegatecall' 成功了, 但没有匹配的函数, 状态不变
      -- (*** 修正结束 ***)

--
-- =========================================================
--         第 4 部分: 证明安全性质 (P_Secure)
-- =========================================================
--
-- P_Secure: "fallback 转换绝不能改变 'start' 变量"
--
theorem fallback_should_not_change_start :
  ∀ (ctx : FibonacciContext) (state : FibonacciStateFSM),

  -- 假设: 我们执行了一次 'fallback' 转换
  let (newState, _) := step ctx state .fallback

  -- 结论: 新状态的 'start' 必须等于旧状态的 'start'
  newState.start = state.start
:=
by
  sorry
