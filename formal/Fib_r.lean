-- FSM_Fibonacci_Fixed.lean
-- 修复后的规范模型 (The "Spec")

import Init.Data.Nat.Basic
import Init.Data.String

abbrev UInt := Nat
abbrev Bytes := List UInt
abbrev Bytes4 := List UInt

structure Address where
  val : String
deriving DecidableEq, Inhabited

--
-- =========================================================
--         第 1 部分: FSM 元组 <S, C, I, O>
-- =========================================================
--

inductive FibonacciControlState where
  | Active
deriving DecidableEq, Inhabited

structure FibonacciStateFSM where
  controlState : FibonacciControlState
  fibonacciLibrary : Address
  calculatedFibNumber : UInt
  start : UInt
  withdrawalCounter : UInt
  balance : UInt
deriving Inhabited

structure FibonacciContext where
  sender : Address
  value : UInt                   -- (*** 修改: 现在需要 msg.value ***)
  delegatecall_success : Bool
deriving DecidableEq, Inhabited

inductive FibonacciOutputEvent where
  | SendEth (to : Address) (amount : UInt)
deriving DecidableEq, Inhabited

inductive FibonacciTrigger where
  | withdraw
  | fallback
deriving DecidableEq, Inhabited

--
-- =========================================================
--         第 2 部分: 'delegatecall' 建模
-- =========================================================
--
-- (这部分与之前相同，因为 'withdraw' 仍然需要它)

partial def internal_fibonacci (n : UInt) (_start : UInt) : UInt :=
  if n == 0 then _start
  else if n == 1 then _start + 1
  else (internal_fibonacci (n - 1) _start) + (internal_fibonacci (n - 2) _start)

--
-- =========================================================
--         第 3 部分: 转换关系 (->) / 'step' 函数
-- =========================================================
--

def step (ctx : FibonacciContext) (state : FibonacciStateFSM) (trigger : FibonacciTrigger)
  : (FibonacciStateFSM × List FibonacciOutputEvent) :=

  match trigger with

  -- --- 转换 1: withdraw() ---
  -- (此分支与之前 *完全相同*)
  | .withdraw =>
    if !ctx.delegatecall_success then
      (state, [])
    else
      let newCounter := state.withdrawalCounter + 1
      let n := newCounter
      let fib_n := internal_fibonacci n state.start
      let newCalculatedFib := fib_n
      let amountToWithdraw := newCalculatedFib * 1000000000000000000

      if state.balance < amountToWithdraw then
        (state, [])
      else
        let newState := { state with
          withdrawalCounter := newCounter,
          calculatedFibNumber := newCalculatedFib,
          balance := state.balance - amountToWithdraw
        }
        let output := [FibonacciOutputEvent.SendEth ctx.sender amountToWithdraw]
        (newState, output)

  -- --- 转换 2: fallback() (*** 已修改 ***) ---
  | .fallback =>
    -- (*** 修正: 删除了所有 'delegatecall' 和 'msg.data' 逻辑 ***)

    -- 动作 F_2: 仅对 'payable' 行为建模
    let newState := { state with balance := state.balance + ctx.value }
    (newState, [])

--
-- =========================================================
--         第 4 部分: 证明安全性质 (P_Secure)
-- =========================================================
--
-- P_Secure: "fallback 转换绝不能改变 'start' 变量"
--
-- 我们现在证明, *这个* FSM 满足了该性质。
--
theorem fallback_does_not_change_start :
  ∀ (ctx : FibonacciContext) (state : FibonacciStateFSM),

  -- 假设: 我们执行了一次 'fallback' 转换
  let (newState, _) := step ctx state .fallback

  -- 结论: 新状态的 'start' 必须等于旧状态的 'start'
  newState.start = state.start
:=
by
  -- 证明:
  intro ctx state
  unfold step

  -- 'step' 函数 'match' 了 .fallback
  -- 目标变为:
  --   ( { state with balance := state.balance + ctx.value }, [] ).fst.start = state.start

  -- 'simp'  tactic (简化) 将自动计算元组的第一部分 (.fst)
  -- 并展开记录更新
  simp

  -- 'simp' 之后, 目标变为:
  --   state.start = state.start

  -- 'rfl' (自反性) 证明了这一点
-- (证明成功!)
