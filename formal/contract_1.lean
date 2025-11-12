-- SPDX-License-Identifier: GPL-3.0
--
-- ZapInContract FSM 的 Lean 4 形式化
--

import Init.Data.Nat.Basic
import Init.Data.String

--
-- 基础类型定义
--

abbrev UInt := Nat

structure Address where
  val : String
deriving DecidableEq, Inhabited

def zeroAddress : Address := ⟨""⟩

--
-- FSM 状态定义
--

-- 1. 来自 JSON "states" 字段
-- 这是 FSM 的“控制状态” (Control State)
inductive ZapInFsmState where
  | Idle
  | Processing
  | LiquidityAdded
  | Failed
deriving DecidableEq, Inhabited

-- 2. 来自 JSON "variables" 字段
-- 这是合约的完整状态, 包含“控制状态”和“数据状态”
structure ZapInContractState where
  -- FSM 控制状态
  fsmState : ZapInFsmState
  -- FSM 数据状态 (来自 "variables")
  goodwill : UInt
  stopped : Bool
  exchange2Token : Address
  tokenAddresses : Address → Bool -- 模拟 mapping(address => bool)
deriving Inhabited

-- 3. 来自 JSON "initialState"
-- 定义合约的初始状态
def initialState : ZapInContractState := {
  fsmState := .Idle,
  goodwill := 0,
  stopped := false,
  exchange2Token := zeroAddress,
  tokenAddresses := fun _ => false
}

--
-- FSM 触发器 (Triggers)
--

-- 4. 来自 JSON "transitions" 中的 "trigger" 字段
-- 这些是驱动状态机变化的外部事件/调用
inductive FsmTrigger where
  | ZapIn
  | TokenApproved
  | Error
  | ReturnPoolTokens
  | Retry
deriving DecidableEq

--
-- 抽象函数 (Conditions & Actions)
--

-- 5. 来自 JSON "transitions" 中的 "condition" 字段
-- 我们将“条件”建模为未实现的 'opaque' (不透明) 函数
-- 它们检查当前状态并返回一个布尔值
opaque isActive (s : ZapInContractState) : Bool
opaque sufficientTokens (s : ZapInContractState) : Bool

-- 6. 来自 JSON "transitions" 中的 "action" 字段
-- 我们将“动作”建模为 'opaque' 函数
-- 它们接受旧状态并返回一个 *更新了数据* 的新状态
opaque initializeZapIn (s : ZapInContractState) : ZapInContractState
opaque processTrade (s : ZapInContractState) : ZapInContractState
opaque handleError (s : ZapInContractState) : ZapInContractState
opaque returnTokens (s : ZapInContractState) : ZapInContractState
opaque cleanup (s : ZapInContractState) : ZapInContractState

--
-- FSM 核心逻辑 (The "step" function)
--

-- 7. 这是 FSM 的核心转移函数
-- 它根据当前状态和触发器, 严格按照 JSON 定义的规则进行转移
def step (state : ZapInContractState) (trigger : FsmTrigger) : ZapInContractState :=
  -- 'state.fsmState' 是当前所处的 FSM 状态
  match state.fsmState with

  | .Idle =>
    match trigger with
    | .ZapIn =>
      -- 'condition': "isActive"
      if isActive state then
        -- 'action': "initializeZapIn"
        let stateAfterAction := initializeZapIn state
        -- 'target': "Processing"
        { stateAfterAction with fsmState := .Processing }
      else
        -- 条件不满足, 保持在 'Idle'
        state
    -- 任何其他触发器在 'Idle' 状态下都无效, 保持状态
    | _ => state

  | .Processing =>
    match trigger with
    | .TokenApproved =>
      -- 'condition': "sufficientTokens"
      if sufficientTokens state then
        -- 'action': "processTrade"
        let stateAfterAction := processTrade state
        -- 'target': "LiquidityAdded"
        { stateAfterAction with fsmState := .LiquidityAdded }
      else
        -- 条件不满足, 保持在 'Processing'
        state
    | .Error =>
      -- 'action': "handleError"
      let stateAfterAction := handleError state
      -- 'target': "Failed"
      { stateAfterAction with fsmState := .Failed }
    -- 其他触发器无效, 保持状态
    | _ => state

  | .LiquidityAdded =>
    match trigger with
    | .ReturnPoolTokens =>
      -- 'action': "returnTokens"
      let stateAfterAction := returnTokens state
      -- 'target': "Idle"
      { stateAfterAction with fsmState := .Idle }
    | _ => state

  | .Failed =>
    match trigger with
    | .Retry =>
      -- 'action': "cleanup"
      let stateAfterAction := cleanup state
      -- 'target': "Idle"
      { stateAfterAction with fsmState := .Idle }
    | _ => state
    
