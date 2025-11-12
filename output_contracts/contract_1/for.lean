-- SPDX-License-Identifier: MIT
--
-- ZapInContract (Solidity 实现) 的 Lean 4 形式化模型
--

import Init.Data.Nat.Basic
import Init.Data.String
import Init.Data.Nat.Lemmas -- 导入 Nat 引理 (用于证明)

--
-- 基础类型定义
--

abbrev UInt := Nat

structure Address where
  val : String
deriving DecidableEq, Inhabited

def zeroAddress : Address := ⟨""⟩

--
-- 合约状态 (FSM $C$ 变量)
--
structure ZapInContractState where
  owner : Address
  goodwill : UInt
  stopped : Bool
  exchange2Token : Address
  tokenAddresses : Address → Bool -- 模拟 mapping(address => bool)
deriving Inhabited

--
-- 交易上下文 (FSM $I$ 变量)
--
structure TxContext where
  sender : Address
  -- 'value' 在这个模型中未使用, 但为完整性保留
  value : UInt

--
-- 合约事件 (FSM $O$ 变量)
--
inductive ContractEvent where
  | ContractActivated
  | ContractPaused
  | TokenApprovalConfirmed
deriving DecidableEq

--
-- Revert 错误 (Solidity 'require' 语句)
--
inductive ContractError where
  | NotOwner   -- "ZapInContract: Not owner"
  | IsPaused   -- "ZapInContract: Contract is paused"
  | IsEnabled  -- (隐式, 如果 'activate' 时未暂停)
  | IsStopped  -- (隐式, 如果 'pause' 时已暂停)
  | TokenNotApproved -- "ZapInContract: Token not approved"

--
-- 成功的函数输出
--
structure TransitionOutput (α : Type) where
  newState : ZapInContractState
  events : List ContractEvent
  returnValue : α

--
-- 函数调用结果 (模拟 Revert)
--
abbrev CallResult (α :Type) := Except ContractError (TransitionOutput α)

--
-- 状态更新的辅助函数
--
def ZapInContractState.updateTokenAddresses (s : ZapInContractState) (addr : Address) (val : Bool) : ZapInContractState :=
  { s with tokenAddresses := fun a => if a == addr then val else s.tokenAddresses a }

--
-- 构造函数
--
def constructor (ctx : TxContext) : ZapInContractState := {
  owner := ctx.sender,
  goodwill := 0,
  stopped := false, -- s0 = Enabled
  exchange2Token := zeroAddress,
  tokenAddresses := fun _ => false
}

--
-- FSM 动作 $F_4$ 的抽象
--
opaque _performZapLogic (s : ZapInContractState) (token : Address) : ZapInContractState

--
-- 形式化的 Solidity 函数
--

def pause (ctx : TxContext) (state : ZapInContractState) : Except ContractError (TransitionOutput Unit) := do
  if ctx.sender != state.owner then throw .NotOwner
  if state.stopped == true then throw .IsStopped

  let newState := { state with stopped := true }
  let events := [ContractEvent.ContractPaused]

  return { newState := newState, events := events, returnValue := () }

def activate (ctx : TxContext) (state : ZapInContractState) : Except ContractError (TransitionOutput Unit) := do
  if ctx.sender != state.owner then throw .NotOwner
  if state.stopped == false then throw .IsEnabled

  let newState := { state with stopped := false }
  let events := [ContractEvent.ContractActivated]

  return { newState := newState, events := events, returnValue := () }

def approveToken (ctx : TxContext) (state : ZapInContractState) (token : Address) : Except ContractError (TransitionOutput Unit) := do
  if state.stopped == true then throw .IsPaused

  let newState := state.updateTokenAddresses token true
  let events := [ContractEvent.TokenApprovalConfirmed]

  return { newState := newState, events := events, returnValue := () }

def ZapIn (ctx : TxContext) (state : ZapInContractState) (token : Address) : Except ContractError (TransitionOutput Unit) := do
  if state.stopped == true then throw .IsPaused
  if state.tokenAddresses token == false then throw .TokenNotApproved

  let newState := _performZapLogic state token

  return { newState := newState, events := [], returnValue := () }

--
-- 形式化的其他函数
--

-- 修正: 使用 '_ctx' 标记未使用的变量
def ZapInWithETH (_ctx : TxContext) (state : ZapInContractState) : Except ContractError (TransitionOutput Unit) := do
  if state.stopped == true then throw .IsPaused
  return { newState := state, events := [], returnValue := () }

-- 修正: 使用 '_ctx' 和 '_token' 标记未使用的变量
def ZapInWithERC20 (_ctx : TxContext) (state : ZapInContractState) (_token : Address) : Except ContractError (TransitionOutput Unit) := do
  if state.stopped == true then throw .IsPaused
  return { newState := state, events := [], returnValue := () }

-- 修正: 使用 '_ctx' 标记未使用的变量
def enterLiquidity (_ctx : TxContext) (state : ZapInContractState) : Except ContractError (TransitionOutput Unit) := do
  if state.stopped == true then throw .IsPaused
  return { newState := state, events := [], returnValue := () }

--
-- =========================================================
-- =========================================================
--            性质 (Properties) 与 证明 (Proofs)
-- =========================================================
-- =========================================================

--
-- 性质 1: 访问控制 (Access Control)
-- (修正版: 使用 'simp only' 来控制 'simp' 的行为)
--
theorem non_owner_cannot_pause :
  ∀ (ctx : TxContext) (state : ZapInContractState),
  -- 假设是一个 Bool 等式
  (ctx.sender != state.owner) = true →
  pause ctx state = .error .NotOwner :=
by
  -- 引入假设 h_cond_is_true
  intro ctx state h_cond_is_true
  unfold pause

  -- 修正:
  -- 使用 'simp only' 来*仅仅*展开 'do' 语句 (bind, pure, etc.)
  -- 这可以防止 'simp' 自动简化或重写 'if' 语句
  simp only [bind, pure, Except.bind, Except.pure]

  -- 此时, 目标 (goal) 仍然是:
  -- ⊢ (if ctx.sender != state.owner then ...) = Except.error ContractError.NotOwner

  -- 现在 'rw [if_pos ...]' 可以找到它要找的 'if' 语句了
  -- 'if_pos' 需要一个 '... = true' 的证明, 而 'h_cond_is_true' 正是
  rw [if_pos h_cond_is_true]


--
-- 性质 2: 紧急停止 (Emergency Stop)
-- (修正版: 移除了 'simp made no progress' 警告)
--
theorem cannot_zap_if_paused :
  ∀ (ctx : TxContext) (state : ZapInContractState) (token : Address),
  state.stopped = true →
  ZapIn ctx state token = .error .IsPaused :=
by
  intro ctx state token h_stopped
  unfold ZapIn
  simp [bind, pure, Except.bind, Except.pure]
  -- 修正: 'simp' 足够智能, 只需 'h_stopped'
  simp [h_stopped]

--
-- 性质 3: 状态转换正确性 (State Transition Correctness)
-- (修正版: 移除了 'injection' 错误)
--
theorem pause_sets_stopped_to_true :
  ∀ (ctx : TxContext) (state : ZapInContractState) (out : TransitionOutput Unit),
  state.stopped = false →
  ctx.sender = state.owner →
  pause ctx state = .ok out →
  out.newState.stopped = true :=
by
  intro ctx state out h_not_stopped h_is_owner h_pause_ok
  unfold pause at h_pause_ok
  simp [bind, pure, Except.bind, Except.pure] at h_pause_ok

  rw [h_is_owner] at h_pause_ok
  simp at h_pause_ok

  rw [h_not_stopped] at h_pause_ok
  simp at h_pause_ok

  -- 此时 'h_pause_ok' 已经被 'simp' 简化为:
  -- { newState := ..., stopped := true, ... } = out
  -- (注意: 'Except.ok' 包装器已经被 'simp' 自动解开)

  -- 修正:
  -- 直接使用 'rw [<-h_pause_ok]' (反向重写)
  -- 将目标 'out.newState.stopped' 中的 'out'
  -- 替换为 'h_pause_ok' 左侧的结构体
  rw [<-h_pause_ok]

  -- 'simp' 自动简化: true = true
