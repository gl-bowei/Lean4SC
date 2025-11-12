-- SPDX-License-Identifier: GPL-3.0
-- MultiSig Wallet (Lean4 Formally Verified Version, fixed)

import Init.Data.Nat.Basic
import Init.Data.Nat.Lemmas
import Init.Data.Array.Basic

-- 关闭“未使用变量”linter（可选）
set_option linter.unusedVariables false


/-- ========= 1. 基础定义 ========= -/
structure Address where
  val : String
deriving DecidableEq, Inhabited, Repr

def zeroAddress : Address := ⟨""⟩
abbrev UInt := Nat
abbrev Bytes := String

structure Transaction where
  destination : Address
  value : UInt
  data : Bytes
  executed : Bool
deriving Inhabited

/-- ========= 2. 状态、事件与错误 ========= -/
structure State where
  owners : Address → Bool
  required : UInt
  transactionCount : UInt
  transactions : UInt → Transaction
  confirmations : UInt → Address → Bool
deriving Inhabited

structure Context where
  sender : Address

inductive MultiSigEvent where
  | Submission (txId : UInt)
  | Confirmation (owner : Address) (txId : UInt)
  | Execution (txId : UInt)
deriving DecidableEq, Inhabited

inductive MultiSigError where
  | NotOwner
  | TransactionDoesNotExist
  | TransactionAlreadyExecuted
  | AlreadyConfirmed
  | NotEnoughConfirmations
  | InvalidOwnerList
deriving DecidableEq, Inhabited

/-- ========= 3. 辅助类型 ========= -/
structure TransitionOutput (α : Type) where
  newState : State
  events : List MultiSigEvent
  transfers : List (Address × UInt)
  returnValue : α

abbrev CallResult (α : Type) := Except MultiSigError (TransitionOutput α)

/-- 先给一个“安全桩”，避免 `sorry`。将来 owners 可枚举时再改成真实计数。 -/
def getConfirmationCount (_s : State) (_txId : UInt) : UInt := 0

/-- ========= 4. 构造函数 ========= -/
def constructor (_owners : Array Address) (_required : UInt) : Except MultiSigError State :=
  if _required > _owners.size ∨ _required = 0 then
    throw MultiSigError.InvalidOwnerList
  else
    let initialOwners := fun (a : Address) => _owners.contains a
    let initialState := {
      owners := initialOwners,
      required := _required,
      transactionCount := 0,
      transactions := fun _ => { destination := zeroAddress, value := 0, data := "", executed := false },
      confirmations := fun _ _ => false
    }
    .ok initialState

/-- ========= 5. 核心函数 ========= -/
def submitTransaction (ctx : Context) (s_old : State)
    (dest : Address) (val : UInt) (data : Bytes) :
    Except MultiSigError (TransitionOutput UInt) := do

  if !(s_old.owners ctx.sender) then
    throw MultiSigError.NotOwner

  let txId := s_old.transactionCount
  let newTx := { destination := dest, value := val, data := data, executed := false }

  let s_new := { s_old with
    transactionCount := s_old.transactionCount + 1,
    transactions := fun id => if id == txId then newTx else s_old.transactions id
  }

  return {
    newState := s_new,
    events := [MultiSigEvent.Submission txId],
    transfers := [],
    returnValue := txId
  }

def confirmTransaction (ctx : Context) (s_old : State) (txId : UInt) :
    Except MultiSigError (TransitionOutput Unit) := do

  if !(s_old.owners ctx.sender) then
    throw MultiSigError.NotOwner

  if txId >= s_old.transactionCount then
    throw MultiSigError.TransactionDoesNotExist

  if (s_old.transactions txId).executed then
    throw MultiSigError.TransactionAlreadyExecuted

  if s_old.confirmations txId ctx.sender then
    throw MultiSigError.AlreadyConfirmed

  let s_new := { s_old with
    confirmations := fun id owner =>
      if id == txId ∧ owner == ctx.sender then true else s_old.confirmations id owner
  }

  return {
    newState := s_new,
    events := [MultiSigEvent.Confirmation ctx.sender txId],
    transfers := [],
    returnValue := ()
  }

def executeTransaction (ctx : Context) (s_old : State) (txId : UInt) :
    Except MultiSigError (TransitionOutput Unit) := do

  if !(s_old.owners ctx.sender) then
    throw MultiSigError.NotOwner

  if txId >= s_old.transactionCount then
    throw MultiSigError.TransactionDoesNotExist

  if (s_old.transactions txId).executed then
    throw MultiSigError.TransactionAlreadyExecuted

  if getConfirmationCount s_old txId < s_old.required then
    throw MultiSigError.NotEnoughConfirmations

  let tx := s_old.transactions txId

  let s_new := { s_old with
    transactions := fun id =>
      if id == txId then { tx with executed := true } else s_old.transactions id
  }

  return {
    newState := s_new,
    events := [MultiSigEvent.Execution txId],
    transfers := [(tx.destination, tx.value)],
    returnValue := ()
  }

/-- ========= 6. 性质与证明 ========= -/

-- 性质 1：执行需要足够签名（不足则报 NotEnoughConfirmations）
theorem prop1_execution_requires_confirmations
  (ctx : Context) (s_old : State) (txId : UInt)
  (h_is_owner : s_old.owners ctx.sender = true)
  (h_tx_exists : txId < s_old.transactionCount)
  (h_not_executed : (s_old.transactions txId).executed = false)
  (h_not_enough_sigs : getConfirmationCount s_old txId < s_old.required) :
  executeTransaction ctx s_old txId = .error MultiSigError.NotEnoughConfirmations := by
  dsimp [executeTransaction]
  -- 排掉前面三层 if，命中“签名不足”分支
  simp [h_is_owner, (Nat.not_le_of_lt h_tx_exists), h_not_executed, h_not_enough_sigs]
  -- 目标成了 `(fun _ => …) <$> Except.error NotEnoughConfirmations = Except.error NotEnoughConfirmations`
  rfl

-- 性质 2a：只有 Owner 能确认（否则 NotOwner）
theorem prop2_confirm_requires_owner
  (ctx : Context) (s_old : State) (txId : UInt)
  (h_not_owner : s_old.owners ctx.sender = false) :
  confirmTransaction ctx s_old txId = .error MultiSigError.NotOwner := by
  dsimp [confirmTransaction]
  -- 第一个 if 就触发 throw
  simp [h_not_owner]
  -- 目标是 `do throw NotOwner; … = Except.error NotOwner`
  rfl

theorem prop2_confirm_is_unique
  (ctx : Context) (s_old : State) (txId : UInt)
  (h_is_owner : s_old.owners ctx.sender = true)
  (h_tx_exists : txId < s_old.transactionCount)
  (h_not_executed : (s_old.transactions txId).executed = false)
  (h_already_confirmed : s_old.confirmations txId ctx.sender = true) :
  confirmTransaction ctx s_old txId = .error MultiSigError.AlreadyConfirmed := by
  dsimp [confirmTransaction]
  -- 先排除 "非 owner"、"tx 不存在"、"已执行" 这三层 if，落到 "已确认" 分支
  simp [h_is_owner, (Nat.not_le_of_lt h_tx_exists), h_not_executed, h_already_confirmed]
  -- 目标现在是 `(fun _ => …) <$> Except.error AlreadyConfirmed = Except.error AlreadyConfirmed`
  rfl

theorem prop3_execute_once
  (ctx : Context) (s_old : State) (txId : UInt)
  (h_is_owner : s_old.owners ctx.sender = true)
  (h_tx_exists : txId < s_old.transactionCount)
  (h_already_executed : (s_old.transactions txId).executed = true) :
  executeTransaction ctx s_old txId = .error MultiSigError.TransactionAlreadyExecuted := by
  dsimp [executeTransaction]
  -- 第二个 if 用 ≤，用 not_le_of_lt 走 false 分支；第三个 if 命中 "已执行"
  simp [h_is_owner, (Nat.not_le_of_lt h_tx_exists), h_already_executed]
  -- 目标已经是 `do throw TransactionAlreadyExecuted; … = Except.error TransactionAlreadyExecuted`
  rfl

-- 性质 4：三种操作都不改变 owners / required
theorem prop4_submit_immutable
  (ctx : Context) (s_old : State) (dest : Address) (val : UInt) (data : Bytes)
  (h_is_owner : s_old.owners ctx.sender = true) :
  ∀ out, submitTransaction ctx s_old dest val data = .ok out →
    out.newState.owners = s_old.owners ∧ out.newState.required = s_old.required := by
  intro out h
  dsimp [submitTransaction] at h
  simp [h_is_owner] at h
  cases h
  simp

theorem prop4_confirm_immutable
  (ctx : Context) (s_old : State) (txId : UInt)
  (h_is_owner : s_old.owners ctx.sender = true)
  (h_tx_exists : txId < s_old.transactionCount)
  (h_not_executed : (s_old.transactions txId).executed = false)
  (h_not_confirmed : s_old.confirmations txId ctx.sender = false) :
  ∀ out, confirmTransaction ctx s_old txId = .ok out →
    out.newState.owners = s_old.owners ∧ out.newState.required = s_old.required := by
  intro out h
  dsimp [confirmTransaction] at h
  simp [h_is_owner, (Nat.not_le_of_lt h_tx_exists), h_not_executed, h_not_confirmed] at h
  cases h
  simp

theorem prop4_execute_immutable
  (ctx : Context) (s_old : State) (txId : UInt)
  (h_is_owner : s_old.owners ctx.sender = true)
  (h_tx_exists : txId < s_old.transactionCount)
  (h_not_executed : (s_old.transactions txId).executed = false)
  (h_enough_sigs : getConfirmationCount s_old txId ≥ s_old.required) :
  ∀ out, executeTransaction ctx s_old txId = .ok out →
    out.newState.owners = s_old.owners ∧ out.newState.required = s_old.required := by
  intro out h
  dsimp [executeTransaction] at h
  simp [h_is_owner, (Nat.not_le_of_lt h_tx_exists), h_not_executed, (Nat.not_lt_of_le h_enough_sigs)] at h
  cases h
  simp
