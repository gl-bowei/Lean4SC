-- SPDX-License-Identifier: GPL-3.0
-- 形式化 Lean 4 代码 (已修正 noncomputable 错误)

import Init.Data.Nat.Basic
import Init.Data.Nat.Lemmas -- 导入 Nat 相关的引理，如 not_le, not_lt_of_le

-- 修正: 使用 'structure' 替代 'opaque' 以获得可计算的实例
structure Address where
  val : String
deriving DecidableEq, Inhabited -- 自动派生可计算的 '==' 和 'default'

-- 修正: 'zeroAddress' 现在是 'Address' 结构体的一个实例
def zeroAddress : Address := ⟨""⟩ -- 使用空字符串作为 "0 地址"

-- 将 uint, timestamp, value 映射为 Lean 的 Nat
abbrev UInt := Nat
abbrev Timestamp := Nat
abbrev Value := Nat

-- 合约的状态结构体
structure SimpleAuctionState where
  beneficiary : Address
  auctionEndTime : Timestamp
  highestBidder : Address
  highestBid : Value
  -- mapping(address => uint) 在功能上等价于一个从地址到值的函数
  pendingReturns : Address → Value
  ended : Bool
deriving Inhabited -- 'deriving Inhabited' 现在可以正常工作了

-- 交易上下文，模拟 msg 和 block 变量
structure TxContext where
  sender : Address
  value : Value
  timestamp : Timestamp

-- 合约事件
inductive AuctionEvent where
  | HighestBidIncreased (bidder : Address) (amount : Value)
  | AuctionEnded (winner : Address) (amount : Value)
deriving DecidableEq -- 修正: 'deriving' 现在可以正常工作

-- 合约自定义错误
inductive AuctionError where
  | AuctionAlreadyEnded
  | BidNotHighEnough (highestBid : Value)
  | AuctionNotYetEnded
  | AuctionEndAlreadyCalled
deriving DecidableEq

-- 帮助函数：更新 mapping (Address → Value)
-- 这模拟了 pendingReturns[addr] = val
-- 修正: 移除 'noncomputable'
def SimpleAuctionState.updatePendingReturns (s : SimpleAuctionState) (addr : Address) (val : Value) : SimpleAuctionState :=
  { s with pendingReturns := fun a => if a == addr then val else s.pendingReturns a }

-- 帮助函数：增加 mapping (Address → Value)
-- 这模拟了 pendingReturns[addr] += val
-- 修正: 移除 'noncomputable'
def SimpleAuctionState.addPendingReturns (s : SimpleAuctionState) (addr : Address) (val : Value) : SimpleAuctionState :=
  { s with pendingReturns := fun a => if a == addr then s.pendingReturns a + val else s.pendingReturns a }

-- 成功执行的输出 (针对 `bid` 和 `auctionEnd`)
-- α 是函数返回值类型 (对于 `bid` 和 `auctionEnd` 是 Unit)
structure TransitionOutput (α : Type) where
  newState : SimpleAuctionState
  events : List AuctionEvent
  -- 模拟外部转账 (如 .transfer())
  transfers : List (Address × Value)
  returnValue : α

-- 函数调用的结果，可以是 revert (Left) 或成功 (Right)
abbrev CallResult (α :Type) := Except AuctionError (TransitionOutput α)

-- 构造函数 (Constructor)
-- 在 Solidity 中，构造函数设置初始状态
def constructor (ctx : TxContext) (biddingTime : UInt) (beneficiaryAddress : Address) : SimpleAuctionState :=
  {
    beneficiary := beneficiaryAddress,
    auctionEndTime := ctx.timestamp + biddingTime,
    highestBidder := zeroAddress, -- 默认地址
    highestBid := 0,
    pendingReturns := fun _ => 0, -- 默认 mapping
    ended := false
  }

--
-- 函数: bid()
--
-- 修正: 移除 'noncomputable'
def bid (ctx : TxContext) (state : SimpleAuctionState) : Except AuctionError (TransitionOutput Unit) := do
  -- if (block.timestamp > auctionEndTime) revert AuctionAlreadyEnded();
  if ctx.timestamp > state.auctionEndTime then
    throw AuctionError.AuctionAlreadyEnded

  -- if (msg.value <= highestBid) revert BidNotHighEnough(highestBid);
  if ctx.value <= state.highestBid then
    throw (AuctionError.BidNotHighEnough state.highestBid)

  -- 状态变更
  let mut newState := state

  -- if (highestBid != 0) {
  --   pendingReturns[highestBidder] += highestBid;
  -- }
  if state.highestBid != 0 then
    newState := newState.addPendingReturns state.highestBidder state.highestBid

  -- highestBidder = msg.sender;
  -- highestBid = msg.value;
  newState := { newState with
    highestBidder := ctx.sender,
    highestBid := ctx.value
  }

  -- emit HighestBidIncreased(msg.sender, msg.value);
  let events := [AuctionEvent.HighestBidIncreased ctx.sender ctx.value]

  -- 返回成功的结果 (在 'do' 块中, 'return' 包装在 'Except.ok' 中)
  return {
    newState := newState,
    events := events,
    transfers := [], -- bid() 函数不产生 .transfer()
    returnValue := ()
  }

--
-- 函数: withdraw()
--
-- `withdraw` 在 `send` 失败时不 revert，而是返回 false。
-- 它的状态更新（将 pendingReturns 设为 0）是 *有条件的*。
-- 我们必须通过一个 "oracle" (`send_succeeds`) 来模拟 `send` 的外部结果。
--

-- `withdraw` 的输出是特殊的，因为它不 revert
structure WithdrawOutput where
  newState : SimpleAuctionState
  -- 记录尝试的转账及其金额
  attemptedTransfer : Option (Address × Value)
  returnValue : Bool

-- 修正: 移除 'noncomputable'
def withdraw (ctx : TxContext) (state : SimpleAuctionState)
             (send_succeeds : Bool) -- Oracle: 模拟 .send() 调用的返回值
             : WithdrawOutput :=

  let amount := state.pendingReturns ctx.sender

  if amount > 0 then
    -- 状态变更 1: pendingReturns[msg.sender] = 0;
    let state_after_zeroing := state.updatePendingReturns ctx.sender 0

    -- 模拟: if (!payable(msg.sender).send(amount))
    if !send_succeeds then
      -- 状态回滚: pendingReturns[msg.sender] = amount;
      let state_after_failure := state_after_zeroing.updatePendingReturns ctx.sender amount

      {
        newState := state_after_failure,
        attemptedTransfer := some (ctx.sender, amount),
        returnValue := false
      }
    else
      -- .send() 成功，状态变更 1 保持
      {
        newState := state_after_zeroing,
        attemptedTransfer := some (ctx.sender, amount),
        returnValue := true
      }
  else
    -- amount == 0
    {
      newState := state, -- 无状态变更
      attemptedTransfer := none,
      returnValue := true
    }

--
-- 函数: auctionEnd()
--
def auctionEnd (ctx : TxContext) (state : SimpleAuctionState) : Except AuctionError (TransitionOutput Unit) := do
  -- 1. 检查条件
  -- if (block.timestamp < auctionEndTime) revert AuctionNotYetEnded();
  if ctx.timestamp < state.auctionEndTime then
    throw AuctionError.AuctionNotYetEnded

  -- if (ended) revert AuctionEndAlreadyCalled();
  if state.ended then
    throw AuctionError.AuctionEndAlreadyCalled

  -- 2. 执行效果
  -- ended = true;
  let newState := { state with ended := true }

  -- emit AuctionEnded(highestBidder, highestBid);
  let events := [AuctionEvent.AuctionEnded state.highestBidder state.highestBid]

  -- 3. 外部交互
  -- beneficiary.transfer(highestBid);
  let transfers := [(state.beneficiary, state.highestBid)]

  return {
    newState := newState,
    events := events,
    transfers := transfers,
    returnValue := ()
  }

theorem beneficiary_is_immutable_bid :
  ∀ (ctx : TxContext) (state : SimpleAuctionState) (out : TransitionOutput Unit),
  bid ctx state = .ok out → out.newState.beneficiary = state.beneficiary :=
by
  intro ctx state out h_bid
  unfold bid at h_bid
  -- 简化 'do' 语句
  simp [bind, pure, Except.bind, Except.pure] at h_bid

  -- 手动处理 'if' 分支
  by_cases h_time : (ctx.timestamp > state.auctionEndTime)
  · -- 情况 1: 时间已过
    -- (修正 linter 警告: 移除 'if_pos')
    simp [h_time] at h_bid
  · -- 情况 2: 时间未过
    -- (修正 linter 警告: 移除 'if_neg')
    simp [h_time] at h_bid

    by_cases h_val : (ctx.value <= state.highestBid)
    · -- 情况 2a: 出价不够高
      -- (修正 linter 警告: 移除 'if_pos')
      simp [h_val] at h_bid
    · -- 情况 2b: 出价成功
      -- (修正 linter 警告: 移除 'if_neg')
      simp [h_val] at h_bid

      -- 'split' 策略是核心库的一部分, 用来处理 'if state.highestBid != 0'
      split at h_bid
      · -- 分支 1: state.highestBid != 0
        injection h_bid with h_eq
        -- 修正: 使用 '<-h_eq' (反向重写)
        rw [<-h_eq]
      · -- 分支 2: state.highestBid = 0
        injection h_bid with h_eq
        -- 修正: 使用 '<-h_eq' (反向重写)
        rw [<-h_eq]
        simp
        rfl

theorem beneficiary_is_immutable_withdraw :
  ∀ (ctx : TxContext) (state : SimpleAuctionState) (b : Bool),
  (withdraw ctx state b).newState.beneficiary = state.beneficiary :=
by
  intro ctx state b
  unfold withdraw
  simp [SimpleAuctionState.updatePendingReturns]

  -- 'split' 策略处理 'if amount > 0'
  split
  · -- 分支 1: amount > 0
    -- 'split' 策略处理 'if !send_succeeds' (即 'if b')
    -- 注意: 'b' 是 'send_succeeds', 所以 'if !b'
    split
    · -- 分支 1a: b = false (send 失败)
      rfl
    · -- 分支 1b: b = true (send 成功)
      rfl
  · -- 分支 2: amount = 0
    rfl

theorem successful_bid_increases_highestBid :
  ∀ (ctx : TxContext) (state : SimpleAuctionState) (out : TransitionOutput Unit),
  bid ctx state = .ok out → out.newState.highestBid > state.highestBid :=
by
  intro ctx state out h_bid
  unfold bid at h_bid
  simp [bind, pure, Except.bind, Except.pure] at h_bid

  -- 使用 by_cases
  by_cases h_time : (ctx.timestamp > state.auctionEndTime)
  · simp [h_time] at h_bid;
  · simp [h_time] at h_bid

    by_cases h_val : (ctx.value <= state.highestBid)
    · simp [h_val] at h_bid;
    · simp [h_val] at h_bid

      -- 'h_val' 是 '¬(ctx.value <= state.highestBid)'
      have h_gt := Nat.not_le.mp h_val

      -- 使用 'split' 处理 'if state.highestBid != 0'
      split at h_bid
      · -- 分支 1: highestBid != 0
        injection h_bid with h_eq
        -- 修正: 使用 '<-h_eq'
        rw [<-h_eq]
        exact h_gt
      · -- 分支 2: highestBid = 0
        injection h_bid with h_eq
        -- 修正: 使用 '<-h_eq'
        rw [<-h_eq]
        simp
        exact h_gt

theorem auctionEnd_fails_if_already_ended :
  ∀ (ctx : TxContext) (state : SimpleAuctionState),
  (ctx.timestamp >= state.auctionEndTime) → state.ended = true →
  auctionEnd ctx state = .error .AuctionEndAlreadyCalled :=
by
  intro ctx state h_time_ge h_ended
  unfold auctionEnd
  simp [bind, pure, Except.bind, Except.pure]

  -- 'h_time_ge' (>=) 是 'h_time_lt' (<) 的否定
  have h_time_not_lt : ¬(ctx.timestamp < state.auctionEndTime) :=
    Nat.not_lt_of_le h_time_ge

  -- 使用 'h_time_not_lt' 简化第一个 'if' (条件为 false)
  simp [h_time_not_lt]

  -- 使用 'h_ended' 简化第二个 'if' (条件为 true)
  simp [h_ended]

theorem successful_withdraw_clears_pendingReturns :
  ∀ (ctx : TxContext) (state : SimpleAuctionState),
  -- 前提 1: 用户的待退款 > 0
  state.pendingReturns ctx.sender > 0 →
  -- 我们假设 'send' 成功 ('send_succeeds = true')
  (withdraw ctx state true).newState.pendingReturns ctx.sender = 0 :=
by
  intro ctx state h_amount_gt
  unfold withdraw

  -- 'let amount := ...'
  -- 'if amount > 0 then ...'
  -- 'if_pos' 使用 'h_amount_gt' 证明第一个 'if' 为 true
  simp [if_pos h_amount_gt]

  -- 'let state_after_zeroing := ...'
  -- 'if !true then ... else ...'
  -- 'simp' 自动处理 '!true' 为 'false', 'if false ...'


  -- 目标变为 'state_after_zeroing.pendingReturns ctx.sender = 0'
  -- 展开 'state_after_zeroing' 的定义
  unfold SimpleAuctionState.updatePendingReturns

  -- 'pendingReturns' 的定义是 'fun a => if a == ctx.sender then 0 else ...'
  -- 当用 'ctx.sender' 调用它时, 'if' 条件为 true ('ctx.sender == ctx.sender')
  simp
