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

-----------------------------------
--- 性质证明
-----------------------------------

--- 1.唯一最高出价者胜出
--- 定理1.1 bid 函数的守卫确保了新的 highestBid 必须严格大于旧的 highestBid。
theorem bid_increases_bid (ctx : TxContext) (state : SimpleAuctionState) :
  ∀ (out : TransitionOutput Unit) (h : bid ctx state = .ok out),
  out.newState.highestBid > state.highestBid :=
by
  intro out h
  unfold bid at h
  simp [bind, pure, Except.bind, Except.pure] at h

  by_cases h_time : (ctx.timestamp > state.auctionEndTime)
  · simp [h_time] at h;
  · simp [h_time] at h

    by_cases h_val : (ctx.value <= state.highestBid)
    · simp [h_val] at h;
    · simp [h_val] at h

      have h_gt := Nat.not_le.mp h_val

      split at h
      · injection h with h_eq
        rw [<-h_eq]
        exact h_gt
      · injection h with h_eq
        rw [<-h_eq]
        simp
        exact h_gt

--- 1.2 auctionEnd 成功时，发出的事件 AuctionEnded 包含正确的 highestBidder 和 highestBid
theorem auctionEnd_reports_correct_winner (ctx : TxContext) (state : SimpleAuctionState) :
  ∀ (out : TransitionOutput Unit) (h : auctionEnd ctx state = .ok out),
  out.events = [AuctionEvent.AuctionEnded state.highestBidder state.highestBid] :=
by
  intro out h
  unfold auctionEnd at h
  simp [bind, pure, Except.bind, Except.pure] at h

-- 同样, 我们通过 'by_cases' 排除 'throw' 的路径
  by_cases h_time : ctx.timestamp < state.auctionEndTime
  . simp [h_time] at h -- 矛盾
  . simp [h_time] at h
    by_cases h_ended : state.ended
    . simp [h_ended] at h -- 矛盾
    . simp [h_ended] at h -- 成功路径

      -- 'h' 是: .ok { ..., events := [AuctionEvent.AuctionEnded ...], ... } = .ok out
      rw [←h]

--- 2.最高价款正确支付
--- 定理2.1 auctionEnd 函数成功时，它产生的 transfers 列表必须精确地包含 (state.beneficiary, state.highestBid)。
theorem auctionEnd_pays_beneficiary_correctly (ctx : TxContext) (state : SimpleAuctionState) :
  ∀ (out : TransitionOutput Unit) (h : auctionEnd ctx state = .ok out),
  out.transfers = [(state.beneficiary, state.highestBid)] ∧ out.newState.ended = true :=
by
  intro out h
  unfold auctionEnd at h
  simp [bind, pure, Except.bind, Except.pure] at h

  -- 排除 'throw' 路径 (与定理 1.2 相同)
  by_cases h_time : ctx.timestamp < state.auctionEndTime
  . simp [h_time] at h
  . simp [h_time] at h
    by_cases h_ended : state.ended
    . simp [h_ended] at h
    . simp [h_ended] at h

      -- 成功路径
      rw [←h]
      simp -- 自动证明 '... = ...' 和 'true = true'

--- 3. 竞拍期限结束：在 auctionEndTime 之前不能结束拍卖；在 auctionEndTime 之后不能出价。
--- 定理 3.1 (不得提早结束)： 如果 ctx.timestamp < state.auctionEndTime，auctionEnd 调用必须 revert
theorem auctionEnd_fails_if_early (ctx : TxContext) (state : SimpleAuctionState)
  (h_time : ctx.timestamp < state.auctionEndTime) :
  auctionEnd ctx state = .error AuctionError.AuctionNotYetEnded :=
by
  unfold auctionEnd
  -- 'simp' 会自动评估 'do' 块
  -- 它看到 'if ctx.timestamp < state.auctionEndTime' (h_time) 为 true
  -- 于是它执行 'throw AuctionError.AuctionNotYetEnded'
  simp [bind, Except.bind, h_time]

--- 定理 3.2 (不得迟出价)： 如果 ctx.timestamp > state.auctionEndTime，bid 调用必须 revert
theorem bid_fails_if_late (ctx : TxContext) (state : SimpleAuctionState)
  (h_time : ctx.timestamp > state.auctionEndTime) :
  bid ctx state = .error AuctionError.AuctionAlreadyEnded :=
by
  unfold bid
  -- 'simp' 评估 'do' 块
  -- 它看到 'if ctx.timestamp > state.auctionEndTime' (h_time) 为 true
  -- 于是它执行 'throw AuctionError.AuctionAlreadyEnded'
  simp [bind, Except.bind, h_time]

--- 4. 资金安全与退款 ：被超越的出价被正确记录为可提现余额 (pendingReturns)，并且 withdraw 函数允许提取该确切金额。
--- 4.1 正确记录退款 ：一次成功的 bid（且存在先前的 highestBidder）会正确地将旧的 highestBid 添加到旧的 highestBidder 的 pendingReturns 中
theorem bid_correctly_records_refund
  (ctx : TxContext)
  (state : SimpleAuctionState)
  (h_bid_exists : state.highestBid ≠ 0) :
  ∀ (out : TransitionOutput Unit), bid ctx state = .ok out →
    out.newState.pendingReturns state.highestBidder =
      state.pendingReturns state.highestBidder + state.highestBid :=
by
  intro out h
  unfold bid at h
  simp [bind, pure, Except.bind, Except.pure] at h

  -- 否定 auctionEndTime 超时的早期返回路径
  by_cases h_time : ctx.timestamp > state.auctionEndTime
  · simp [h_time] at h -- 出现 .error，矛盾
  · simp [h_time] at h

    -- 否定 bid 金额不高的早期返回路径
    by_cases h_val : ctx.value <= state.highestBid
    · simp [h_val] at h -- 出现 .error，矛盾
    · simp [h_val] at h

      -- 我们根据 `state.highestBid = 0` 分情况分析
      by_cases h_zero : state.highestBid = 0
      · contradiction -- 与 h_bid_exists: ≠ 0 矛盾
      · simp [h_zero] at h

        -- 现在我们进入的是 `addPendingReturns` 的 else 分支
        -- h 形如：.ok (... addPendingReturns state.highestBidder state.highestBid ...)
        -- 所以我们可以用 `rw` + `simp` 提取出 newState 的 pendingReturns

        rw [← h]
        simp [SimpleAuctionState.addPendingReturns]

-- 定理 4.2 (正确提取退款)：(已修正声明)
theorem withdraw_succeeds_and_zeros_balance
  (ctx : TxContext)
  (state : SimpleAuctionState)
  (send_succeeds : Bool) -- <-- 修正 1: 'send_succeeds' 必须作为参数传入
  (h_amount : state.pendingReturns ctx.sender > 0)
  (h_send : send_succeeds = true) : -- <-- 修正 2: 'h_send' 现在可以引用 'send_succeeds'

  let out := withdraw ctx state send_succeeds -- <-- 修正 3: 'let' 绑定现在可以找到 'send_succeeds'
  out.returnValue = true ∧
  out.newState.pendingReturns ctx.sender = 0 ∧
  out.attemptedTransfer = some (ctx.sender, state.pendingReturns ctx.sender)
:=
by
  -- 您的 'by' 块是正确的, 无需更改
  unfold withdraw
  simp [h_amount, h_send]
  unfold SimpleAuctionState.updatePendingReturns
  simp

--- Lean 定理 5 (重入无效)： 我们可以证明，如果一个用户的 pendingReturns 已经是 0（就像在重入调用中那样），withdraw 函数不会执行任何状态更改，也不会尝试任何转账
theorem withdraw_is_safe_against_reentrancy (ctx : TxContext) (state : SimpleAuctionState) (send_succeeds : Bool)
  (h_amount_zero : state.pendingReturns ctx.sender = 0) :
  let out := withdraw ctx state send_succeeds
  out.returnValue = true ∧
  out.attemptedTransfer = none ∧
  out.newState = state :=
by
  -- 'h_amount_zero' 意味着 'let amount := 0'
  unfold withdraw
  simp [h_amount_zero]
