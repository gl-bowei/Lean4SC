-- SPDX-License-Identifier: GPL-3.0
-- 形式化 Lean 4 代码

import Init.Data.Nat.Basic
import Init.Data.String
import Init.Data.Array
import Init.Data.Array.Basic -- 修正 3: 导入 'foldlIdx'

-- 定义 Solidity 基础类型
abbrev UInt := Nat

structure Address where
  val : String
deriving DecidableEq, Inhabited, Repr

def zeroAddress : Address := ⟨""⟩

-- 模拟 bytes32
structure Bytes32 where
  val : String -- 使用 String 作为简单模拟
deriving DecidableEq, Inhabited, Repr

-- 合约的 `struct` 定义
structure Voter where
  weight : UInt := 0
  voted : Bool := false
  delegate : Address := zeroAddress
  vote : UInt := 0
deriving Inhabited

structure Proposal where
  name : Bytes32
  voteCount : UInt := 0
deriving Inhabited

-- 合约的完整状态
structure BallotState where
  chairperson : Address
  voters : Address → Voter
  proposals : Array Proposal

-- 交易上下文 (模拟 msg.sender)
structure TxContext where
  sender : Address

-- 合约自定义错误
inductive BallotError where
  | NotChairperson
  | AlreadyVoted
  | WeightNotZero -- giveRightToVote: require(voters[voter].weight == 0)
  | NoRightToVote
  | SelfDelegation
  | DelegationLoop
  | CannotDelegateToNonVoter
  | ProposalOutOfBounds
deriving DecidableEq, Repr

-- 成功执行的输出
structure TransitionOutput (α : Type) where
  newState : BallotState
  returnValue : α

-- 函数调用的结果，可以是 revert (Left) 或成功 (Right)
abbrev CallResult (α : Type) := Except BallotError (TransitionOutput α)

--
-- 状态更新的辅助函数
--

-- 辅助函数：更新 mapping (voters[addr] = voter)
def BallotState.updateVoter (s : BallotState) (addr : Address) (v : Voter) : BallotState :=
  { s with voters := fun a => if a == addr then v else s.voters a }

-- 辅助函数：更新数组 (proposals[idx] = proposal)
-- 修正 4: 使用 'set!' 来处理 Lean 的类型安全
def BallotState.updateProposal (s : BallotState) (idx : UInt) (p : Proposal) : BallotState :=
  { s with proposals := s.proposals.set! idx p }

--
-- 合约函数
--

-- 构造函数 (Constructor)
def constructor (ctx : TxContext) (proposalNames : Array Bytes32) : BallotState :=
  -- 修正 5: 添加显式类型 ': Proposal'
  let initialProposals := proposalNames.map (fun name => ({ name := name, voteCount := 0 } : Proposal))
  -- 修正 5: 添加显式类型 ': BallotState'
  let initialState : BallotState := {
    chairperson := ctx.sender,
    voters := fun _ => default,
    proposals := initialProposals
  }
  -- 设置主席的权重
  initialState.updateVoter ctx.sender { weight := 1 }

--
-- 函数: giveRightToVote(address voter)
--
-- 修正 1: 更改返回类型以修复 'do' 标记
def giveRightToVote (ctx : TxContext) (state : BallotState) (voterAddr : Address) : Except BallotError (TransitionOutput Unit) := do
  let voter := state.voters voterAddr

  -- require(msg.sender == chairperson)
  if ctx.sender != state.chairperson then throw .NotChairperson

  -- require(!voters[voter].voted)
  if voter.voted then throw .AlreadyVoted

  -- require(voters[voter].weight == 0)
  if voter.weight != 0 then throw .WeightNotZero

  -- 效果: voters[voter].weight = 1;
  let newVoter := { voter with weight := 1 }
  let newState := state.updateVoter voterAddr newVoter

  return { newState := newState, returnValue := () }

--
-- 函数: delegate(address to)
--

-- `delegate` 中 `while` 循环的纯函数递归模拟
partial def findDelegationTarget
  (to : Address)
  (sender : Address)
  (voters : Address → Voter)
  (fuel : Nat)
  : Except BallotError Address :=
  match fuel with
  | 0 => .error .DelegationLoop
  | fuel' + 1 =>
    let currentTargetVoter := voters to
    if currentTargetVoter.delegate == zeroAddress then
      .ok to
    else
      let nextTo := currentTargetVoter.delegate
      if nextTo == sender then
        .error .DelegationLoop
      else
        findDelegationTarget nextTo sender voters fuel'

-- 修正 1: 更改返回类型以修复 'do' 标记
def delegate (ctx : TxContext) (state : BallotState) (to : Address)
             (fuel : Nat := 1000)
             : Except BallotError (TransitionOutput Unit) := do
  let sender := state.voters ctx.sender

  if sender.weight == 0 then throw .NoRightToVote
  if sender.voted then throw .AlreadyVoted
  if to == ctx.sender then throw .SelfDelegation

  let finalDelegateAddress ←
    match findDelegationTarget to ctx.sender state.voters fuel with
    | .ok addr => pure addr
    | .error e => throw e

  let delegate_ := state.voters finalDelegateAddress

  if delegate_.weight < 1 then throw .CannotDelegateToNonVoter

  let newSender := { sender with voted := true, delegate := finalDelegateAddress }
  let mut newState := state.updateVoter ctx.sender newSender

  if delegate_.voted then
    let proposalIndex := delegate_.vote
    -- 修正 2: 使用 'state.proposals[idx]?' 语法
    match newState.proposals[proposalIndex]? with
    | none => throw .ProposalOutOfBounds
    | some p =>
      let newProposal := { p with voteCount := p.voteCount + sender.weight }
      newState := newState.updateProposal proposalIndex newProposal
  else
    let newDelegate := { delegate_ with weight := delegate_.weight + sender.weight }
    newState := newState.updateVoter finalDelegateAddress newDelegate

  return { newState := newState, returnValue := () }

--
-- 函数: vote(uint proposal)
--
-- 修正 1: 更改返回类型以修复 'do' 标记
def vote (ctx : TxContext) (state : BallotState) (proposalIndex : UInt) : Except BallotError (TransitionOutput Unit) := do
  let sender := state.voters ctx.sender

  if sender.weight == 0 then throw .NoRightToVote
  if sender.voted then throw .AlreadyVoted

  let newSender := { sender with voted := true, vote := proposalIndex }
  let mut newState := state.updateVoter ctx.sender newSender

  -- 修正 2: 使用 'state.proposals[idx]?' 语法
  match state.proposals[proposalIndex]? with
  | none => throw .ProposalOutOfBounds
  | some proposalToUpdate =>
    let newProposal := { proposalToUpdate with voteCount := proposalToUpdate.voteCount + sender.weight }
    newState := newState.updateProposal proposalIndex newProposal

    return { newState := newState, returnValue := () }

--
-- 函数: winningProposal() (view)
--
def winningProposal (state : BallotState) : UInt :=
  let proposals := state.proposals

  -- 我们定义一个 'let rec' (递归) 辅助函数来充当循环
  let rec findWinnerLoop (i : Nat) (acc : UInt × UInt) : UInt × UInt :=

    -- 'h : i < proposals.size' 是我们检查是否越界的条件
    if h : i < proposals.size then
      -- 使用现代索引语法
      let proposal := proposals[i]
      let (currentWinningVoteCount, _) := acc

      -- 检查当前提案是否是新的获胜者
      if proposal.voteCount > currentWinningVoteCount then
        -- 是新获胜者: 递归调用, 'i' 增加 1, 'acc' 更新为新获胜者
        findWinnerLoop (i + 1) (proposal.voteCount, i)
      else
        -- 不是新获胜者: 递归调用, 'i' 增加 1, 'acc' 保持不变
        findWinnerLoop (i + 1) acc
    else
      -- 基础情况: 'i' 不再小于 size, 我们已经遍历完数组
      -- 返回我们找到的 'acc'
      acc

  -- 修正: 'termination_by' 必须位于 'let rec' 函数体的 *末尾*
  termination_by proposals.size - i

  -- 'let rec' 定义了函数, 现在我们调用它来启动循环
  -- 从索引 0 开始, 初始获胜者是 (0 票, 索引 0)
  let (winningVoteCount, winningProposal_) := findWinnerLoop 0 (0, 0)

  winningProposal_
--
-- 函数: winnerName() (view)
--
def winnerName (state : BallotState) : Option Bytes32 :=
  let winningIndex := winningProposal state
  -- 修正 2: 使用 'state.proposals[idx]?' 语法
  -- 附带简化了 'Option.map' 的写法
  (state.proposals[winningIndex]?).map (·.name)
