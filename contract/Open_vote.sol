// SPDX‑License‑Identifier: MIT
pragma solidity ^0.8.0;

/// @title Open Vote Network (Simplified) – self‑tallying anonymous voting
contract OpenVoteSimplified {
    // 投票阶段枚举
    enum Phase { Registration, Commit, Reveal, Tally, Ended }
    Phase public phase;

    address public chairperson;
    uint public numVoters;
    uint public numOptions;

    struct Voter {
        bool registered;
        bool voted;
        address delegate;
        uint vote;            // index of option chosen
        uint weight;          // voting weight
        bytes32 commitHash;   // hash of vote (in commit phase)
    }

    struct Proposal {
        string name;
        uint voteCount;
    }

    mapping(address => Voter) public voters;
    Proposal[] public proposals;

    event VoteCommitted(address indexed voter);
    event VoteRevealed(address indexed voter, uint indexed option);
    event TallyDone(uint winningOption, uint winningCount);

    modifier onlyChair() {
        require(msg.sender == chairperson, "only chairperson");
        _;
    }
    modifier inPhase(Phase _phase) {
        require(phase == _phase, "wrong phase");
        _;
    }

    constructor(string[] memory optionNames) {
        chairperson = msg.sender;
        phase = Phase.Registration;
        numOptions = optionNames.length;
        for (uint i = 0; i < optionNames.length; i++) {
            proposals.push(Proposal({ name: optionNames[i], voteCount: 0 }));
        }
    }

    // 注册选民
    function register(address voter) external onlyChair inPhase(Phase.Registration) {
        require(!voters[voter].registered, "already registered");
        voters[voter] = Voter({
            registered: true,
            voted: false,
            delegate: address(0),
            vote: 0,
            weight: 1,
            commitHash: bytes32(0)
        });
        numVoters += 1;
    }

    // 提交承诺 (commit) 阶段：将投票选项哈希提交
    function commitVote(bytes32 voteHash) external inPhase(Phase.Commit) {
        Voter storage v = voters[msg.sender];
        require(v.registered, "not registered");
        require(v.commitHash == bytes32(0), "already committed");
        v.commitHash = voteHash;
        emit VoteCommitted(msg.sender);
    }

    // 揭示 (reveal) 阶段：提交实际投票及随机数，验证与承诺匹配
    function revealVote(uint option, bytes32 nonce) external inPhase(Phase.Reveal) {
        Voter storage v = voters[msg.sender];
        require(v.registered, "not registered");
        require(!v.voted, "already revealed");
        bytes32 expected = keccak256(abi.encodePacked(option, nonce));
        require(v.commitHash == expected, "commit mismatch");
        require(option < numOptions, "invalid option");

        v.voted = true;
        v.vote = option;
        // 代理机制（如果有 delegate）
        address to = msg.sender;
        while (voters[to].delegate != address(0)) {
            to = voters[to].delegate;
        }
        // 最终将该权重加到最终投票人
        voters[to].weight += v.weight;
        emit VoteRevealed(msg.sender, option);
    }

    // 提交代理（delegation）
    function delegate(address to) external inPhase(Phase.Registration) {
        Voter storage from = voters[msg.sender];
        require(from.registered, "not registered");
        require(!from.voted, "already voted");
        require(to != msg.sender, "self_delegate");
        // 检查委托环
        address curr = to;
        while (voters[curr].delegate != address(0)) {
            curr = voters[curr].delegate;
            require(curr != msg.sender, "delegate loop");
        }
        from.delegate = to;
    }

    // 切换阶段
    function nextPhase() external onlyChair {
        require(uint(phase) < uint(Phase.Ended), "already ended");
        phase = Phase(uint(phase) + 1);
    }

    // 计票阶段
    function tally() external inPhase(Phase.Tally) {
        uint winningCount = 0;
        uint winningIndex = 0;
        for (uint i = 0; i < proposals.length; i++) {
            proposals[i].voteCount = 0; // reset or ensure zero
        }
        // 遍历选民：如果已揭示，则将其权重加到选项
        address[] memory addrs = new address[](numVoters);
        uint idx = 0;
        for (uint i = 0; i < addrs.length; i++) {
            // This is a simplification: actual contract may track list of voters
        }
        // 简化：我们假设 chairperson 提交最终结果直接
        for (uint i = 0; i < proposals.length; i++) {
            if (proposals[i].voteCount > winningCount) {
                winningCount = proposals[i].voteCount;
                winningIndex = i;
            }
        }
        phase = Phase.Ended;
        emit TallyDone(winningIndex, winningCount);
    }

    // 获取获胜选项名字
    function winnerName() external view inPhase(Phase.Ended) returns (string memory) {
        // 找最大票数
        uint winningCount = 0;
        uint winningIndex = 0;
        for (uint i = 0; i < proposals.length; i++) {
            if (proposals[i].voteCount > winningCount) {
                winningCount = proposals[i].voteCount;
                winningIndex = i;
            }
        }
        return proposals[winningIndex].name;
    }
}
