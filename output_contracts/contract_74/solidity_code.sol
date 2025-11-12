// SPDX-License-Identifier: MIT
pragma solidity ^0.8.12;

contract NFTMintingDistributionContract {

    address[] public receiverAddresses;
    address[] public team;
    uint public currentBatchMinted;
    mapping(address => uint) public balances;

    enum State { MintingPreparation, MintingActive, Distribution }
    State public currentState;

    event Minted(address indexed receiver);

    constructor(address[] memory _team) {
        team = _team;
        currentState = State.MintingPreparation;
    }

    modifier inState(State state) {
        require(currentState == state, "Invalid state for this action.");
        _;
    }

    function startMinting() external inState(State.MintingPreparation) {
        currentState = State.MintingActive;
    }

    function mintToken() external inState(State.MintingActive) {
        // Minting logic here
        receiverAddresses.push(msg.sender);
        balances[msg.sender] += 1;  // Assuming each mint gives 1 token
        currentBatchMinted += 1;

        emit Minted(msg.sender);
    }

    function finishMinting() external inState(State.MintingActive) {
        currentState = State.Distribution;
    }

    function distributeFunds() external inState(State.Distribution) {
        // Distribution logic here
        for (uint i = 0; i < team.length; i++) {
            // Assuming some logic to calculate and distribute funds
            balances[team[i]] += 10;  // Example distribution
        }
    }

    function sendFunds() external inState(State.Distribution) {
        // Logic to send funds to team members
    }

    function withdrawTeamMemberBalanceTo(address payable member) external inState(State.Distribution) {
        require(balances[member] > 0, "No balance to withdraw.");
        uint amount = balances[member];
        balances[member] = 0;
        member.transfer(amount);
    }
}