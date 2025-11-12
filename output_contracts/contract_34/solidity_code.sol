// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract TeamVestingContract {
    uint public totalAllocatedAmount;
    uint public initialTimestamp;

    mapping(address => uint) public investorsInfo;

    enum State { Initialized, InvestorsAdded, TokensReleased }
    State public currentState;

    event InvestorsAdded(address investor, uint amount);
    event TokensWithdrawn(address investor, uint amount);
    event TokenRecovery(address token, uint amount);

    constructor() {
        currentState = State.Initialized;
    }

    modifier onlyInitialized() {
        require(currentState == State.Initialized, "Contract is not initialized.");
        _;
    }

    modifier onlyInvestorsAdded() {
        require(currentState == State.InvestorsAdded, "Investors not added.");
        _;
    }

    modifier onlyTokensReleased() {
        require(currentState == State.TokensReleased, "Tokens not released.");
        _;
    }

    function setInitialTimestamp() external onlyInitialized {
        initialTimestamp = block.timestamp;
        currentState = State.InvestorsAdded;
    }

    function addInvestors(address investor, uint amount) external onlyInvestorsAdded {
        investorsInfo[investor] += amount;
        totalAllocatedAmount += amount;
        emit InvestorsAdded(investor, amount);
    }

    function releaseTokens() external onlyInvestorsAdded {
        // Add logic to release tokens based on vesting schedule
        currentState = State.TokensReleased;
    }

    function allowWithdraw(uint amount) external onlyTokensReleased {
        require(investorsInfo[msg.sender] >= amount, "Insufficient tokens.");
        investorsInfo[msg.sender] -= amount;
        // Transfer tokens logic here
        emit TokensWithdrawn(msg.sender, amount);
    }

    function recoverTokens(address token, uint amount) external {
        // Logic for recovering tokens
        emit TokenRecovery(token, amount);
    }
}