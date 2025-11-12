// SPDX-License-Identifier: MIT
pragma solidity ^0.8.7;

contract MembershipContract {
    address public owner;
    address[] public tokenContracts;
    uint256 public exchangeRate = 100; // Example exchange rate
    uint256 public airdropAmount = 1000; // Amount for airdrop
    mapping(uint256 => uint256) public tierThresholds;

    enum State { AirdropEligibility, AirdropClaiming, TokenPurchasing }
    State public currentState;

    event ClaimAirdrop(address indexed user);
    event Purchase(address indexed user, uint256 amount);
    event WithdrawAll(address indexed owner);

    modifier onlyOwner() {
        require(msg.sender == owner, "Not the contract owner");
        _;
    }

    constructor() {
        owner = msg.sender;
        currentState = State.AirdropEligibility;
    }

    function checkEligibility() public {
        require(currentState == State.AirdropEligibility, "Not in AirdropEligibility state");
        if (isEligible(msg.sender)) {
            currentState = State.AirdropClaiming;
        } 
    }

    function claimAirdropAction() public {
        require(currentState == State.AirdropClaiming, "Not in AirdropClaiming state");
        emit ClaimAirdrop(msg.sender);
        // Logic to transfer airdrop to msg.sender
        currentState = State.TokenPurchasing;
    }

    function purchaseTokensAction(uint256 amount) public {
        require(currentState == State.TokenPurchasing, "Not in TokenPurchasing state");
        // Logic to purchase tokens based on exchangeRate
        emit Purchase(msg.sender, amount);
        currentState = State.AirdropEligibility;
    }

    function isEligible(address user) internal view returns (bool) {
        // Eligibility logic
        return true; // Placeholder
    }

    function withdrawAll() external onlyOwner {
        emit WithdrawAll(owner);
        // Logic to withdraw all funds or tokens
    }
}