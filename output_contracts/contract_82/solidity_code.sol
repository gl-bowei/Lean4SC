// SPDX-License-Identifier: MIT
pragma solidity ^0.6.2;

contract RetroactiveClaimsContract {
    uint public xtk;
    mapping(address => bool) public eligibleRecipients;
    uint public totalXtkDistribution;
    mapping(address => uint) public distributionUnitsMapping;

    enum State { RecipientsListOpen, RecipientsListLocked, ClaimsProcessed }
    State public currentState;

    event EligibleRecipientsAdded(address indexed recipient);
    event Claimed(address indexed recipient);

    modifier onlyInState(State expectedState) {
        require(currentState == expectedState, "Invalid state transition.");
        _;
    }

    constructor() public {
        currentState = State.RecipientsListOpen;
    }

    function addEligibleRecipients(address recipient) public onlyInState(State.RecipientsListOpen) {
        eligibleRecipients[recipient] = true;
        emit EligibleRecipientsAdded(recipient);
    }

    function lockRecipientsList() public onlyInState(State.RecipientsListOpen) {
        currentState = State.RecipientsListLocked;
    }

    function claim() public onlyInState(State.RecipientsListLocked) {
        require(eligibleRecipients[msg.sender], "Not eligible to claim.");
        // Logic for distributing tokens
        emit Claimed(msg.sender);
        currentState = State.ClaimsProcessed; // Move to ClaimsProcessed state after claiming
    }

    function retrieveToken() public onlyInState(State.ClaimsProcessed) {
        // Logic for token retrieval
    }
}