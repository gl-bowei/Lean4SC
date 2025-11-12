// SPDX-License-Identifier: MIT
pragma solidity ^0.6.4;

contract StakingContract {
    // Events
    event StakeStarted(address indexed user, uint amount, uint duration);
    event StakeEnded(address indexed user, uint totalAmount);

    // Variables
    mapping(address => uint) public userInfo;
    mapping(address => StakeInfo) public stakeInfo;
    uint public fees;

    struct StakeInfo {
        uint amount;
        uint duration;
    }

    enum State { StakingActive, StakingEnded, StakeWithdrawn }
    State public currentState;

    constructor() public {
        currentState = State.StakingActive;
    }

    modifier inState(State _state) {
        require(currentState == _state, "Invalid state for this operation");
        _;
    }

    function stakeHex(uint _amount, uint _duration) external inState(State.StakingActive) {
        // Assume a function to transfer HEX tokens to this contract
        // transferFrom(msg.sender, address(this), _amount);

        userInfo[msg.sender] += _amount;
        stakeInfo[msg.sender] = StakeInfo(_amount, _duration);
        currentState = State.StakingEnded;

        emit StakeStarted(msg.sender, _amount, _duration);
    }

    function endStake() external inState(State.StakingEnded) {
        StakeInfo memory stake = stakeInfo[msg.sender];
        require(stake.amount > 0, "No stake found");

        uint totalAmount = stake.amount; // This can include interest logic

        // Resetting the user's stake info
        delete stakeInfo[msg.sender];
        currentState = State.StakeWithdrawn;

        // Transfer totalAmount back to user
        // transfer(msg.sender, totalAmount);

        emit StakeEnded(msg.sender, totalAmount);
    }

    // Additional functions to update user and stake data can be implemented here
    function updateUserData(address _user, uint _amount) external {
        // Logic to update user data
    }

    function updateStakeData(address _user, uint _amount, uint _duration) external {
        // Logic to update stake data
    }
}