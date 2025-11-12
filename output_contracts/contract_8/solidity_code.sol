// SPDX-License-Identifier: MIT
pragma solidity ^0.8.2;

contract AllocationPoolContract {
    struct PoolInfo {
        // Define the structure for pool information
    }

    struct UserInfo {
        // Define the structure for user information
    }

    address public allocRewardToken;
    mapping(uint => PoolInfo) public allocPoolInfo;
    mapping(address => UserInfo) public allocUserInfo;

    enum State { PoolCreation, ActivePool, PendingWithdrawal }
    State public currentState;

    event AllocPoolCreated();
    event AllocDeposit(address indexed user, uint amount);
    event AllocWithdraw(address indexed user, uint amount);

    constructor(address _allocRewardToken) {
        allocRewardToken = _allocRewardToken;
        currentState = State.PoolCreation;
    }

    function allocAddPool() external {
        require(currentState == State.PoolCreation, "Must be in PoolCreation state");
        // Logic for adding a new pool
        emit AllocPoolCreated();
        currentState = State.ActivePool;
    }

    function allocDeposit(uint amount) external {
        require(currentState == State.ActivePool, "Must be in ActivePool state");
        // Logic for depositing LP tokens
        emit AllocDeposit(msg.sender, amount);
    }

    function allocWithdraw(uint amount) external {
        require(currentState == State.ActivePool, "Must be in ActivePool state");
        // Logic for withdrawing tokens into PendingWithdrawal state
        emit AllocWithdraw(msg.sender, amount);
        currentState = State.PendingWithdrawal;
    }

    function allocClaimReward() external {
        require(currentState == State.PendingWithdrawal, "Must be in PendingWithdrawal state");
        // Logic for clearing rewards after time elapsed
        currentState = State.ActivePool; // Return to ActivePool after claiming
    }
}