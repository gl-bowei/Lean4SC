// SPDX-License-Identifier: MIT
pragma solidity ^0.6.12;

contract TokenSaleContract {
    uint public tokenSaleHardCap = 10000;
    uint public baseRate = 1;
    mapping(address => bool) public frozenAccount;

    enum State { TokenSaleOpen, TokenSaleInProgress, TokenSaleClosed }
    State public currentState;

    event FrozenFunds(address target, bool frozen);
    event Transfer(address from, address to, uint value);

    constructor() public {
        currentState = State.TokenSaleOpen;
    }

    modifier inState(State _state) {
        require(currentState == _state, "Invalid state");
        _;
    }

    function actionStartSale() external inState(State.TokenSaleOpen) {
        currentState = State.TokenSaleInProgress;
    }

    function actionPurchaseTokens(uint _amount) external inState(State.TokenSaleInProgress) {
        // Logic for purchasing tokens
        emit Transfer(msg.sender, address(this), _amount);
    }

    function actionMintToken(uint _amount) external inState(State.TokenSaleInProgress) {
        // Logic for minting tokens
    }

    function actionFreezeAccount(address _account) external inState(State.TokenSaleInProgress) {
        frozenAccount[_account] = true;
        emit FrozenFunds(_account, true);
    }

    function actionCloseSale() external inState(State.TokenSaleInProgress) {
        // Implement logic to check if hard cap is reached
        currentState = State.TokenSaleClosed;
    }

    function actionClaimFunds() external inState(State.TokenSaleClosed) {
        // Logic for claiming funds
    }
}