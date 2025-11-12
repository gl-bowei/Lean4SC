// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract TokenContract {
    string[] private _tokenURISuffixes;
    mapping(address => bool) private _usedSessions;
    uint private constant MAX_UNITS = 10000;
    uint private totalSupply;
    
    enum State { OpenForSale, MintingInProgress, SaleEnded }
    State public currentState;

    event Mint(address indexed to, uint tokenId);

    constructor() {
        currentState = State.OpenForSale;
    }

    modifier onlyInState(State state) {
        require(currentState == state, "Invalid state for this operation");
        _;
    }

    function startMinting() external onlyInState(State.OpenForSale) {
        currentState = State.MintingInProgress;
    }

    function mintTokens(address to, uint sessionId, bytes memory signature) external onlyInState(State.MintingInProgress) {
        require(verify(to, sessionId, signature), "Invalid signature or session");
        require(totalSupply < MAX_UNITS, "Max units exceeded");
        require(!_usedSessions[to], "Session already used");

        totalSupply++;
        _usedSessions[to] = true;

        emit Mint(to, totalSupply);
    }

    function endSale() external onlyInState(State.MintingInProgress) {
        currentState = State.SaleEnded;
    }

    function withdrawAll() external onlyInState(State.SaleEnded) {
        // Logic for withdrawing all tokens or balances
    }

    function verify(address to, uint sessionId, bytes memory signature) internal view returns (bool) {
        // Logic to verify the signature
        return true; // Replace with actual verification
    }
}