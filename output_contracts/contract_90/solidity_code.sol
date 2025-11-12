// SPDX-License-Identifier: MIT
pragma solidity ^0.5.17;

contract StarCoin {
    // State variables
    mapping(address => uint) private _balances;
    uint private _totalSupply;
    string private _name = "Star Coin";
    string private _symbol = "SRC";
    uint8 private _decimals = 18;

    // State enumeration
    enum State { Initialized, TransferringTokens, AllowanceManagement, TokenBurning }
    State public currentState;

    // Events
    event Transfer(address indexed from, address indexed to, uint value);
    event Approval(address indexed owner, address indexed spender, uint value);

    // Constructor
    constructor(uint totalSupply) public {
        _totalSupply = totalSupply;
        _balances[msg.sender] = totalSupply;
        currentState = State.TransferringTokens; // Transition to Transferring Tokens state
    }

    // Transfer tokens
    function transfer(address to, uint value) public {
        require(currentState == State.TransferringTokens, "Not in Transferring Tokens state");
        require(_balances[msg.sender] >= value, "Insufficient balance");
        _balances[msg.sender] -= value;
        _balances[to] += value;
        emit Transfer(msg.sender, to, value);
    }

    function approve(address spender, uint value) public returns (bool) {
        require(currentState == State.TransferringTokens, "Not in Transferring Tokens state");
        emit Approval(msg.sender, spender, value);
        return true;
    }

    function transferFrom(address from, address to, uint value) public {
        require(currentState == State.TransferringTokens, "Not in Transferring Tokens state");
        require(_balances[from] >= value, "Insufficient balance");
        _balances[from] -= value;
        _balances[to] += value;
        emit Transfer(from, to, value);
    }

    function increaseAllowance(address spender, uint addedValue) public {
        require(currentState == State.AllowanceManagement, "Not in Allowance Management state");
        emit Approval(msg.sender, spender, addedValue);
    }

    function decreaseAllowance(address spender, uint subtractedValue) public {
        require(currentState == State.AllowanceManagement, "Not in Allowance Management state");
        emit Approval(msg.sender, spender, subtractedValue);
    }

    function burn(uint value) public {
        require(currentState == State.TokenBurning, "Not in Token Burning state");
        require(_balances[msg.sender] >= value, "Insufficient balance to burn");
        _balances[msg.sender] -= value;
        _totalSupply -= value;
        emit Transfer(msg.sender, address(0), value);
        currentState = State.TransferringTokens; // Transition back to Transferring Tokens state
    }

    function balanceOf(address account) public view returns (uint) {
        return _balances[account];
    }
}