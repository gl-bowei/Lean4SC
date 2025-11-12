pragma solidity ^0.4.20;

contract CorexToken {
    // State variables
    uint public sellPrice;
    uint public buyPrice;
    mapping(address => bool) public frozenAccount;
    bool public canMint = false;

    // Events
    event FrozenFunds(address target, bool frozen);
    event Mint(address to, uint amount);
    event CanMint(bool canMint);

    // Modifier to check if account is frozen
    modifier notFrozen() {
        require(!frozenAccount[msg.sender]);
        _;
    }

    // Function to transfer tokens
    function transfer(address _to, uint _value) public notFrozen {
        // Transfer logic (to be defined)
    }

    // Function to freeze an account
    function freezeAccount(address _target) public {
        frozenAccount[_target] = true;
        emit FrozenFunds(_target, true);
    }

    // Function to unfreeze an account
    function unfreezeAccount(address _target) public {
        frozenAccount[_target] = false;
        emit FrozenFunds(_target, false);
    }

    // Function to mint new tokens
    function mint(address _to, uint _amount) public {
        require(canMint);
        // Minting logic (to be defined)
        emit Mint(_to, _amount);
    }

    // Function to change minting state
    function changeMintingState(bool _canMint) public {
        canMint = _canMint;
        emit CanMint(canMint);
    }
}