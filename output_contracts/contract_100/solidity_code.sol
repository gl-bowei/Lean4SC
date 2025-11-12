pragma solidity ^0.5.1;

contract WithdrawalContract {
    address public token_contract;
    address public admin;
    uint public withdraw_value;
    bool public capReached;
    mapping(address => bool) public withdrawed_addresses;
    mapping(address => bool) public whitelistedAddresses;
    
    enum State { WhitelistingPhase, WithdrawalPhase, CapReached }
    State public currentState;

    event AddressWhitelisted(address indexed addr);
    event WithdrawalSuccessful(address indexed addr, uint amount);
    event CapStatusUpdated(bool status);

    constructor(address _token_contract, address _admin) public {
        token_contract = _token_contract;
        admin = _admin;
        currentState = State.WhitelistingPhase;
    }

    modifier onlyAdmin() {
        require(msg.sender == admin, "Only admin can perform this action");
        _;
    }
    
    modifier inWhitelistingPhase() {
        require(currentState == State.WhitelistingPhase, "Not in Whitelisting Phase");
        _;
    }
    
    modifier inWithdrawalPhase() {
        require(currentState == State.WithdrawalPhase, "Not in Withdrawal Phase");
        _;
    }

    function manageWhitelist(address addr, bool status) public onlyAdmin inWhitelistingPhase {
        whitelistedAddresses[addr] = status;
        emit AddressWhitelisted(addr);
    }

    function enableWithdrawals() public onlyAdmin inWhitelistingPhase {
        currentState = State.WithdrawalPhase;
    }

    function processWithdrawal(uint amount) public inWithdrawalPhase {
        require(whitelistedAddresses[msg.sender], "Address not whitelisted");
        require(!withdrawed_addresses[msg.sender], "Address already withdrawn");
        require(amount <= withdraw_value, "Amount exceeds withdraw value");

        // Simulating token transfer...
        // IERC20(token_contract).transfer(msg.sender, amount);

        withdrawed_addresses[msg.sender] = true;
        emit WithdrawalSuccessful(msg.sender, amount);
        
        // Check if cap reached (simple example)
        checkCap();
    }

    function checkCap() internal {
        // Dummy condition for cap reached
        if (/* condition that checks if the cap is reached */) {
            capReached = true;
            currentState = State.CapReached;
            emit CapStatusUpdated(capReached);
        }
    }

    function denyWithdrawal() public {
        require(currentState == State.CapReached, "Cap not reached");
        // Logic to prevent further withdrawals...
    }
}