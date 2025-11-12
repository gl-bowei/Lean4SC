pragma solidity ^0.6.12;

contract BalanceManagementContract {
    address public devWallet;
    mapping(address => uint) public balances;
    mapping(address => address) public referrers;
    uint public devPercent = 10;
    uint public refPercent = 5;
    uint public govPercent = 15;
    
    enum State { Active, UpdatingBalance, Withdrawals }
    State public state = State.Active;

    event Deposit(address indexed user, uint amount);
    event Withdraw(address indexed user, uint amount);
    event ReferralAdded(address indexed referrer, address indexed user);
    event PriceChange(uint newPrice);

    modifier inState(State _state) {
        require(state == _state, "Invalid state");
        _;
    }

    function deposit() public payable inState(State.Active) {
        require(msg.value > 0, "Deposit must be greater than 0");
        balances[msg.sender] += msg.value;
        
        // Update referrer if applicable
        // (Assuming some logic here to set referrers)

        emit Deposit(msg.sender, msg.value);
        
        state = State.UpdatingBalance;
        updateBalance();
    }

    function withdraw() public inState(State.Active) {
        require(balances[msg.sender] > 0, "Insufficient funds");
        // Additional logic to ensure 1 hour has elapsed
        
        emit Withdraw(msg.sender, balances[msg.sender]);
        state = State.Withdrawals;
        finalizeWithdrawal();
    }

    function finalizeWithdrawal() internal {
        uint amount = balances[msg.sender];
        balances[msg.sender] = 0;
        msg.sender.transfer(amount); // Send funds back
        state = State.Active;
    }

    function updateBalance() public inState(State.UpdatingBalance) {
        // Logic to update balances, possibly using current time and price
        // Reset to Active state after updating
        state = State.Active;
    }

    function changePrice(uint newPrice) public {
        // Logic to change the pricing
        emit PriceChange(newPrice);
    }

    // Other functions for referral management, etc.

}