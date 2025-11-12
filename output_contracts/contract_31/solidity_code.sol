pragma solidity ^0.4.23;

contract ERC20 {
    function transfer(address to, uint value) public returns (bool);
    function transferFrom(address from, address to, uint value) public returns (bool);
    function approve(address spender, uint value) public returns (bool);
}

contract TokenContract is ERC20 {
    mapping(address => uint) public balances;
    uint public totalSupply;
    mapping(address => mapping(address => uint)) public allowed;
    mapping(address => uint) public investmentPaths;

    enum State { Initialization, BuyingPhase, SellingPhase }
    State public currentState;

    event Transfer(address indexed from, address indexed to, uint value);
    event Approval(address indexed owner, address indexed spender, uint value);
    event Burned(address indexed from, uint value);

    constructor() public {
        currentState = State.Initialization;
    }

    function initializeTokenSupply() internal {
        // Initialize token supply logic
        totalSupply = 1000000; // Example total supply
        currentState = State.BuyingPhase;
    }

    function buy() public payable {
        require(currentState == State.BuyingPhase);
        require(msg.value > 0);

        uint tokenAmount = calculateTokenAmount(msg.value);
        balances[msg.sender] += tokenAmount;
        totalSupply += tokenAmount;
        emit Transfer(address(0), msg.sender, tokenAmount);
    }

    function calculateTokenAmount(uint investmentValue) internal view returns (uint) {
        // Logic to calculate the amount of tokens based on investment value
        return investmentValue * 100; // Example conversion rate
    }

    function sell(uint amount) public {
        require(currentState == State.SellingPhase);
        require(balances[msg.sender] >= amount);

        balances[msg.sender] -= amount;
        totalSupply -= amount;
        msg.sender.transfer(amount / 100); // Example conversion back to Ether
        emit Transfer(msg.sender, address(0), amount);
    }

    function transfer(address to, uint value) public returns (bool) {
        require(balances[msg.sender] >= value);
        balances[msg.sender] -= value;
        balances[to] += value;
        emit Transfer(msg.sender, to, value);
        return true;
    }

    function transferFrom(address from, address to, uint value) public returns (bool) {
        require(balances[from] >= value && allowed[from][msg.sender] >= value);
        balances[from] -= value;
        balances[to] += value;
        allowed[from][msg.sender] -= value;
        emit Transfer(from, to, value);
        return true;
    }

    function approve(address spender, uint value) public returns (bool) {
        allowed[msg.sender][spender] = value;
        emit Approval(msg.sender, spender, value);
        return true;
    }

    function mintToken(uint amount) internal {
        totalSupply += amount;
        balances[msg.sender] += amount;
        emit Transfer(address(0), msg.sender, amount);
    }

    function invest() public {
        // Investment logic would go here
    }

    function prepareForSelling() internal {
        currentState = State.SellingPhase;
    }

    function resetForBuying() internal {
        currentState = State.BuyingPhase;
    }
}