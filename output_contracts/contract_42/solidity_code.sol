pragma solidity ^0.4.24;

contract XinXianToken {

    uint public ethFundDeposit = 0;
    uint public currentSupply = 0;
    uint public fundingStartBlock = 0;
    uint public tokenExchangeRate = 0;

    enum State { FundingNotStarted, FundingInProgress, FundingEnded }
    State public currentState;

    event AllocateToken(address indexed to, uint amount);
    event IssueToken(address indexed to, uint amount);
    event IncreaseSupply(uint amount);
    event Migrate(address indexed to, uint amount);

    modifier onlyInState(State _state) {
        require(currentState == _state);
        _;
    }

    constructor() public {
        currentState = State.FundingNotStarted;
    }

    function startFunding() public onlyInState(State.FundingNotStarted) {
        currentState = State.FundingInProgress;
        fundingStartBlock = block.number;
    }

    function stopFunding() public onlyInState(State.FundingInProgress) {
        currentState = State.FundingEnded;
    }

    function allocateToken(address _to, uint _amount) public onlyInState(State.FundingInProgress) {
        currentSupply += _amount;
        emit AllocateToken(_to, _amount);
        emit IncreaseSupply(_amount);
    }

    function migrate(address _to, uint _amount) public onlyInState(State.FundingEnded) {
        emit Migrate(_to, _amount);
        // Logic to transfer tokens to the new contract can be implemented here
    }
}