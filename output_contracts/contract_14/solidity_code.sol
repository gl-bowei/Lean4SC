pragma solidity ^0.4.24;

contract LiquidityListingContract {
    string public name;
    bool public enable;
    uint public count;
    mapping(address => uint) public receivedEther;

    event Received(address indexed contributor, uint amount);
    event Withdraw(address indexed withdrawer, uint amount);

    modifier onlyActive() {
        require(enable, "Contract is not active.");
        _;
    }

    function LiquidityListingContract(string _name) public {
        name = _name;
        enable = true;
        count = 0;
    }

    function() public payable onlyActive {
        receivedEther[msg.sender] += msg.value;
        count++;
        emit Received(msg.sender, msg.value);
    }

    function removeLiquidity(uint amount) public onlyActive {
        require(receivedEther[msg.sender] >= amount, "Insufficient balance.");
        receivedEther[msg.sender] -= amount;
        msg.sender.transfer(amount);
        emit Withdraw(msg.sender, amount);
    }

    function stopContract() public onlyActive {
        enable = false;
    }

    function startContract() public {
        enable = true;
    }

    function claimTokens() public {
        // Token claiming logic would go here.
    }
}