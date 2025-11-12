// SPDX-License-Identifier: MIT
pragma solidity ^0.8.10;

contract SimpleAuction {

    // --- FSM $C$ 变量 ---
    address public owner;
    uint public auctionEndTime;
    address public highestBidder;
    uint public highestBid;
    
    // FSM $S$ 状态变量: false = Bidding, true = Ended
    bool public ended;

    // --- FSM $O$ 事件 ---
    event AuctionEnded();
    event NewHighestBid(address bidder, uint amount);
    event Withdrawal(address owner, uint amount);

    constructor(uint _auctionEndTime) {
        owner = msg.sender;
        auctionEndTime = _auctionEndTime;
        ended = false; // s0 = Bidding
    }

    /**
     * @dev FSM 转换 1: bid()
     */
    function bid() external payable {
        // 守卫 G_1 (部分):
        require(block.timestamp < auctionEndTime, "Auction already ended");
        require(!ended, "Auction already ended"); // (这是 G_2 的反向)
        require(msg.value > highestBid, "Bid not high enough");
        
        // 动作 F_1:
        address oldBidder = highestBidder;
        uint oldBid = highestBid;

        highestBidder = msg.sender;
        highestBid = msg.value;

        emit NewHighestBid(msg.sender, msg.value);

        // 动作 F_1 (O.Send):
        if (oldBid > 0) {
            // 我们使用 .transfer() 来实现 O.Send
            payable(oldBidder).transfer(oldBid);
        }
    }

    /**
     * @dev FSM 转换 2: endAuction()
     */
    function endAuction() external {
        // 守卫 G_2:
        require(block.timestamp >= auctionEndTime, "Auction not yet ended");
        require(!ended, "Auction already ended");

        // 动作 F_2:
        ended = true;
        emit AuctionEnded();
    }

    /**
     * @dev FSM 转换 3: withdraw()
     */
    function withdraw() external {
        // 守卫 G_3 (部分):
        require(ended, "Auction not yet ended");
        require(msg.sender == owner, "Only owner can withdraw");
        
        uint amount = highestBid;
        
        // 动作 F_3:
        highestBid = 0;
        emit Withdrawal(owner, amount);

        // 动作 F_3 (O.Send):
        payable(owner).transfer(amount);
    }
}