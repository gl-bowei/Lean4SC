// SPDX-License-Identifier: MIT
pragma solidity ^0.8.11;

contract MarketplaceContract {
    uint public platformFee = 5;
    uint public maxTokens = 100;

    mapping(address => uint) public saleOffers;
    mapping(address => uint) public bids;

    enum State { NFTOfferedForSale, NFTSold, BidEntered }
    State public currentState;

    event Offered(address indexed seller, uint tokenId);
    event BidEntered(address indexed bidder, uint amount);
    event Bought(address indexed buyer, uint tokenId);
    event RemovedFromSale(address indexed seller, uint tokenId);

    constructor() {
        currentState = State.NFTOfferedForSale;
    }

    function offerForSale(uint tokenId) public {
        require(currentState == State.NFTOfferedForSale, "NFT is not available for sale");
        saleOffers[msg.sender] = tokenId;
        currentState = State.NFTSold;
        emit Offered(msg.sender, tokenId);
    }

    function buy(uint tokenId) public payable {
        require(currentState == State.NFTSold, "NFT is not sold");
        require(msg.value >= saleOffers[msg.sender], "Insufficient funds");
        currentState = State.NFTOfferedForSale;
        emit Bought(msg.sender, tokenId);
        // Add logic to transfer NFT and handle payment
    }

    function enterBid(uint amount) public {
        require(currentState == State.NFTOfferedForSale, "Bids can only be placed when NFT is offered for sale");
        bids[msg.sender] = amount;
        currentState = State.BidEntered;
        emit BidEntered(msg.sender, amount);
    }

    function acceptBid(address bidder) public {
        require(currentState == State.BidEntered, "No bids to accept");
        uint bidAmount = bids[bidder];
        // Logic for accepting bid
        currentState = State.NFTSold;
        emit Bought(bidder, saleOffers[bidder]);
    }

    function withdraw() public {
        require(currentState == State.NFTOfferedForSale || currentState == State.BidEntered, "Cannot withdraw at this stage");
        // Logic to withdraw funds or bids
        emit RemovedFromSale(msg.sender, saleOffers[msg.sender]);
    }
}