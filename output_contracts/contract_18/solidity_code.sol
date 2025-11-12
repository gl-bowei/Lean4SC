// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract OnChainBotsNFT {
    uint public maxSupply = 10000;
    uint public numClaimed = 0;
    uint public mintPrice = 0.05 ether;
    bool public saleIsActive = false;
    address public owner;

    event PaymentReleased(address to, uint amount);

    modifier onlyOwner() {
        require(msg.sender == owner, "Not the contract owner");
        _;
    }

    constructor() {
        owner = msg.sender;
    }

    function flipSaleState() public onlyOwner {
        saleIsActive = !saleIsActive;
    }

    function claim(uint quantity) public payable {
        require(saleIsActive, "Sale is not active");
        require(quantity > 0 && quantity <= 10, "Can only mint 1 to 10 NFTs at a time");
        require(numClaimed + quantity <= maxSupply, "Max supply reached");
        require(msg.value == mintPrice * quantity, "Incorrect Ether value");

        numClaimed += quantity;

        emit PaymentReleased(msg.sender, msg.value);
    }

    function ownerClaim() public onlyOwner {
        // Logic for owner to mint special NFTs
    }

    function setMintPrice(uint newPrice) public onlyOwner {
        mintPrice = newPrice;
    }

    function withdraw() public onlyOwner {
        payable(owner).transfer(address(this).balance);
    }
}