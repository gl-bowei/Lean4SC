// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract NFTMintingContract {
    uint public constant MAX_ELEMENTS = 1000;
    uint public constant PRICE = 0.05 ether;
    uint public SOLD_OUT_ELEMENTS = 0;
    string public baseTokenURI;

    enum SaleState { SaleOpen, SalePaused, SaleRevealed }
    SaleState public currentSaleState;

    address public owner;

    modifier onlyOwner() {
        require(msg.sender == owner, "Not the contract owner");
        _;
    }

    constructor(string memory _baseTokenURI) {
        owner = msg.sender;
        baseTokenURI = _baseTokenURI;
        currentSaleState = SaleState.SaleOpen;
    }

    event CreateAircoinsMetaverse();

    function mintTokens(uint256 quantity) public payable {
        require(currentSaleState == SaleState.SaleOpen, "Sale is not open");
        require(msg.value == quantity * PRICE, "Incorrect Ether amount");
        require(SOLD_OUT_ELEMENTS + quantity <= MAX_ELEMENTS, "Exceeds max elements");
        
        SOLD_OUT_ELEMENTS += quantity;
        emit CreateAircoinsMetaverse();
    }

    function presaleMint(uint256 quantity) public payable {
        // Implement presale minting logic
    }

    function setBaseURI(string memory _baseTokenURI) public onlyOwner {
        baseTokenURI = _baseTokenURI;
    }

    function withdrawAll() public onlyOwner {
        payable(owner).transfer(address(this).balance);
    }

    function pauseSale() public onlyOwner {
        currentSaleState = SaleState.SalePaused;
    }

    function resumeSale() public onlyOwner {
        currentSaleState = SaleState.SaleOpen;
    }

    function revealSale() public onlyOwner {
        require(currentSaleState == SaleState.SaleOpen, "Sale must be open to reveal");
        currentSaleState = SaleState.SaleRevealed;
    }
}