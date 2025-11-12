// SPDX-License-Identifier: MIT
pragma solidity ^0.8.9;

contract TokenContract {
    uint public constant MAX_SUPPLY = 10000;
    uint public constant PRICE = 0.05 ether;
    uint public constant MAX_PER_MINT = 5;

    string public baseTokenURI;
    bool public isActive;
    uint public royalties;
    address public beneficiary;

    enum State { MintingInactive, MintingActive }
    State public currentState;

    event NFTMinted(address indexed minter, uint quantity);
    event RoyaltyUpdated(uint newRoyalties);

    modifier onlyWhenActive() {
        require(currentState == State.MintingActive, "Minting is not active");
        _;
    }

    constructor() {
        currentState = State.MintingInactive;
        beneficiary = msg.sender;
    }

    function setMintingActive() external {
        require(currentState == State.MintingInactive, "Already active");
        currentState = State.MintingActive;
    }

    function setMintingInactive() external {
        require(currentState == State.MintingActive, "Already inactive");
        currentState = State.MintingInactive;
    }

    function processMinting(uint quantity) external payable onlyWhenActive {
        require(quantity <= MAX_PER_MINT, "Exceeds max per mint");
        require(msg.value == quantity * PRICE, "Incorrect payment");
        // Logic to mint NFTs, increase supply, etc.
        emit NFTMinted(msg.sender, quantity);
    }

    function reserveNFT() external {
        // Logic to reserve NFTs for the contract owner
    }

    function updateBaseURI(string memory newBaseURI) external {
        baseTokenURI = newBaseURI;
    }

    function updateRoyalties(uint newRoyalties) external {
        royalties = newRoyalties;
        emit RoyaltyUpdated(newRoyalties);
    }
}