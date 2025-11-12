// SPDX-License-Identifier: MIT
pragma solidity ^0.8.7;

contract NFTMintingContract {
    uint public maxSupply = 10000;
    uint public price = 0.05 ether;
    uint public presaleStart;
    uint public publicStart;
    string public baseURI;
    bool public revealed = false;
    bool public paused = false;

    address public owner;

    enum State { PresalePeriod, PublicSalePeriod, PausedState, RevealedState }
    State public currentState;

    event TokenMinted(address indexed minter, uint amount);
    event SaleStarted(State saleState);
    event SalePaused();

    modifier onlyOwner() {
        require(msg.sender == owner, "Not the contract owner");
        _;
    }

    modifier isNotPaused() {
        require(!paused, "Contract is paused");
        _;
    }

    constructor() {
        owner = msg.sender;
        currentState = State.PresalePeriod;
    }

    function presaleMint() external payable isNotPaused {
        require(currentState == State.PresalePeriod, "Not in presale");
        require(msg.value == price, "Incorrect ETH amount");
        require(/* check eligibility based on signature */ true, "Not eligible for presale");

        // Mint NFT logic here
        emit TokenMinted(msg.sender, 1);
    }

    function publicSaleMint() external payable isNotPaused {
        require(currentState == State.PublicSalePeriod, "Not in public sale");
        require(msg.value == price, "Incorrect ETH amount");
        require(/* check minting limit */ true, "Max supply reached");

        // Mint NFT logic here
        emit TokenMinted(msg.sender, 1);
    }

    function airdrop(address recipient) external onlyOwner isNotPaused {
        // Airdrop NFT logic here
        emit TokenMinted(recipient, 1);
    }

    function startPublicSale() external onlyOwner {
        require(currentState == State.PresalePeriod, "Invalid state transition");
        currentState = State.PublicSalePeriod;
        emit SaleStarted(currentState);
    }

    function pauseContract() external onlyOwner {
        paused = true;
        currentState = State.PausedState;
        emit SalePaused();
    }

    function resumeContract() external onlyOwner {
        require(currentState == State.PausedState, "Contract is not paused");
        paused = false;
        currentState = State.PresalePeriod; // or PublicSalePeriod based on requirement
    }

    function revealNFTs() external onlyOwner {
        require(currentState == State.PresalePeriod || currentState == State.PublicSalePeriod, "Invalid state for revealing");
        revealed = true;
        currentState = State.RevealedState;
    }
}