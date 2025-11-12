// SPDX-License-Identifier: MIT
pragma solidity ^0.8.3;

contract NFTMintingManagement {
    uint public NFTIndex;
    bool public mintingActive;
    mapping(uint => string) public artworkData;

    event NewNFTMouldCreated(address indexed creator);
    event NewNFTCreatedFor(address indexed owner, uint indexed nftId);
    event RedeemNFT(address indexed owner, uint indexed nftId);

    enum State { MintingActive, MintingClosed, Nftevolved }
    State public currentState;

    constructor() {
        NFTIndex = 0;
        mintingActive = true;
        currentState = State.MintingActive;
    }

    modifier onlyMintingActive() {
        require(mintingActive == true, "Minting is not active");
        _;
    }

    modifier onlyIfMintingClosed() {
        require(currentState == State.MintingClosed, "Minting is still active");
        _;
    }

    function createNFTMould() external {
        // Logic for creating NFT mould
        emit NewNFTMouldCreated(msg.sender);
    }

    function ownerMint() external onlyMintingActive {
        // Logic for minting NFTs
        emit NewNFTCreatedFor(msg.sender, NFTIndex);
        NFTIndex++;
        
        // Logic to check if edition size reached
        if (NFTIndex >= 100) { // Assuming edition size is 100
            mintingActive = false;
            currentState = State.MintingClosed;
        }
    }

    function evolveNFT() external onlyIfMintingClosed {
        // Logic for evolving the NFT
        currentState = State.Nftevolved;
    }

    function redeemNFT() external onlyIfMintingClosed {
        // Logic for redeeming NFTs for attributes
        emit RedeemNFT(msg.sender, NFTIndex);
    }
}