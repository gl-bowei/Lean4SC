// SPDX-License-Identifier: MIT
pragma solidity ^0.8.1;

contract CreatureMintingContract {
    address public proxyRegistryAddress;
    address public nftAddress;
    string public baseURI;
    uint public GENTE_SUPPLY;
    
    enum State { MintingAvailable, MintingClosed }
    State public currentState;

    event Transfer(address indexed from, address indexed to, uint256 indexed tokenId);

    constructor(address _proxyRegistryAddress, address _nftAddress, string memory _baseURI) {
        proxyRegistryAddress = _proxyRegistryAddress;
        nftAddress = _nftAddress;
        baseURI = _baseURI;
        GENTE_SUPPLY = 10000;
        currentState = State.MintingAvailable;
    }

    modifier onlyWhenMintingAvailable() {
        require(currentState == State.MintingAvailable, "Minting is closed");
        _;
    }

    function mintSingle() external onlyWhenMintingAvailable {
        require(canMintSingle(), "Minting single not allowed");
        // Logic for minting a single creature
        emit Transfer(address(0), msg.sender, 1); // Example tokenId
    }

    function mintNickelPack() external onlyWhenMintingAvailable {
        require(canMintNickelPack(), "Minting nickel pack not allowed");
        // Logic for minting a nickel pack of creatures
        emit Transfer(address(0), msg.sender, 2); // Example tokenId
    }

    function mintDimePack() external onlyWhenMintingAvailable {
        require(canMintDimePack(), "Minting dime pack not allowed");
        // Logic for minting a dime pack of creatures
        emit Transfer(address(0), msg.sender, 3); // Example tokenId
    }

    function mintDubPack() external onlyWhenMintingAvailable {
        require(canMintDubPack(), "Minting dub pack not allowed");
        // Logic for minting a dub pack of creatures
        emit Transfer(address(0), msg.sender, 4); // Example tokenId
    }

    function canMintSingle() internal view returns (bool) {
        // Logic to check if minting a single creature is allowed
        return true; // Placeholder
    }

    function canMintNickelPack() internal view returns (bool) {
        // Logic to check if minting a nickel pack is allowed
        return true; // Placeholder
    }

    function canMintDimePack() internal view returns (bool) {
        // Logic to check if minting a dime pack is allowed
        return true; // Placeholder
    }

    function canMintDubPack() internal view returns (bool) {
        // Logic to check if minting a dub pack is allowed
        return true; // Placeholder
    }

    function closeMinting() external {
        currentState = State.MintingClosed;
    }

    function openMinting() external {
        currentState = State.MintingAvailable;
    }
}