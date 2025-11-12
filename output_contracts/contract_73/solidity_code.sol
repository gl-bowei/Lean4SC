// SPDX-License-Identifier: MIT
pragma solidity ^0.8.11;

contract NFTContract {

    uint public totalPublicSupply;
    uint public remainingPublicSupply;
    mapping(uint256 => string) public metadata;

    address public owner;
    string public baseURI;

    enum State { PublicMinting, PrivateMinting, MetadataUpdates }
    State public currentState;

    event BaseURIUpdated(string newBaseURI);

    modifier onlyOwner() {
        require(msg.sender == owner, "Not the contract owner");
        _;
    }

    constructor(uint _totalPublicSupply) {
        owner = msg.sender;
        totalPublicSupply = _totalPublicSupply;
        remainingPublicSupply = _totalPublicSupply;
        currentState = State.PublicMinting;
    }

    function mintPublic() external payable {
        require(currentState == State.PublicMinting, "Public minting not active");
        require(remainingPublicSupply > 0, "No public NFTs remaining");
        // Add logic for payment and minting NFT
        remainingPublicSupply--;
    }

    function mintPrivate(address to) external onlyOwner {
        require(currentState == State.PrivateMinting, "Private minting not active");
        // Add logic for minting NFT to specified address
    }

    function setBaseURI(string calldata newBaseURI) external onlyOwner {
        baseURI = newBaseURI;
        emit BaseURIUpdated(newBaseURI);
        currentState = State.MetadataUpdates;
    }

    function endPublicMint() external onlyOwner {
        currentState = State.PrivateMinting;
    }

    // Additional functions could be added for updating metadata, etc.
}