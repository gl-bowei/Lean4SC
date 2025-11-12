// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract NFTMintingContract {
    string public baseURI;
    uint public cost;
    uint public maxSupply;
    bool public paused = true;
    bool public revealed = false;
    mapping(address => uint) public addressMintedBalance;

    enum State {Paused, MintingActive, Revealed}
    State public currentState;

    event NFTMinted(address indexed to, uint256 quantity);
    event MetadataRevealed(string baseURI);
    event WhitelistUpdated(address indexed user, bool isWhitelisted);

    constructor(string memory _baseURI, uint _cost, uint _maxSupply) {
        baseURI = _baseURI;
        cost = _cost;
        maxSupply = _maxSupply;
        currentState = State.Paused;
    }

    modifier onlyOwner {
        // Placeholder for owner check
        require(msg.sender == address(this), "Not the owner");
        _;
    }

    modifier whenNotPaused {
        require(currentState == State.MintingActive, "Contract is paused");
        _;
    }

    modifier whenPaused {
        require(currentState == State.Paused, "Contract is not paused");
        _;
    }

    function unpauseContract() public onlyOwner whenPaused {
        currentState = State.MintingActive;
    }

    function mintNFT(uint256 quantity) public payable whenNotPaused {
        require(msg.value >= cost * quantity, "Insufficient ETH sent");
        require(addressMintedBalance[msg.sender] + quantity <= maxSupply, "Minting limit exceeded");
        
        addressMintedBalance[msg.sender] += quantity;
        emit NFTMinted(msg.sender, quantity);
    }

    function completeMinting() public onlyOwner {
        currentState = State.Revealed;
    }

    function revealMetadata() public onlyOwner {
        revealed = true;
        emit MetadataRevealed(baseURI);
    }

    function setCosts(uint _cost) public onlyOwner {
        cost = _cost;
    }

    function manageWhitelists(address user, bool isWhitelisted) public onlyOwner {
        emit WhitelistUpdated(user, isWhitelisted);
    }
}