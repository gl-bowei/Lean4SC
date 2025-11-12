// SPDX-License-Identifier: MIT
pragma solidity ^0.8.4;

import "@openzeppelin/contracts/token/ERC721/extensions/ERC721A.sol";

contract NFTMintingContract is ERC721A {
    // State variables
    uint public PRICE = 0.02 ether;
    uint public MAX_PER_TXN = 5;
    bool public publicSale = false;
    uint public maxSupply = 10000;
    uint public freeMaxSupply = 1000;

    // Events
    event PublicSaleActivated(bool isActive);
    event Minted(address indexed to, uint256 quantity);

    // Constructor
    constructor() ERC721A("NFT Minting Token", "NFTMT") {}

    // Function to toggle the public sale status
    function togglePublicSale() external {
        publicSale = !publicSale;
        emit PublicSaleActivated(publicSale);
    }

    // Function to mint tokens
    function mint(uint256 quantity) external payable {
        require(publicSale, "Public sale is not active");
        require(quantity > 0 && quantity <= MAX_PER_TXN, "Invalid quantity");
        require(totalSupply() + quantity <= maxSupply, "Exceeds max supply");
        require(msg.value >= PRICE * quantity, "Insufficient funds");

        _mint(msg.sender, quantity);
        emit Minted(msg.sender, quantity);
    }

    // Function for admin to mint tokens
    function adminMint(address to, uint256 quantity) external {
        // This function should have access control to allow only admin to mint
        _mint(to, quantity);
        emit Minted(to, quantity);
    }

    // Function to decrease total supply (for example, for burning tokens)
    function decreaseTotalSupply(uint256 quantity) external {
        // Logic for decreasing total supply goes here
    }

    // Function to set the base URI for the token metadata
    function setBaseURI(string calldata baseURI) external {
        // Logic for setting the base URI goes here
    }

    // Override base URI-related methods if necessary
}