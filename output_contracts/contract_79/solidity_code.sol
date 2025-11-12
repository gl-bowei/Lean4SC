// SPDX-License-Identifier: MIT
pragma solidity ^0.8.9;

contract TokenContract {
    string private baseTokenUri;
    string public name;
    string public symbol;

    enum State { TokenMinting, TokenMinted, MetadataManagement }
    State public currentState;

    event MintToken(address indexed recipient, uint256 tokenId, uint256 amount);
    event UpdateBaseURI(string newBaseUri);
    event RetrieveMetadata(uint256 tokenId);

    constructor(string memory _name, string memory _symbol, string memory _baseTokenUri) {
        name = _name;
        symbol = _symbol;
        baseTokenUri = _baseTokenUri;
        currentState = State.TokenMinting;
    }

    function mintTokenAction(address recipient, uint256 tokenId, uint256 amount) external {
        require(currentState == State.TokenMinting, "Invalid state for minting");
        // Mint logic (omitted for brevity)
        
        emit MintToken(recipient, tokenId, amount);
        currentState = State.TokenMinted;
    }

    function setBaseTokenURIAction(string memory newBaseUri) external {
        require(currentState == State.TokenMinting || currentState == State.TokenMinted, "Invalid state for base URI update");
        baseTokenUri = newBaseUri;

        emit UpdateBaseURI(newBaseUri);
        currentState = State.MetadataManagement;
    }

    function retrieveTokenMetadataAction(uint256 tokenId) external {
        require(currentState == State.MetadataManagement, "Invalid state for metadata retrieval");
        // Retrieve metadata logic (omitted for brevity)

        emit RetrieveMetadata(tokenId);
    }

    function uri(uint256 tokenId) external view returns (string memory) {
        return string(abi.encodePacked(baseTokenUri, "/", uint2str(tokenId)));
    }
    
    function uint2str(uint256 _i) internal pure returns (string memory) {
        if (_i == 0) {
            return "0";
        }
        uint256 j = _i;
        uint256 len;
        while (j != 0) {
            len++;
            j /= 10;
        }
        bytes memory bstr = new bytes(len);
        uint256 k = len;
        while (_i != 0) {
            bstr[--k] = bytes1(uint8(48 + _i % 10));
            _i /= 10;
        }
        return string(bstr);
    }
}