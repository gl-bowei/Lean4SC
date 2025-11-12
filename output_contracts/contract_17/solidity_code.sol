// SPDX-License-Identifier: MIT
pragma solidity ^0.8.4;

contract LinkedCollectorsPass {
    address public DEVELOPER_ADDRESS;
    address public SAFE_ADDRESS;
    uint public tokenPrice;
    uint public constant MAX_SUPPLY = 1000;
    uint public totalSupply;
    enum State { MintingActive, MintingPaused, MintingFinished }
    State public currentState;

    mapping(address => bool) public admins;

    event TokensMinted(address indexed to, uint amount);
    event MintingPaused();
    event MintingUnpaused();
    event AdminAdded(address indexed newAdmin);
    event TokenPriceSet(uint newPrice);

    constructor(address developer, address safe) {
        DEVELOPER_ADDRESS = developer;
        SAFE_ADDRESS = safe;
        currentState = State.MintingActive;
    }

    modifier onlyAdmin() {
        require(admins[msg.sender], "Not an admin");
        _;
    }

    modifier inState(State state) {
        require(currentState == state, "Invalid state");
        _;
    }

    function mintTokens() external inState(State.MintingActive) {
        require(totalSupply < MAX_SUPPLY, "Max supply reached");
        totalSupply++;
        emit TokensMinted(msg.sender, 1);
    }

    function burnTokens(uint amount) external {
        // Implement burn logic
    }

    function setTokenPrice(uint newPrice) external onlyAdmin {
        tokenPrice = newPrice;
        emit TokenPriceSet(newPrice);
    }

    function addAdmin(address newAdmin) external onlyAdmin {
        admins[newAdmin] = true;
        emit AdminAdded(newAdmin);
    }

    function withdraw() external onlyAdmin {
        // Implement withdraw logic
    }

    function pauseMinting() external onlyAdmin inState(State.MintingActive) {
        currentState = State.MintingPaused;
        emit MintingPaused();
    }

    function unpauseMinting() external onlyAdmin inState(State.MintingPaused) {
        currentState = State.MintingActive;
        emit MintingUnpaused();
    }
}