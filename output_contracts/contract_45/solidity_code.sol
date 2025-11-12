// SPDX-License-Identifier: MIT
pragma solidity ^0.6.12;

contract TokenSaleContract {
    address public cyceToken;
    address public WETH;
    address public usdPair;
    address[] public tokenList;

    enum State { TokenSaleActive, TokenSaleEnded }
    State public currentState;

    event PurchaseEvent(address buyer, uint256 amount);
    event EndSaleEvent();
    event WithdrawEvent(address owner, uint256 amount);

    constructor(address _cyceToken, address _WETH, address _usdPair) public {
        cyceToken = _cyceToken;
        WETH = _WETH;
        usdPair = _usdPair;
        currentState = State.TokenSaleActive;
    }

    modifier onlyDuringSale() {
        require(currentState == State.TokenSaleActive, "Token sale is not active");
        _;
    }

    modifier onlyAfterSale() {
        require(currentState == State.TokenSaleEnded, "Token sale is not ended yet");
        _;
    }

    function buyToken(uint256 usdAmount) external onlyDuringSale {
        // Logic for purchasing tokens using USD value
        emit PurchaseEvent(msg.sender, usdAmount);
    }

    function endSale() external {
        currentState = State.TokenSaleEnded;
        emit EndSaleEvent();
    }

    function withdraw() external onlyAfterSale {
        // Logic for withdrawing funds and remaining tokens
        emit WithdrawEvent(msg.sender, address(this).balance);
    }

    function getUSDValue(uint256 tokenAmount) external view returns (uint256) {
        // Logic to calculate the equivalent USD value for tokens
        return tokenAmount; // Placeholder logic
    }
}