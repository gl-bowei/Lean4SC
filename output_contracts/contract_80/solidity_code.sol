// SPDX-License-Identifier: MIT
pragma solidity ^0.8.10;

contract NFTMintingContract {
    enum SaleState { NotActive, PreSaleActive, PublicSaleActive }
    SaleState public currentState = SaleState.NotActive;

    uint public maxImps = 10000;
    mapping(address => uint) public MintedWallets;
    uint public SuperballPresaleMinted;
    uint public FloatingHeadClaimed;

    event NFTMinted(address indexed to, uint amount);
    event SaleStateChanged(SaleState newState);
    event GiveawayExecuted(address indexed winner);

    modifier onlyInState(SaleState _state) {
        require(currentState == _state, "Invalid state for this action");
        _;
    }

    function flipPreSaleState() public {
        require(currentState == SaleState.NotActive, "Cannot activate pre-sale");
        currentState = SaleState.PreSaleActive;
        emit SaleStateChanged(currentState);
    }

    function flipPublicSaleState() public {
        require(currentState == SaleState.NotActive, "Cannot activate public sale");
        currentState = SaleState.PublicSaleActive;
        emit SaleStateChanged(currentState);
    }

    function PreSuperballMintImps() public onlyInState(SaleState.PreSaleActive) {
        require(eligibleForPreSale(msg.sender), "Not eligible for pre-sale");
        
        MintedWallets[msg.sender]++;
        SuperballPresaleMinted++;

        emit NFTMinted(msg.sender, 1);
    }

    function PublicMintImps() public onlyInState(SaleState.PublicSaleActive) {
        require(eligibleForPublicSale(msg.sender), "Not eligible for public sale");

        MintedWallets[msg.sender]++;
        
        emit NFTMinted(msg.sender, 1);
    }

    function endPreSale() public onlyInState(SaleState.PreSaleActive) {
        currentState = SaleState.NotActive;
        emit SaleStateChanged(currentState);
    }

    function endPublicSale() public onlyInState(SaleState.PublicSaleActive) {
        currentState = SaleState.NotActive;
        emit SaleStateChanged(currentState);
    }

    function eligibleForPreSale(address addr) internal view returns (bool) {
        // Custom eligibility logic for pre-sale (e.g., ownership check)
        return true; // Placeholder
    }

    function eligibleForPublicSale(address addr) internal view returns (bool) {
        // Custom eligibility logic for public sale
        return true; // Placeholder
    }

    function withdraw() public {
        // Withdraw logic here
    }
}