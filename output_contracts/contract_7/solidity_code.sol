// SPDX-License-Identifier: MIT
pragma solidity ^0.8.7;

contract VapenApes {
    bool public SALE_ACTIVE = false;
    bool public PRESALE_ACTIVE = false;
    uint public MAX_APES = 1000;
    uint public PRESALE_PURCHASES = 0;

    event MintApe(address indexed owner, uint quantity);

    modifier onlyDuringPresale() {
        require(PRESALE_ACTIVE, "Presale is not active.");
        _;
    }

    modifier onlyDuringSale() {
        require(SALE_ACTIVE, "Sale is not active.");
        _;
    }

    function flipSaleState() internal {
        SALE_ACTIVE = !SALE_ACTIVE;
    }

    function presaleMintApe() public onlyDuringPresale {
        require(PRESALE_PURCHASES < MAX_APES, "Max presale limit reached.");
        PRESALE_PURCHASES++;
        emit MintApe(msg.sender, 1);
    }

    function mintApe() public onlyDuringSale {
        emit MintApe(msg.sender, 1);
    }

    function activatePresale() external {
        require(!PRESALE_ACTIVE, "Presale already active.");
        PRESALE_ACTIVE = true;
        flipSaleState();
    }

    function endPresale() external {
        require(PRESALE_ACTIVE, "Presale is not active.");
        PRESALE_ACTIVE = false;
        flipSaleState();
    }

    function activateSale() external {
        require(!SALE_ACTIVE, "Sale already active.");
        flipSaleState();
    }

    function reserveApes(uint quantity) external {
        // Function to reserve Apes (implementation can be added as required)
    }
}