// SPDX-License-Identifier: MIT
pragma solidity ^0.8.10;

// 导入 OpenZeppelin 合约来安全地处理 'owner' 
import "@openzeppelin/contracts/access/Ownable.sol";
import "@openzeppelin/contracts/token/ERC20/IERC20.sol";

/*
 * @title KikiInuToken
 * @dev 实现了 FSM 规范中定义的 PreLiquidity 和 ActiveTrading 状态 。
 */
contract KikiInuToken is IERC20, Ownable {

    // --- FSM $C$ 变量 (ERC20 标准) ---
    mapping(address => uint256) private _balances; // 
    mapping(address => mapping(address => uint256)) private _allowances; // 
    uint256 private _totalSupply;
    string private _name = "Kiki Inu";
    string private _symbol = "KIKI";
    uint8 private _decimals = 9;

    // --- FSM $C$ 变量 (自定义) ---
    
    // FSM 状态变量: false = PreLiquidity, true = ActiveTrading
    bool public tradingIsEnabled = false;

    // "fee structures" 
    uint256 public taxFeePercent = 2;
    uint256 public liquidityFeePercent = 5;

    // "liquidity management" 
    uint256 public minTokensBeforeSwap = 1000 * (10**_decimals);
    bool private _inSwapAndLiquify = false; // 重入保护

    // "protections against sniping" 
    mapping(address => bool) public isSniper;

    // Uniswap/Pancakeswap Router (用于 swap liquidity)
    address public uniswapRouter; 
    address public uniswapPair;

    // --- FSM $O$ 事件 ---
    event MinTokensBeforeSwapUpdated(uint256 minTokens); // 
    event SwapAndLiquify(uint256 tokensSwapped, uint256 ethReceived); // 
    event SniperCaught(address sniperAddress); // 

    // --- 修饰符 (Modifiers) ---
    
    // 此修饰符防止 'swap liquidity'  动作重入
    modifier lockTheSwap {
        _inSwapAndLiquify = true;
        _;
        _inSwapAndLiquify = false;
    }
    
    // --- 构造函数 (设置 s0 = PreLiquidity) ---
    constructor(address routerAddress) {
        _totalSupply = 1_000_000 * (10**_decimals);
        // 在 'PreLiquidity' 状态下，所有代币都归 'owner' (部署者)
        // 以便 "owner sets up liquidity" 
        _balances[msg.sender] = _totalSupply;
        emit Transfer(address(0), msg.sender, _totalSupply);

        uniswapRouter = routerAddress;
        
        // FSM 初始状态 s0 = PreLiquidity
        tradingIsEnabled = false;
    }

    // --- FSM 转换 (Transitions) ---

    /**
     * @dev 转换 1: (PreLiquidity) -> (ActiveTrading)
     * 对应 "enabling trading features" 
     */
    function enableTrading() external onlyOwner {
        require(!tradingIsEnabled, "KIKI: Trading is already enabled");
        // 动作 F_1:
        tradingIsEnabled = true;
        
        // (可选) 创建 LP 对
        // ...
    }

    /**
     * @dev 转换 2 & 3: transfer()
     * 核心 FSM 逻辑发生在这里
     */
    function transfer(address to, uint256 amount) external override returns (bool) {
        return _transfer(msg.sender, to, amount);
    }
    
    function transferFrom(address from, address to, uint256 amount) external override returns (bool) {
        _approve(from, msg.sender, _allowances[from][msg.sender] - amount);
        return _transfer(from, to, amount);
    }

    // --- 核心 FSM 逻辑 (transfer) ---
    function _transfer(address from, address to, uint256 amount) internal returns (bool) {
        require(from != address(0), "ERC20: transfer from the zero address");
        require(to != address(0), "ERC20: transfer to the zero address");
        require(_balances[from] >= amount, "ERC20: transfer amount exceeds balance");

        // 检查是否应触发 "swap liquidity" 
        uint256 contractTokenBalance = _balances[address(this)];
        bool canSwap = (contractTokenBalance >= minTokensBeforeSwap);

        if (canSwap && tradingIsEnabled && !_inSwapAndLiquify && from != uniswapPair) {
            // 动作 F_SWAP (对应 "swap liquidity" )
            swapAndLiquify(contractTokenBalance);
        }

        // --- FSM 状态路由 ---
        if (!tradingIsEnabled) {
            // 状态 = PreLiquidity
            // 守卫 G_2: (msg.sender == owner)
            require(from == owner(), "KIKI: Trading not enabled. Only owner can transfer for liquidity setup.");
            // 动作 F_2: (无税)
            _basicTransfer(from, to, amount);
        } else {
            // 状态 = ActiveTrading
            // 守卫 G_3: (!isSniper[from])
            require(!isSniper[from], "KIKI: Sniper address blocked.");
            // 动作 F_3: (有税)
            _taxedTransfer(from, to, amount);
        }

        return true;
    }

    // 动作 F_2 (PreLiquidity 状态下的转账)
    function _basicTransfer(address from, address to, uint256 amount) internal {
        _balances[from] -= amount;
        _balances[to] += amount;
        emit Transfer(from, to, amount);
    }

    // 动作 F_3 (ActiveTrading 状态下的转账)
    function _taxedTransfer(address from, address to, uint256 amount) internal {
        uint256 taxFee = (amount * taxFeePercent) / 100;
        uint256 liqFee = (amount * liquidityFeePercent) / 100;
        uint256 transferAmount = amount - taxFee - liqFee;

        _balances[from] -= amount;
        _balances[to] += transferAmount;
        _balances[address(this)] += (taxFee + liqFee); // 将税费发送给合约

        emit Transfer(from, to, transferAmount);
    }

    // 动作 F_SWAP (对应 "swap liquidity" )
    function swapAndLiquify(uint256 tokenAmount) internal lockTheSwap {
        // ...
        // 在这里实现将 'tokenAmount' 兑换为 ETH/BNB
        // 并将其添加回流动性池的逻辑
        // ...
        emit SwapAndLiquify(tokenAmount, 0); // (0 是 ETH 占位符)
    }

    // --- FSM 转换 (Owner/管理功能) ---

    /**
     * @dev 转换 4: setTaxes()
     * 对应 "set taxes" 
     */
    function setTaxes(uint256 newTaxFee, uint256 newLiquidityFee) external onlyOwner {
        // 动作 F_4:
        taxFeePercent = newTaxFee;
        liquidityFeePercent = newLiquidityFee;
    }

    /**
     * @dev 转换 5: catchSniper()
     * 对应 "protections against sniping" 
     */
    function catchSniper(address sniperAddress) external onlyOwner {
        // 动作 F_5:
        isSniper[sniperAddress] = true;
        emit SniperCaught(sniperAddress);
    }

    function setMinTokensBeforeSwap(uint256 newAmount) external onlyOwner {
        minTokensBeforeSwap = newAmount;
        emit MinTokensBeforeSwapUpdated(newAmount);
    }
    
    /**
     * @dev 转换 6: approve()
     * 对应 "approval of allowances" 
     */
    function approve(address spender, uint256 amount) external override returns (bool) {
        _approve(msg.sender, spender, amount);
        return true;
    }

    function _approve(address owner, address spender, uint256 amount) internal {
        require(owner != address(0), "ERC20: approve from the zero address");
        require(spender != address(0), "ERC20: approve to the zero address");
        // 动作 F_6:
        _allowances[owner][spender] = amount;
        emit Approval(owner, spender, amount);
    }

    // --- ERC20 只读函数 (用于显示 C 变量) ---
    function totalSupply() external view override returns (uint256) { return _totalSupply; }
    function balanceOf(address account) external view override returns (uint256) { return _balances[account]; }
    function allowance(address owner, address spender) external view override returns (uint256) { return _allowances[owner][spender]; }
    function name() external view returns (string memory) { return _name; }
    function symbol() external view returns (string memory) { return _symbol; }
    function decimals() external view returns (uint8) { return _decimals; }

    // (为 'swapAndLiquify' 添加 receive() 以接收 ETH/BNB)
    receive() external payable {}
}