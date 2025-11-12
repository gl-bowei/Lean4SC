// SPDX-License-Identifier: MIT
pragma solidity ^0.8.10;

/*
 * @title ZapInContract
 * @dev Implements the synchronous FSM defined in the specification.
 * Corresponds to the 'requirement.txt' description.
 */
contract ZapInContract {

    // --- 状态变量 (FSM $C$ 变量) ---
    address public owner;
    uint public goodwill; //
    bool public stopped;    //
    address public exchange2Token; //
    mapping(address => bool) public tokenAddresses; //

    // --- 事件 (FSM $O$ 变量) ---
    event ContractActivated(); //
    event ContractPaused();    //
    event TokenApprovalConfirmed(); //
    
    // --- FSM 状态映射 ---
    // 'Enabled' 状态  == (stopped == false)
    // 'Paused' 状态   == (stopped == true)

    constructor() {
        owner = msg.sender;
        stopped = false; // 初始状态 s0 = Enabled
        exchange2Token = address(0);
        goodwill = 0;
    }

    // --- FSM 守卫 (Guards) ---

    // 守卫: 检查状态是否为 'Enabled'
    modifier onlyEnabled() {
        // FSM Guard: [!stopped]
        require(!stopped, "ZapInContract: Contract is paused");
        _;
    }

    // 守卫: 检查调用者是否为 'owner'
    modifier onlyOwner() {
        // FSM Guard: [msg.sender == owner]
        require(msg.sender == owner, "ZapInContract: Not owner");
        _;
    }

    // --- FSM 转换 (Transitions) ---

    /*
     * @dev 转换: (Enabled, G_1, F_1, Paused)
     * 触发器: pause()
     */
    function pause() external onlyOwner {
        // 动作 F_1:
        stopped = true;
        emit ContractPaused();
        // 目标状态: Paused
    }

    /*
     * @dev 转换: (Paused, G_2, F_2, Enabled)
     * 触发器: activate()
     */
    function activate() external onlyOwner {
        // 动作 F_2:
        stopped = false;
        emit ContractActivated();
        // 目标状态: Enabled
    }

    /*
     * @dev 转换: (Enabled, G_3, F_3, Enabled)
     * 触发器: approveToken(token)
     */
    function approveToken(address token) external onlyEnabled {
        // 动作 F_3:
        tokenAddresses[token] = true;
        emit TokenApprovalConfirmed();
        // 目标状态: Enabled (自循环)
    }

    /*
     * @dev 转换: (Enabled, G_4, F_4, Enabled)
     * 触发器: ZapIn(token)
     */
    function ZapIn(address token) external onlyEnabled {
        // 守卫 G_4:
        require(tokenAddresses[token], "ZapInContract: Token not approved");
        
        // 动作 F_4 (原子):
        _performTrade(token);
        _addLiquidity(token);
        _returnPoolTokens(msg.sender);
        
        // 目标状态: Enabled (自循环)
    }

    // --- FSM 中未明确定义但已列出的函数 ---

    function ZapInWithETH() external payable onlyEnabled {
        // 逻辑...
    }

    function ZapInWithERC20(address token) external onlyEnabled {
        // 逻辑...
    }

    function enterLiquidity() external onlyEnabled {
        // 逻辑...
    }


    // --- 内部逻辑 (FSM 动作 F_4 的辅助函数) ---
    
    function _performTrade(address token) internal {
        // "performs trades"
        // ... (交易逻辑)
    }
    
    function _addLiquidity(address token) internal {
        // "adding liquidity to the selected Curve pool"
        // ... (添加流动性逻辑)
    }
    
    function _returnPoolTokens(address user) internal {
        // "returns the received pool tokens"
        // ... (返回 LP token 逻辑)
    }
}