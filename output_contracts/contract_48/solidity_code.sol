pragma solidity >=0.4.21 <0.6.0;







contract CFETHControllerInterface{
  function get_current_pool() public view returns(ICurvePoolForETH);
}

contract TokenInterfaceETH{
  function destroyTokens(address _owner, uint _amount) public returns(bool);
  function generateTokens(address _owner, uint _amount) public returns(bool);
}

contract CFETHVault is Ownable, TokenClaimer, ReentrancyGuard{
  using SafeMath for uint256;

  // target token is ETH
  CFETHControllerInterface public controller;

  uint256 public ratio_base;
  uint256 public withdraw_fee_ratio;
  uint256 public deposit_fee_ratio;
  address payable public fee_pool;
  address public lp_token;
  uint256 public max_amount;
  uint256 public slip;

  constructor(address _lp_token, address _controller) public {
  //ERC20Base(ERC20Base(address(0x0)), 0, "CFF liquidity pool token", 18, "CFF", true) public{
    require(_controller != address(0x0), "invalid controller");
    controller = CFETHControllerInterface(_controller);
    ratio_base = 10000;
    lp_token = _lp_token;
  }

  event ChangeMaxAmount(uint256 old, uint256 _new);
  function set_max_amount(uint _amount) public onlyOwner{
    uint256 old = max_amount;
    max_amount = _amount;
    emit ChangeMaxAmount(old, max_amount);
  }

  event ChangeSlippage(uint256 old, uint256 _new);
  function set_slippage(uint256 _slip) public onlyOwner{
    uint256 old = slip;
    slip = _slip;
    emit ChangeSlippage(old, slip);
  }

  event CFFDeposit(address from, uint256 eth_amount, uint256 cff_amount, uint256 virtual_price);
  event CFFDepositFee(address from, uint256 eth_amount, uint256 fee_amount);
  //@_amount: USDC amount
  function deposit() public payable nonReentrant{
    require(controller != CFETHControllerInterface(0x0) && controller.get_current_pool() != ICurvePoolForETH(0x0), "paused");
    require(msg.value > 0, "CFVault: zero amount");
    require(slip != 0, "Slippage not set");

    uint _amount = msg.value;

    require(_amount <= max_amount, "too large amount");

    if(deposit_fee_ratio != 0 && fee_pool != address(0x0)){
      uint256 f = _amount.safeMul(deposit_fee_ratio).safeDiv(ratio_base);
      emit CFFDepositFee(msg.sender, _amount, f);
      _amount = _amount.safeSub(f);
      if(f != 0){
        fee_pool.transfer(f);
      }
    }
    uint eth_before = address(this).balance;

    uint vir = controller.get_current_pool().get_virtual_price();
    uint min_amount = _amount.safeMul(uint(1e14)).safeMul(slip).safeDiv(vir);

    uint lp_before = controller.get_current_pool().get_lp_token_balance();
    controller.get_current_pool().deposit.value(_amount)();

    //check exchanges
    uint eth_after = address(this).balance;
    uint actual_deposit =  eth_before - eth_after;

    require(actual_deposit <= _amount, "deposit overflow");
    if(_amount != actual_deposit){
      msg.sender.transfer(_amount - actual_deposit);
    }


    uint lp_after = controller.get_current_pool().get_lp_token_balance();
    uint256 lp_amount = lp_after.safeSub(lp_before);

    require(lp_amount >= min_amount, "Slippage");

    uint256 d = ERC20Base(controller.get_current_pool().get_lp_token_addr()).decimals();
    require(d <= 18, "invalid decimal");
    uint cff_amount = 0;
    if (lp_before == 0){
      cff_amount = lp_amount.safeMul(uint256(10)**18).safeDiv(uint256(10)**d);
    }
    else{
      cff_amount = lp_amount.safeMul(IERC20(lp_token).totalSupply()).safeDiv(lp_before);
    }
    TokenInterfaceETH(lp_token).generateTokens(msg.sender, cff_amount);
    emit CFFDeposit(msg.sender, actual_deposit, cff_amount, get_virtual_price());
  }


  event CFFWithdraw(address from, uint256 eth_amount, uint256 cff_amount, uint256 usdc_fee, uint256 virtual_price);
  //@_amount: CFLPToken amount
  function withdraw(uint256 _amount) public nonReentrant{
    require(controller != CFETHControllerInterface(0x0) && controller.get_current_pool() != ICurvePoolForETH(0x0), "paused");
    require(slip != 0, "Slippage not set");
    uint256 amount = IERC20(lp_token).balanceOf(msg.sender);
    require(amount >= _amount, "no enough LP tokens");

    uint LP_token_amount = _amount.safeMul(controller.get_current_pool().get_lp_token_balance()).safeDiv(IERC20(lp_token).totalSupply());

    uint vir = controller.get_current_pool().get_virtual_price();
    uint min_amount = _amount.safeMul(vir).safeMul(slip).safeDiv(uint(1e22));

    uint256 _before = address(this).balance;
    controller.get_current_pool().withdraw(LP_token_amount);
    uint256 _after = address(this).balance;
    uint256 eth_amount = _after.safeSub(_before);

    require(eth_amount >= min_amount, "Slippage");


    if(withdraw_fee_ratio != 0 && fee_pool != address(0x0)){
        uint256 f = eth_amount.safeMul(withdraw_fee_ratio).safeDiv(ratio_base);
        uint256 r = eth_amount.safeSub(f);
        TokenInterfaceETH(lp_token).destroyTokens(msg.sender, _amount);
        msg.sender.transfer(r);
        fee_pool.transfer(f);

        emit CFFWithdraw(msg.sender, r, _amount, f, get_virtual_price());
    }else{
        TokenInterfaceETH(lp_token).destroyTokens(msg.sender, _amount);
        msg.sender.transfer(eth_amount);
        emit CFFWithdraw(msg.sender, eth_amount, _amount, 0, get_virtual_price());
    }
  }

  event ChangeWithdrawFee(uint256 old, uint256 _new);
  function changeWithdrawFee(uint256 _fee) public onlyOwner{
    require(_fee < ratio_base, "invalid fee");
    uint256 old = withdraw_fee_ratio;
    withdraw_fee_ratio = _fee;
    emit ChangeWithdrawFee(old, withdraw_fee_ratio);
  }

  event ChangeDepositFee(uint256 old, uint256 _new);
  function changeDepositFee(uint256 _fee) public onlyOwner{
    require(_fee < ratio_base, "invalid fee");
    uint256 old = deposit_fee_ratio;
    deposit_fee_ratio = _fee;
    emit ChangeDepositFee(old, deposit_fee_ratio);
  }

  event ChangeController(address old, address _new);
  function changeController(address _ctrl) public onlyOwner{
    address old = address(controller);
    controller = CFETHControllerInterface(_ctrl);
    emit ChangeController(old, address(controller));
  }

  event ChangeFeePool(address old, address _new);
  function changeFeePool(address payable _fp) public onlyOwner{
    address payable old = fee_pool;
    fee_pool = _fp;
    emit ChangeFeePool(old, fee_pool);
  }

  function get_virtual_price() public view returns(uint256){
    ICurvePoolForETH cp = controller.get_current_pool();
    uint256 v1 = cp.get_lp_token_balance().safeMul(uint256(10)**ERC20Base(lp_token).decimals());
    uint256 v2 = IERC20(lp_token).totalSupply().safeMul(uint256(10) ** ERC20Base(cp.get_lp_token_addr()).decimals());
    if(v2 == 0){
      return 0;
    }
    return v1.safeMul(cp.get_virtual_price()).safeDiv(v2);
  }


  function() external payable{

  }

}

contract CFETHVaultFactory{
  event NewCFETHVault(address addr);

  function createCFETHVault(address _lp_token, address _controller) public returns(address){
    CFETHVault cf = new CFETHVault(_lp_token, _controller);
    cf.transferOwnership(msg.sender);
    emit NewCFETHVault(address(cf));
    return address(cf);
  }

}