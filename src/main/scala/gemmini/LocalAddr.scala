package gemmini

import chisel3._
import chisel3.util._

class LocalAddr(sp_banks: Int, sp_bank_entries: Int, acc_banks: Int, acc_bank_entries: Int) extends Bundle {
  private val localAddrBits = 32 // TODO magic number; 地址的总位宽，固定为 32 位

  private val spAddrBits = log2Ceil(sp_banks * sp_bank_entries)   //(14) 计算片上存储地址所需的位数
  private val accAddrBits = log2Ceil(acc_banks * acc_bank_entries)//(10) 计算累加器地址所需的位数
  private val maxAddrBits = spAddrBits max accAddrBits            //(14) 取spAddrBits 和 accAddrBits 中的较大值，用于确定存储数据的位宽

  private val spBankBits = log2Up(sp_banks)           //(2) 用于表示片上存储银行编号所需的位数
  private val spBankRowBits = log2Up(sp_bank_entries) //(12) 用于表示片上存储行号所需的位数

  private val accBankBits = log2Up(acc_banks)   //(1) 用于表示累加器银行编号所需的位数
  val accBankRowBits = log2Up(acc_bank_entries) //(9) 用于表示累加器行号所需的位数，并且是一个公共字段

  val spRows = sp_banks * sp_bank_entries //(16384) 计算片上存储的总行数

  val is_acc_addr = Bool()       //指示该地址是否指向累加器
  val accumulate = Bool()        //指示是否执行累加操作
  val read_full_acc_row = Bool() //指示是否读取完整的累加器行
  val norm_cmd = NormCmd()       //规范化命令

  private val metadata_w = is_acc_addr.getWidth + accumulate.getWidth + read_full_acc_row.getWidth + norm_cmd.getWidth //元数据位宽的总和
  assert(maxAddrBits + metadata_w < 32) //确保地址数据和元数据的总位宽不超过 32 位

  val garbage = UInt(((localAddrBits - maxAddrBits - metadata_w - 1) max 0).W)                  //未使用的位，用于填充地址位宽
  val garbage_bit = if (localAddrBits - maxAddrBits >= metadata_w + 1) UInt(1.W) else UInt(0.W) //单个垃圾位，用于标记地址是否为垃圾
  val data = UInt(maxAddrBits.W) //存储实际地址信息

  def sp_bank(dummy: Int = 0) = if (spAddrBits == spBankRowBits) 0.U else data(spAddrBits - 1, spBankRowBits)     //提取片上存储的银行号
  def sp_row(dummy: Int = 0) = data(spBankRowBits - 1, 0)  //提取片上存储的行号
  def acc_bank(dummy: Int = 0) = if (accAddrBits == accBankRowBits) 0.U else data(accAddrBits - 1, accBankRowBits)//提取累加器的银行号
  def acc_row(dummy: Int = 0) = data(accBankRowBits - 1, 0)//提取累加器的行号

  def full_sp_addr(dummy: Int = 0) = data(spAddrBits - 1, 0)  //返回完整的片上存储地址
  def full_acc_addr(dummy: Int = 0) = data(accAddrBits - 1, 0)//返回完整的累加器地址

  def is_same_address(other: LocalAddr): Bool = is_acc_addr === other.is_acc_addr && data === other.data //检查两个地址是否相同，LocalAddr 类型
  def is_same_address(other: UInt): Bool = is_same_address(other.asTypeOf(this))                         //检查两个地址是否相同，UInt 类型
  def is_garbage(dummy: Int = 0) = is_acc_addr && accumulate && read_full_acc_row && data.andR &&        //判断地址是否为无效或垃圾地址
    (if (garbage_bit.getWidth > 0) garbage_bit.asBool else true.B)

  //地址加法操作，结果是 LocalAddr 类型
  def +(other: UInt) = {
    require(isPow2(sp_bank_entries)) // TODO remove this requirement
    require(isPow2(acc_bank_entries)) // TODO remove this requirement

    val result = WireInit(this)
    result.data := data + other
    result
  }

  //比较两个 LocalAddr 的大小，支持累加器和片上存储地址
  def <=(other: LocalAddr) =
    is_acc_addr === other.is_acc_addr &&
      Mux(is_acc_addr, full_acc_addr() <= other.full_acc_addr(), full_sp_addr() <= other.full_sp_addr())

  //检查是否小于另一个地址
  def <(other: LocalAddr) =
    is_acc_addr === other.is_acc_addr &&
      Mux(is_acc_addr, full_acc_addr() < other.full_acc_addr(), full_sp_addr() < other.full_sp_addr())

  //检查是否大于另一个地址
  def >(other: LocalAddr) =
    is_acc_addr === other.is_acc_addr &&
      Mux(is_acc_addr, full_acc_addr() > other.full_acc_addr(), full_sp_addr() > other.full_sp_addr())

  //进行加法运算并检查是否有溢出，返回新的地址和溢出标志
  def add_with_overflow(other: UInt): Tuple2[LocalAddr, Bool] = {
    require(isPow2(sp_bank_entries)) // TODO remove this requirement
    require(isPow2(acc_bank_entries)) // TODO remove this requirement

    val sum = data +& other

    val overflow = Mux(is_acc_addr, sum(accAddrBits), sum(spAddrBits))

    val result = WireInit(this)
    result.data := sum(maxAddrBits - 1, 0)

    (result, overflow)
  }

  // This function can only be used with non-accumulator addresses. Returns both new address and underflow
  //执行减法操作，检查是否下溢，返回新地址和下溢标志
  def floorSub(other: UInt, floor: UInt): (LocalAddr, Bool) = {
    require(isPow2(sp_bank_entries)) // TODO remove this requirement
    require(isPow2(acc_bank_entries)) // TODO remove this requirement

    val underflow = data < (floor +& other)

    val result = WireInit(this)
    result.data := Mux(underflow, floor, data - other)

    (result, underflow)
  }

  //将当前地址标记为垃圾地址
  def make_this_garbage(dummy: Int = 0): Unit = {
    is_acc_addr := true.B
    accumulate := true.B
    read_full_acc_row := true.B
    garbage_bit := 1.U
    data := ~(0.U(maxAddrBits.W))
  }

}

object LocalAddr {
  def cast_to_local_addr[T <: Data](local_addr_t: LocalAddr, t: T): LocalAddr = {
    // This convenience function is basically the same as calling "asTypeOf(local_addr_t)". However, this convenience
    // function will also cast unnecessary garbage bits to 0, which may help reduce multiplier/adder bitwidths
    val result = WireInit(t.asTypeOf(local_addr_t))
    if (result.garbage_bit.getWidth > 0) result.garbage := 0.U
    result
  }

  def cast_to_sp_addr[T <: Data](local_addr_t: LocalAddr, t: T): LocalAddr = {
    // This function is a wrapper around cast_to_local_addr, but it assumes that the input will not be the garbage
    // address
    val result = WireInit(cast_to_local_addr(local_addr_t, t))
    result.is_acc_addr := false.B
    result.accumulate := false.B
    result.read_full_acc_row := false.B

    // assert(!result.garbage_bit, "cast_to_sp_addr doesn't work on garbage addresses")

    result
  }

  def cast_to_acc_addr[T <: Data](local_addr_t: LocalAddr, t: T, accumulate: Bool, read_full: Bool): LocalAddr = {
    // This function is a wrapper around cast_to_local_addr, but it assumes that the input will not be the garbage
    // address
    val result = WireInit(cast_to_local_addr(local_addr_t, t))
    result.is_acc_addr := true.B
    result.accumulate := accumulate
    result.read_full_acc_row := read_full

    // assert(!result.garbage_bit, "cast_to_acc_addr doesn't work on garbage addresses")

    result
  }

  def garbage_addr(local_addr_t: LocalAddr): LocalAddr = {
    val result = Wire(chiselTypeOf(local_addr_t))
    result := DontCare
    result.make_this_garbage()
    result
  }
}
