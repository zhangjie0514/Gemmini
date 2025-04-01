package gemmini

import chisel3._
import chisel3.util._
import GemminiISA._
import Util._
import org.chipsalliance.cde.config.Parameters
import midas.targetutils.PerfCounter

class AddressSequenceGenerator(sp_bank_entries: Int, sp_banks: Int) extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(log2Ceil(sp_bank_entries).W))   // 输入地址
    val valid = Input(Bool())             // 输入有效信号
    val addr_banks_in = Input(UInt(log2Ceil(sp_banks).W))
    val outAddr = Output(UInt(log2Ceil(sp_bank_entries).W)) // 输出地址
    val outValid = Output(Bool())         // 输出有效信号
    val outAddr_acc = Output(UInt(log2Ceil(sp_bank_entries / 8).W))
    val outValid_acc = Output(Bool())
    val addr_banks_out = Output(UInt(log2Ceil(sp_banks).W))
    val last = Output(Bool())
  })

  dontTouch(io.outValid_acc)
  dontTouch(io.outAddr_acc)
  dontTouch(io.last)
  // 地址表
  class AddressEntry extends Bundle {
    val valid = Bool()
    val isProcessed = Bool()
    val addr = UInt((log2Ceil(sp_bank_entries) - 4).W)
    val addr_banks = UInt(log2Ceil(sp_banks).W)
  }

  val numAccumulators = 5
  val addressTable = RegInit(VecInit(Seq.fill(numAccumulators)(0.U.asTypeOf(new AddressEntry))))
  // 计算 n = addr / 16
  val nWidth = log2Ceil(sp_bank_entries) - 4            // n 的位宽
  val n = io.addr(log2Ceil(sp_bank_entries) - 1, 4)     // addr >> 4

    // 命中检查
    val hitVec = Wire(Vec(numAccumulators, Bool()))
    for (i <- 0 until numAccumulators) {
        hitVec(i) := addressTable(i).valid && (addressTable(i).addr === n) && (addressTable(i).addr_banks === io.addr_banks_in)
    }
    val hit = hitVec.asUInt.orR            //按位或归约，判断是否命中
    val hitIndex = PriorityEncoder(hitVec) //返回第一个true，找到索引

    // 空闲累加器检查
    val freeVec = Wire(Vec(numAccumulators, Bool()))
    for (i <- 0 until numAccumulators) {
        freeVec(i) := !addressTable(i).valid
    }
    val hasFree = freeVec.asUInt.orR         //按位或归约，判断是否有空的累加器
    val freeIndex = PriorityEncoder(freeVec) //返回第一个空累加器索引
  when(io.valid){
    when (!hit && hasFree) {
            // 未命中，有空闲累加器
            // 分配新的累加器
            addressTable(freeIndex).valid := true.B
            addressTable(freeIndex).addr := n
            addressTable(freeIndex).addr_banks := io.addr_banks_in
    }
  }

  // 步骤1：计算 xorVec
  val xorVec = Wire(Vec(numAccumulators, Bool()))
  for (i <- 0 until numAccumulators) {
    xorVec(i) := addressTable(i).valid ^ addressTable(i).isProcessed
  }
  // 步骤2：计算变量1（xorVec 的按位或归约）
  val xorOr = xorVec.asUInt.orR // 变量1
  // 步骤3：计算变量2（第一个 xorVec(i) 为 1 的序号）
  val firstIndex = PriorityEncoder(xorVec) // 变量2
  // 状态机定义
  val sIdle :: sOutput :: rs :: Nil = Enum(3)
  val state = RegInit(sIdle)

  val cnt = RegInit(0.U(4.W))           // 计数器，从 0 到 15

  // 记录当前处理的 n 值和基地址
  val nReg = Reg(UInt(nWidth.W))
  val baseAddr = Reg(UInt(log2Ceil(sp_bank_entries).W))
  val addr_banks = Reg(UInt(log2Ceil(sp_banks).W))

  // 初始化输出
  io.outValid := false.B
  io.outAddr := 0.U
  io.outValid_acc := false.B
  io.outAddr_acc := 0.U
  io.addr_banks_out := DontCare
  io.last := false.B

  switch(state) {
    is(sIdle) {
      when(xorOr) {
        // 开始输出新的地址序列
        nReg := addressTable(firstIndex).addr
        baseAddr := Cat(addressTable(firstIndex).addr, 0.U(4.W))    // n * 16，补充低 4 位为 0
        addr_banks := addressTable(firstIndex).addr_banks
        cnt := 0.U
        // 标记 n 已经处理过
        addressTable(firstIndex).isProcessed := true.B
        state := sOutput
      }
    }
    is(sOutput) {
      io.outValid := true.B
      io.outAddr := baseAddr + cnt
      io.addr_banks_out := addr_banks
      when((cnt === 15.U) && xorOr) {
        // 序列输出完成，返回空闲状态
        state := sIdle
      } .elsewhen((cnt === 15.U) && !io.valid){
        state := rs
      }.otherwise {
        cnt := cnt + 1.U
      }
      when(cnt < 2.U){
      io.outValid_acc := true.B
      io.outAddr_acc := nReg * 2.U + cnt
      }
      when(cnt === 15.U){
        val allProcessed = addressTable.map { entry =>
          !(entry.valid && (entry.addr_banks === addr_banks)) || entry.isProcessed
        }.reduce(_ && _)
        io.last := allProcessed
      }
    }
    is(rs) {
        for(i <- 0 until numAccumulators){
            addressTable(i).addr := 0.U
            addressTable(i).isProcessed := false.B
            addressTable(i).valid := false.B
        }
        state := sIdle
    }
  }
}

// 更加通用化的表达
class AddressSequenceGenerator_ForAccumlator(acc_bank_entries: Int, rows: Int, acc_banks: Int) extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(log2Ceil(acc_bank_entries).W))   // 输入地址
    val addr_banks_in = Input(UInt(log2Ceil(acc_banks).W))
    val valid = Input(Bool())             // 输入有效信号
    val outAddr = Output(UInt(log2Ceil(acc_bank_entries).W)) // 输出地址
    val addr_banks_out = Output(UInt(log2Ceil(acc_banks).W))
    val outValid = Output(Bool())         // 输出有效信号
    val outAddr_acc = Output(UInt(log2Ceil(acc_bank_entries).W))
    val outValid_acc = Output(Bool())
    val blocks = Input(UInt((log2Ceil(rows) + 1).W))
    val blocks_valid = Input(Bool())
    // val dataIn = Flipped(Decoupled(new ScratchpadReadReq(n)))
    // val dataOut = Decoupled(new ScratchpadReadReq(n))
  })

  dontTouch(io.outValid_acc)
  dontTouch(io.outAddr_acc)
  dontTouch(io.outAddr)
  dontTouch(io.outValid)
  //接受块的大小
  val blocks = RegInit(0.U((log2Ceil(rows) + 1).W))
  when(io.blocks_valid){
    blocks := io.blocks
  }
  // 地址表
  class AddressEntry(addrWidth: Int, acc_banks: Int) extends Bundle {
    val valid = Bool()
    val isProcessed = Bool()
    val addr = UInt(addrWidth.W)
    val addr_banks = UInt(log2Ceil(acc_banks).W)
  }

  val numAccumulators = 3
  val addressTable = RegInit(VecInit(Seq.fill(numAccumulators)(0.U.asTypeOf(new AddressEntry(log2Ceil(acc_bank_entries), acc_banks)))))
  // 计算 n = addr / blocks
  val n = io.addr / blocks     

    // 命中检查
    val hitVec = Wire(Vec(numAccumulators, Bool()))
    for (i <- 0 until numAccumulators) {
        hitVec(i) := addressTable(i).valid && (addressTable(i).addr === n)
    }
    val hit = hitVec.asUInt.orR //按位或归约，判断是否命中
    val hitIndex = PriorityEncoder(hitVec) //返回第一个true，找到索引

    // 空闲累加器检查
    val freeVec = Wire(Vec(numAccumulators, Bool()))
    for (i <- 0 until numAccumulators) {
        freeVec(i) := !addressTable(i).valid
    }
    val hasFree = freeVec.asUInt.orR //按位或归约，判断是否有空的累加器
    val freeIndex = PriorityEncoder(freeVec) //返回第一个空累加器索引
  when(io.valid){
    when (!hit && hasFree) {
            // 未命中，有空闲累加器
            // 分配新的累加器
            addressTable(freeIndex).valid := true.B
            addressTable(freeIndex).addr := n
            addressTable(freeIndex).addr_banks := io.addr_banks_in
    }
  }

  // 步骤1：计算 xorVec
  val xorVec = Wire(Vec(numAccumulators, Bool()))
  for (i <- 0 until numAccumulators) {
    xorVec(i) := addressTable(i).valid ^ addressTable(i).isProcessed
  }

  // 步骤2：计算变量1（xorVec 的按位或归约）
  val xorOr = xorVec.asUInt.orR // 变量1

  // 步骤3：计算变量2（第一个 xorVec(i) 为 1 的序号）
  val firstIndex = PriorityEncoder(xorVec) // 变量2

  // 状态机定义
  val sIdle :: sOutput :: rs :: Nil = Enum(3)
  val state = RegInit(sIdle)

  val cnt = RegInit(0.U((log2Ceil(rows) + 1).W))           // 计数器，从 0 到 15
  val cnt_1 = RegInit(0.U((log2Ceil(rows) + 1).W))

  // 记录当前处理的 n 值和基地址
  val nReg = Reg(UInt(log2Ceil(acc_bank_entries).W))
  val baseAddr = Reg(UInt(log2Ceil(acc_bank_entries).W))
  val addr_banks = Reg(UInt(log2Ceil(acc_banks).W))

  // 初始化输出
  io.outValid := false.B
  io.outAddr := 0.U
  io.outValid_acc := false.B
  io.outAddr_acc := 0.U
  io.addr_banks_out := 0.U

  switch(state) {
    is(sIdle) {
      when(xorOr) {
        // 开始输出新的地址序列
        nReg := addressTable(firstIndex).addr
        baseAddr := addressTable(firstIndex).addr * blocks    // n * 16，补充低 4 位为 0
        addr_banks := addressTable(firstIndex).addr_banks
        cnt := 0.U
        cnt_1 := 0.U
        // 标记 n 已经处理过
        addressTable(firstIndex).isProcessed := true.B
        state := sOutput
      }
    }
    is(sOutput) {
      io.outAddr := baseAddr + cnt
      io.addr_banks_out := addr_banks
      cnt_1 := cnt_1 + 1.U
      when(cnt_1 < blocks){
        io.outValid := true.B
      }
      when((cnt === blocks - 1.U) && xorOr) {
        // 序列输出完成，返回空闲状态
        state := sIdle
      }.elsewhen((cnt === blocks - 1.U) && !io.valid){
        state := rs
      }.elsewhen((cnt === blocks - 1.U)){
        cnt := cnt //这是为了解决一些特殊情况
      }.otherwise {
        cnt := cnt + 1.U
      }
      when(cnt < 2.U){
      io.outValid_acc := true.B
      io.outAddr_acc := nReg * 2.U + cnt
      }
    }
    is(rs) {
        for(i <- 0 until numAccumulators){
            addressTable(i).addr := 0.U
            addressTable(i).addr_banks := 0.U
            addressTable(i).isProcessed := false.B
            addressTable(i).valid := false.B
        }
        state := sIdle
    }
  }
}
