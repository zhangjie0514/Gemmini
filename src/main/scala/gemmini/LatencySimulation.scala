package gemmini

import chisel3._
import chisel3.util._
import GemminiISA._
import Util._
import org.chipsalliance.cde.config.Parameters
import midas.targetutils.PerfCounter
import shapeless.ops.nat.Mod

class LatencySimulation_ReadSpad123(w: Int, rows: Int) extends Module{
    val io = IO(new Bundle {
        val in = Input(new ScratchpadReadResp(w))
        val valid_in = Input(Bool())
        val out = Output(new ScratchpadReadResp(w))
        val valid_out = Output(Bool())

        val Verification_completed = Input(Bool())
    })
    dontTouch(io.out)
    dontTouch(io.valid_out)
    io.out := DontCare
    io.valid_out := false.B
    val counter = RegInit(0.U((log2Ceil(rows) + 1).W))
    val counter_1 = RegInit(0.U((log2Ceil(rows) + 1).W))
    val storage_data = Reg(Vec(rows, new ScratchpadReadResp(w)))
    val storage_valid = Reg(Vec(rows, Bool()))
    val state = RegInit(false.B)
    when(io.valid_in){
        storage_data(counter) := io.in
        storage_valid(counter) := io.valid_in
        counter := counter + 1.U
    }
    when(io.Verification_completed){
        state := true.B
    }
    when(state){
        io.out := storage_data(counter_1)
        io.valid_out := storage_valid(counter_1)
        counter_1 := counter_1 + 1.U
        when(counter_1 === counter - 1.U){
            state := false.B
            counter := 0.U
            counter_1 := 0.U
        }
    }
}

class LatencySimulation_ReadSpad(w: Int, rows: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(new ScratchpadReadResp(w))
    val valid_in = Input(Bool())
    val out = Output(new ScratchpadReadResp(w))
    val valid_out = Output(Bool())
    val Verification_completed = Input(Bool())
  })
  
  dontTouch(io.out)
  dontTouch(io.valid_out)
  // 双缓冲存储
  val buffers = RegInit(VecInit(Seq.fill(2)(VecInit(Seq.fill(rows)(0.U.asTypeOf(new ScratchpadReadResp(w)))))))
  val valids = RegInit(VecInit(Seq.fill(2)(VecInit(Seq.fill(rows)(false.B)))))
  val writeCounters = RegInit(VecInit(Seq(0.U((log2Ceil(rows)+1).W), 0.U((log2Ceil(rows)+1).W))))
  
  val readBuffer = RegInit(0.U(1.W))
  val writeBuffer = RegInit(0.U(1.W))
  val readCounter = RegInit(0.U((log2Ceil(rows)+1).W))
  val state = RegInit(false.B)

  // 输入处理
  when(io.valid_in) {
    buffers(writeBuffer)(writeCounters(writeBuffer)) := io.in
    valids(writeBuffer)(writeCounters(writeBuffer)) := io.valid_in
    writeCounters(writeBuffer) := writeCounters(writeBuffer) + 1.U
  }

  // 完成信号处理
  when(io.Verification_completed && !state) {
    readBuffer := writeBuffer
    writeBuffer := ~writeBuffer
    state := true.B
    readCounter := 0.U
  }

  // 输出处理
  io.out := buffers(readBuffer)(readCounter)
  io.valid_out := valids(readBuffer)(readCounter)
  
  when(state) {
    readCounter := readCounter + 1.U
    when(readCounter === writeCounters(readBuffer) - 1.U) {
      // 清空已读缓冲区的有效标志
      valids(readBuffer).foreach(_ := false.B)
      writeCounters(readBuffer) := 0.U
      state := false.B
    }
  }.otherwise {
    io.valid_out := false.B
  }
}

class LatencySimulation_WriteAccumlator [T <: Data, U <: Data, V <: Data](xLen: Int, tagWidth: Int, config: GemminiArrayConfig[T, U, V])
                                  (implicit p: Parameters, ev: Arithmetic[T]) extends Module {
    import config._
    import ev._

    val io = IO(new Bundle {
        val in = Input(new AccumulatorWriteReq(acc_bank_entries, Vec(meshColumns, Vec(tileColumns, accType))))
        val valid_in = Input(Bool())
        val addr_banks_in = Input(UInt(log2Ceil(acc_banks).W))
        val out = Output(new AccumulatorWriteReq(acc_bank_entries, Vec(meshColumns, Vec(tileColumns, accType))))
        val valid_out = Output(Bool())
        val addr_banks_out = Output(UInt(log2Ceil(acc_banks).W))
        val Verification_completed = Input(Bool())
    })
    dontTouch(io.out)
    dontTouch(io.valid_out)
    io.out := DontCare
    io.valid_out := false.B
    io.addr_banks_out := 0.U
    val counter = RegInit(0.U((log2Ceil(tileRows * meshRows) + 1).W))
    val counter_1 = RegInit(0.U((log2Ceil(tileRows * meshRows) + 1).W))
    val storage_data = Reg(Vec(tileRows * meshRows, new AccumulatorWriteReq(acc_bank_entries, Vec(meshColumns, Vec(tileColumns, accType)))))
    val storage_valid = Reg(Vec(tileRows * meshRows, Bool()))
    val storage_addr_banks = Reg(UInt(log2Ceil(acc_banks).W))
    val state = RegInit(false.B)
    when(io.valid_in){
        storage_data(counter) := io.in
        storage_valid(counter) := io.valid_in
        storage_addr_banks := io.addr_banks_in
        counter := counter + 1.U
    }
    when(io.Verification_completed){
        state := true.B
    }
    /* when(state){
        io.out := storage_data(counter - 1.U)
        io.valid_out := storage_valid(counter - 1.U)
        counter := counter - 1.U
        when(counter === 0.U){
            state := false.B
        }
    } */
     when(state){
        io.out := storage_data(counter_1)
        io.valid_out := storage_valid(counter_1)
        io.addr_banks_out := storage_addr_banks
        counter_1 := counter_1 + 1.U
        when(counter_1 === counter - 1.U){
            state := false.B
            counter := 0.U
            counter_1 := 0.U
        }
    }
}

class LatencySimulation_ReadSpad_InsideEx[T <: Data: Arithmetic, U <: TagQueueTag with Data](meshRows: Int, tileRows: Int, meshColumns: Int, tileColumns: Int, inputType: T, accType: T, tagType: U) 
 extends Module {

  val A_TYPE = Vec(meshRows, Vec(tileRows, inputType))
  val B_TYPE = Vec(meshColumns, Vec(tileColumns, inputType))
  val D_TYPE = Vec(meshColumns, Vec(tileColumns, inputType))
  val io = IO(new Bundle {
    // 输入信号
    val Verification_completed = Input(Bool())
    val dataA_in = Input(A_TYPE)
    val dataA_Valid_in = Input(Bool())
    /* val dataB_in = Input(B_TYPE)
    val dataB_Valid_in = Input(Bool())
    val dataD_in = Input(D_TYPE)
    val dataD_Valid_in = Input(Bool()) */
    val cntl_in = Input(new MeshWithDelaysReq(accType, tagType.cloneType, meshRows * tileRows))
    val cntl_Valid_in = Input(Bool())
    // 输出信号
    val dataA_out = Output(A_TYPE)
    val dataA_Valid_out = Output(Bool())
    /* val dataB_out = Output(B_TYPE)
    val dataB_Valid_out = Output(Bool())
    val dataD_out = Output(D_TYPE)
    val dataD_Valid_out = Output(Bool()) */
    val cntl_out = Output(new MeshWithDelaysReq(accType, tagType.cloneType, meshRows * tileRows))
    val cntl_Valid_out = Output(Bool())
  })
  
  dontTouch(io.dataA_out)
  dontTouch(io.dataA_Valid_out)
  dontTouch(io.cntl_out)
  dontTouch(io.cntl_Valid_out)
  // 双缓冲存储
  val buffers_dataA = Reg(Vec(2, Vec(meshRows * tileRows, A_TYPE)))
  val buffers_dataA_Valid = Reg(Vec(2, Vec(meshRows * tileRows, Bool())))
  val buffers_dataB = Reg(Vec(2, Vec(meshRows * tileRows, B_TYPE)))
  val buffers_dataB_Valid = Reg(Vec(2, Vec(meshRows * tileRows, Bool())))
  val buffers_dataD = Reg(Vec(2, Vec(meshRows * tileRows, D_TYPE)))
  val buffers_dataD_Valid = Reg(Vec(2, Vec(meshRows * tileRows, Bool())))
  val buffers_cntl = Reg(Vec(2, Vec(meshRows * tileRows, new MeshWithDelaysReq(accType, tagType.cloneType, meshRows * tileRows))))
  val buffers_cntl_Valid = Reg(Vec(2, Vec(meshRows * tileRows, Bool())))
  val writeCounters = RegInit(VecInit(Seq(0.U((log2Ceil(meshRows * tileRows)+1).W), 0.U((log2Ceil(meshRows * tileRows)+1).W))))
  
  val readBuffer = RegInit(0.U(1.W))
  val writeBuffer = RegInit(0.U(1.W))
  val readCounter = RegInit(0.U((log2Ceil(meshRows * tileRows)+1).W))
  val state = RegInit(false.B)

  // 输入处理
  when(io.dataA_Valid_in || io.cntl_Valid_in) {
    buffers_dataA(writeBuffer)(writeCounters(writeBuffer)) := io.dataA_in
    buffers_dataA_Valid(writeBuffer)(writeCounters(writeBuffer)) := io.cntl_Valid_in
    buffers_cntl(writeBuffer)(writeCounters(writeBuffer)) := io.cntl_in
    buffers_cntl_Valid(writeBuffer)(writeCounters(writeBuffer)) := io.cntl_Valid_in
    writeCounters(writeBuffer) := writeCounters(writeBuffer) + 1.U
  }

  // 完成信号处理
  when(io.Verification_completed && !state) {
    readBuffer := writeBuffer
    writeBuffer := ~writeBuffer
    state := true.B
    readCounter := 0.U
  }

  // 输出处理
  io.dataA_out := buffers_dataA(readBuffer)(readCounter)
  io.dataA_Valid_out := buffers_dataA_Valid(readBuffer)(readCounter)
  io.cntl_out := buffers_cntl(readBuffer)(readCounter)
  io.cntl_Valid_out := buffers_cntl_Valid(readBuffer)(readCounter)
  
  when(state) {
    readCounter := readCounter + 1.U
    when(readCounter === writeCounters(readBuffer) - 1.U) {
      // 清空已读缓冲区的有效标志
      buffers_dataA_Valid(readBuffer).foreach(_ := false.B)
      buffers_cntl_Valid(readBuffer).foreach(_ := false.B)
      writeCounters(readBuffer) := 0.U
      state := false.B
    }
  }.otherwise {
    io.dataA_Valid_out := false.B
    io.cntl_Valid_out := false.B
  }
}
