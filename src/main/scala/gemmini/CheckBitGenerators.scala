package gemmini

import chisel3._
import chisel3.util._
import GemminiISA._
import Util._
import org.chipsalliance.cde.config.Parameters
import midas.targetutils.PerfCounter

class CheckBitGenerator[T <: Data: Arithmetic](inputType: T) (implicit ev: Arithmetic[T]) extends Module {
    import ev._

    val A_TYPE = Vec(16, inputType)

    val io = IO(new Bundle {
    val a = Flipped(Decoupled(A_TYPE))
    val a_out = Decoupled(A_TYPE) //要不要直接通过？或者这里直接不用？
    val col_addresult = Decoupled(A_TYPE)
    val row_addresult = Decoupled(inputType)   
  })

  io.a_out.bits := io.a.bits
  io.a_out.valid := io.a.valid
  io.a.ready := io.a_out.ready

  io.row_addresult.bits := io.a.bits.reduce(_ + _)
  io.row_addresult.valid := io.a.valid

  val sumRegs = RegInit(VecInit(Seq.fill(16)(0.S.asTypeOf(inputType)))) 
  val resultReg = RegInit(VecInit(Seq.fill(16)(0.S.asTypeOf(inputType)))) 
  val prevValid = RegNext(io.a.valid, init = false.B)

  io.col_addresult.valid := true.B
  when(io.a.valid) {
    io.row_addresult.valid := false.B
    for (i <- 0 until 16) {
    sumRegs(i) := sumRegs(i) + io.a.bits(i)
  }
}
  when(prevValid && !io.a.valid) {
    io.row_addresult.valid := false.B
    for (i <- 0 until 16) {
    resultReg(i) := sumRegs(i)  // 存储累加和结果
    sumRegs(i) := 0.S       // 清零累加和寄存器
    }
  }
  io.col_addresult.bits := resultReg

//不好用状态机
/*   object State extends ChiselEnum {
    val idle, working, done = Value
  }
  val state = RegInit(State.idle)

  val sum_regs = RegInit(VecInit(Seq.fill(16)(0.U.asTypeOf(inputType)))) //存储中间结果
  io.col_addresult.bits := sum_regs

  io.a_out.bits := io.a.bits
  io.a_out.valid := io.a.valid

  io.a.ready := state === State.idle
  io.col_addresult.valid := state === State.done
  io.row_addresult.valid := true.B

  io.row_addresult.bits := io.a.bits.reduce(_ + _)
  
  switch(state){
    is(State.idle){
        sum_regs := VecInit(Seq.fill(16)(0.U.asTypeOf(inputType)))
        when(io.a.fire){
            state := State.working
        }
    }
    is(State.working){
        //io.row_addresult.bits := io.a.bits.reduce(_ + _)
        for (i <- 0 until 16) {
        sum_regs(i) := sum_regs(i) + io.a.bits(i)
      }
      when(!io.a.valid){
        state := State.done
      }
    }
    is(State.done){
        when(io.col_addresult.fire){
            state := State.idle
        }
    }
  } */

}

class CheckBitGenerator_Mvin[T <: Data: Arithmetic](inputType: T, accType: T) (implicit ev: Arithmetic[T]) extends Module {
    import ev._

    val A_TYPE = Vec(16, inputType)
    val B_TYPE = Vec(16,accType)

    val io = IO(new Bundle {
      val in_valid = Input(Bool())
      val in_addr = Input(UInt(12.W))
      val in_data = Input(A_TYPE)
      val in_mask = Input(Vec(16, Bool()))
      val out_valid = Output(Bool())
      val out_addr = Output(UInt(12.W))
      val out_data = Output(A_TYPE)
      val out_mask = Output(Vec(16, Bool()))
      val col_addresult = Decoupled(B_TYPE)
      val row_addresult = Decoupled(accType)   
  })

  io.out_addr := io.in_addr
  io.out_data := io.in_data
  io.out_mask := io.in_mask

  val colsumRegs = RegInit(VecInit(Seq.fill(16)(0.S.asTypeOf(accType))))
  val rowsumRegs = RegInit(VecInit(Seq.fill(16)(0.S.asTypeOf(accType))))

  when(io.in_valid){
    for(i <- 0 until 16){
      when(io.in)
    }
  }
}