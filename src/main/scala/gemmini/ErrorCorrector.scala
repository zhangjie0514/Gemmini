package gemmini

import chisel3._
import chisel3.util._
import GemminiISA._
import Util._
import org.chipsalliance.cde.config.Parameters
import midas.targetutils.PerfCounter

class ErrorCorrector[T <: Data: Arithmetic](cols: Int, rows: Int, accType: T) (implicit ev: Arithmetic[T]) extends Module {
  import ev._

  val io = IO(new Bundle {
    // 输入接口
    val data_in = Input(Vec(cols, accType))  
    val row_coordinate = Input(UInt(log2Ceil(rows).W))
    val col_coordinate = Input(UInt(log2Ceil(cols).W))
    val difference = Input(accType)
    val valid_in = Input(Bool())          
    
    val data_out = Output(Vec(cols, accType))
    val valid_out = Output(Bool())
  })
  io.data_out := io.data_in
  io.valid_out := io.valid_in
  val counter = RegInit(0.U(log2Ceil(rows).W))
  when(io.valid_in){
    counter := counter + 1.U
  }
  when(io.row_coordinate === counter){
    io.data_out(io.col_coordinate) := io.data_in(io.col_coordinate) + io.difference
  }
}