package gemmini

import chisel3._
import chisel3.util._
import GemminiISA._
import Util._
import org.chipsalliance.cde.config.Parameters
import midas.targetutils.PerfCounter

class Add_test[T <: Data, U <: Data, V <: Data](config: GemminiArrayConfig[T, U, V], coreMaxAddrBits: Int,
                                                      local_addr_t: LocalAddr)
                               (implicit p: Parameters) extends Module {
  import config._

  val io = IO(new Bundle {
    val cmd = Flipped(Decoupled(new GemminiCmd(reservation_station_entries)))

    val completed = Decoupled(UInt(log2Up(reservation_station_entries).W)) //6.W
  })

  //计划通过rs1、rs2、rd来传递做加法的次数，因为一个或者两个的话预计不够，workload中有的矩阵很大
  //担心乘法时钟周期太多,换成位拼接
  /* val times_0 = io.cmd.bits.cmd.inst.rs1
  val times_1 = io.cmd.bits.cmd.inst.rs2
  val times_2 = io.cmd.bits.cmd.inst.rd
  val times_total = times_0 * times_1 * times_2 */

  //val times = Cat(io.cmd.bits.cmd.inst.rs1, io.cmd.bits.cmd.inst.rs2, io.cmd.bits.cmd.inst.rd)
  val times = RegInit(0.U(64.W))
  
  val initialValues = VecInit(Seq(
    0x01.U(8.W), 0x02.U(8.W), 0x03.U(8.W), 0x04.U(8.W),
    0x05.U(8.W), 0x06.U(8.W), 0x07.U(8.W), 0x08.U(8.W),
    0x09.U(8.W), 0x0A.U(8.W), 0x0B.U(8.W), 0x0C.U(8.W),
    0x0D.U(8.W), 0x0E.U(8.W), 0x0F.U(8.W), 0x10.U(8.W)
  ))
  //planB：数字更随意
  /* val initialValues = VecInit(Seq(
    0x1A.U(8.W), 0x2B.U(8.W), 0x3C.U(8.W), 0x4D.U(8.W),
    0x5E.U(8.W), 0x6F.U(8.W), 0x70.U(8.W), 0x81.U(8.W),
    0x92.U(8.W), 0xA3.U(8.W), 0xB4.U(8.W), 0xC5.U(8.W),
    0xD6.U(8.W), 0xE7.U(8.W), 0xF8.U(8.W), 0x09.U(8.W)
  )) */
  val Nums = RegInit(initialValues)
  val Result = RegInit(0.U(13.W))
  dontTouch(Result)
  val counter = RegInit(0.U(64.W))
  val rob_id_reg = RegInit(0.U(6.W))

  //printf(p"Cycle: $counter, Result: $Result\n")

  object State extends ChiselEnum {
    val idle, working, done = Value
  }
  val state = RegInit(State.idle)

  io.cmd.ready := (state === State.idle)
  io.completed.valid := (state === State.done)
  io.completed.bits := rob_id_reg

  //状态转换
  switch(state){
    is(State.idle){
        counter := 0.U
        //io.cmd.ready := true.B
        when(io.cmd.fire){
            state := State.working
            rob_id_reg := io.cmd.bits.rob_id.bits
            times := io.cmd.bits.cmd.rs1
        }
    }
    is(State.working){
        counter := counter + 1.U
        when(counter < times){
            Result := Result + Nums.reduce(_ + _)
            printf(p"Cycle: $counter, Result: $Result\n")
        }
        when(counter === times){
            state := State.done
        }
    }
    is(State.done){
        state := State.idle
        //io.cmd.ready := true.B
        //io.completed.valid := true.B
    }
  }
}