package gemmini

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum
import org.chipsalliance.cde.config.Parameters
import Util._
import midas.targetutils.PerfCounter

class TransposePreloadUnroller[T <: Data, U <: Data, V <: Data](config: GemminiArrayConfig[T, U, V])
                                                                 (implicit p: Parameters) extends Module {
  import config._
  import GemminiISA._

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new GemminiCmd(reservation_station_entries)))
    val out = Decoupled(new GemminiCmd(reservation_station_entries))
    val counter = new CounterEventIO()
  })

  object State extends ChiselEnum {
    val idle = Value
    val first_compute, second_preload = Value
  }
  import State._
  val state = RegInit(idle)

  val garbage_addr = ~0.U(32.W)

  val (q, len)  = MultiHeadedQueue(io.in, entries=2, heads=2, maxpop = 1)

  val cmds = q.bits
  val valids = q.valid
  val functs = cmds.map(_.cmd.inst.funct)

  val first_preload = valids(0) && functs(0) === PRELOAD_CMD && state === idle//判断当前是否是第一条预加载指令，并且状态为 idle（空闲状态）

  val b_transposed_and_ws = Reg(Bool())//表示当前的 B 矩阵是否已经转置，并且数据流模式是否是 Weight Stationary（WS）
  //判断是否需要展开预加载指令。如果 B 矩阵已经转置并且数据流模式是 WS，并且第二条指令是 COMPUTE_AND_FLIP_CMD，则需要展开预加载指令。
  val unroll_preload = b_transposed_and_ws && valids(1) && functs(1) === COMPUTE_AND_FLIP_CMD

  val first_preload_cmd = WireInit(cmds(0))//第一条预加载指令的副本
  first_preload_cmd.cmd.rs2 := Cat(cmds(0).cmd.rs2(63, 32), garbage_addr)//将它的 rs2 字段的低 32 位设置为垃圾地址（garbage_addr），表示这部分数据无效
  first_preload_cmd.rob_id.valid := false.B//将 ROB（Reorder Buffer）的 ID 设置为无效，表示这条指令不需要被重新排序

  val first_compute_cmd = WireInit(cmds(1))//第一条计算指令的副本
  first_compute_cmd.cmd.inst.rs1 := Cat(cmds(1).cmd.rs1(63, 32), garbage_addr)//rs1的低 32 位被设置为垃圾地址，表示这部分数据无效
  first_compute_cmd.cmd.inst.rs2 := Cat(cmds(1).cmd.rs2(63, 32), garbage_addr)//rs2的低 32 位被设置为垃圾地址，表示这部分数据无效
  first_compute_cmd.cmd.inst.funct := COMPUTE_AND_STAY_CMD//将功能码修改为 COMPUTE_AND_STAY_CMD，表示这是一条矩阵乘法指令
  first_compute_cmd.rob_id.valid := false.B//将 ROB（Reorder Buffer）的 ID 设置为无效，表示这条指令不需要被重新排序

  val second_preload_cmd = WireInit(cmds(0))//第二条预加载指令的副本
  second_preload_cmd.cmd.rs1 := Cat(cmds(0).cmd.rs1(63, 32), garbage_addr)//rs1 的低 32 位被设置为垃圾地址，表示这部分数据无效

  val config_cmd_type = cmds(0).cmd.rs1(1,0) // TODO magic numbers;从 rs1 寄存器的最低两位中提取配置信息
  val is_config = functs(0) === CONFIG_CMD && config_cmd_type === CONFIG_EX//判断当前指令是否是一条配置指令（CONFIG_CMD），并且具体是 CONFIG_EX 类型的配置指令

  io.out.valid := MuxCase(valids(0), Seq(
    first_preload -> (!b_transposed_and_ws || valids(1)),//如果当前是第一条预加载指令，并且 B 矩阵没有转置，或者有第二条指令，则发射指令
    (state > first_compute) -> true.B                    //如果状态已经进入 first_compute 或更高状态，则发射指令
  ))

  io.out.bits := MuxCase(cmds(0), Seq(
    (first_preload && unroll_preload) -> first_preload_cmd,//如果当前是第一条预加载指令，并且需要展开预加载，则发射 first_preload_cmd
    (state === first_compute) -> first_compute_cmd,        //如果状态是 first_compute，则发射 first_compute_cmd
    (state === second_preload) -> second_preload_cmd,      //如果状态是 second_preload，则发射 second_preload_cmd
  ))

  q.pop := Mux(io.out.fire && !(first_preload && unroll_preload) && state =/= first_compute, 1.U, 0.U)

  when (io.out.fire) {
    when (is_config) {                            //如果当前是配置指令（CONFIG_CMD），则更新 b_transposed_and_ws 标志
      val set_only_strides = cmds(0).cmd.rs1(7)
      when (!set_only_strides) {
        b_transposed_and_ws := ((dataflow == Dataflow.WS).B || cmds(0).cmd.rs1(2) === Dataflow.WS.id.U) && cmds(0).cmd.rs1(9)
      }
    }.elsewhen (first_preload && unroll_preload) {//如果当前是第一条预加载指令，并且需要展开预加载，则进入 first_compute 状态
      state := first_compute
    }.elsewhen (state >= first_compute) {         //如果状态已经是 first_compute，则进入下一个状态
      state := state.next
    }
  }

  CounterEventIO.init(io.counter)
  io.counter.connectEventSignal(CounterEvent.TRANSPOSE_PRELOAD_UNROLLER_ACTIVE_CYCLES, state =/= idle)
}

object TransposePreloadUnroller {
  def apply[T <: Data, U <: Data, V <: Data](in: ReadyValidIO[GemminiCmd], config: GemminiArrayConfig[T, U, V], counter: CounterEventIO)(implicit p: Parameters): DecoupledIO[GemminiCmd] = {
    val mod = Module(new TransposePreloadUnroller(config))
    mod.io.in <> in
    counter.collect(mod.io.counter)
    mod.io.out
  }
}
