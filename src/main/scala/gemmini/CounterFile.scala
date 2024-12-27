package gemmini

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink.{TLIdentityNode}
import GemminiISA._
import Util._

// Counter Address list
object CounterEvent {
  val DISABLE = 0

  val MAIN_LD_CYCLES = 1
  val MAIN_ST_CYCLES = 2
  val MAIN_EX_CYCLES = 3
  val MAIN_LD_ST_CYCLES = 4
  val MAIN_LD_EX_CYCLES = 5
  val MAIN_ST_EX_CYCLES = 6
  val MAIN_LD_ST_EX_CYCLES = 7

  val LOAD_DMA_WAIT_CYCLE = 8//计算加载操作等待 DMA（直接内存访问）完成的时钟周期数
  val LOAD_ACTIVE_CYCLE = 9//计算加载操作处于活动状态的时钟周期数
  val LOAD_SCRATCHPAD_WAIT_CYCLE = 10//计算加载操作等待从 Scratchpad 读取数据的时钟周期数

  val STORE_DMA_WAIT_CYCLE = 11
  val STORE_ACTIVE_CYCLE = 12
  val STORE_POOLING_CYCLE = 13//计算存储操作中的池化（pooling）操作时钟周期数
  val STORE_SCRATCHPAD_WAIT_CYCLE = 14

  val DMA_TLB_MISS_CYCLE = 15//计算 DMA 发生 TLB缺失的时钟周期数
  val DMA_TLB_HIT_REQ = 16//计算 DMA 请求中的 TLB 命中次数
  val DMA_TLB_TOTAL_REQ = 17//计算 DMA 请求中的 TLB 总请求次数

  val RDMA_ACTIVE_CYCLE = 18//计算读取 DMA 处于活动状态的时钟周期数
  val RDMA_TLB_WAIT_CYCLES = 19//计算读取 DMA 等待 TLB 处理的时钟周期数
  val RDMA_TL_WAIT_CYCLES = 20//计算读取 DMA 等待总线事务的时钟周期数

  val WDMA_ACTIVE_CYCLE = 21//计算写入 DMA 处于活动状态的时钟周期数
  val WDMA_TLB_WAIT_CYCLES = 22//计算写入 DMA 等待 TLB 处理的时钟周期数
  val WDMA_TL_WAIT_CYCLES = 23//计算写入 DMA 等待总线事务的时钟周期数

  val EXE_ACTIVE_CYCLE = 24//计算执行单元处于活动状态的时钟周期数
  val EXE_FLUSH_CYCLE = 25//计算执行单元被刷新（flush）的时钟周期数
  val EXE_CONTROL_Q_BLOCK_CYCLE = 26//计算执行单元等待控制队列的时钟周期数
  val EXE_PRELOAD_HAZ_CYCLE = 27//计算执行单元因为预加载导致的 hazard（冲突）时钟周期数
  val EXE_OVERLAP_HAZ_CYCLE = 28//计算执行单元因为重叠操作导致的 hazard 时钟周期数

  val SCRATCHPAD_A_WAIT_CYCLE = 29//计算 Scratchpad A 等待时钟周期数
  val SCRATCHPAD_B_WAIT_CYCLE = 30//计算 Scratchpad B 等待时钟周期数
  val SCRATCHPAD_D_WAIT_CYCLE = 31//计算 Scratchpad D 等待时钟周期数

  val ACC_A_WAIT_CYCLE = 32//计算加速器 A 等待时钟周期数
  val ACC_B_WAIT_CYCLE = 33//计算加速器 B 等待时钟周期数
  val ACC_D_WAIT_CYCLE = 34//计算加速器 D 等待时钟周期数

  val A_GARBAGE_CYCLES = 35//计算 A 数据通路中无效操作的时钟周期数
  val B_GARBAGE_CYCLES = 36//计算 B 数据通路中无效操作的时钟周期数
  val D_GARBAGE_CYCLES = 37//计算 D 数据通路中无效操作的时钟周期数

  val IM2COL_MEM_CYCLES = 38//计算 IM2COL 操作等待内存的时钟周期数
  val IM2COL_ACTIVE_CYCLES = 39//计算 IM2COL 操作处于活动状态的时钟周期数
  val IM2COL_TRANSPOSER_WAIT_CYCLE = 40//计算 IM2COL 操作等待数据转置器的时钟周期数

  val RESERVATION_STATION_FULL_CYCLES = 41//计算预留站（reservation station）满载时的时钟周期数
  val RESERVATION_STATION_ACTIVE_CYCLES = 42//计算预留站处于活动状态的时钟周期数

  val LOOP_MATMUL_ACTIVE_CYCLES = 43//计算矩阵乘法循环处于活动状态的时钟周期数
  val TRANSPOSE_PRELOAD_UNROLLER_ACTIVE_CYCLES = 44//计算数据转置和预加载展开器处于活动状态的时钟周期数

  val n = 45//定义了计数器的总数为 45，用于代码中方便循环或索引计数器
}

object CounterExternal {
  val DISABLE = 0

  val RESERVATION_STATION_LD_COUNT = 1//计算预留站（reservation station）中 load（加载）操作的次数
  val RESERVATION_STATION_ST_COUNT = 2//计算预留站中 store（存储）操作的次数
  val RESERVATION_STATION_EX_COUNT = 3//计算预留站中 execute（执行）操作的次数

  val RDMA_BYTES_REC = 4//计算从 RDMA（读取 DMA）接收到的数据字节数
  val WDMA_BYTES_SENT = 5//计算通过 WDMA（写入 DMA）发送出去的数据字节数

  val RDMA_TOTAL_LATENCY = 6//计算读取 DMA 操作的总延迟，通常用于评估读取请求的响应时间
  val WDMA_TOTAL_LATENCY = 7//计算写入 DMA 操作的总延迟，用于评估写入请求的响应时间

  val n = 8//定义计数器的总数为 8，用于方便在代码中遍历这些计数器或进行索引操作

  val EXTERNAL_WIDTH = 32//定义计数器的宽度为 32 位，表示每个计数器的数据表示范围为 32 位宽。
}

class CounterEventIO extends Bundle {
  val event_signal = Output(Vec(CounterEvent.n, Bool()))
  val external_values = Output(Vec(CounterExternal.n, UInt(CounterExternal.EXTERNAL_WIDTH.W)))
  val external_reset = Input(Bool())//是否要重置外部计数器

  // Connect Event Signal
  private var connected = Array.fill(CounterEvent.n)(false)//用于跟踪每个event_signal是否已经被连接，初始值全为零
  def connectEventSignal(addr: Int, sig: UInt) = {
    event_signal(addr) := sig
    connected(addr) = true//表明该事件已经成功连接
  }

  // Connect Event Signal
  private var connected_external = Array.fill(CounterEvent.n)(false)//用于跟踪每个external_values是否已经被连接，初始值全为零
  def connectExternalCounter(addr: Int, ext_value: UInt) = {
    external_values(addr) := ext_value
    connected_external(addr) = true//表明该事件已经成功连接
  }

  // Collect IO from submodule
  def collect(io: CounterEventIO) = {
    io.external_reset := external_reset
    for (i <- 0 until CounterEvent.n)
      if (io.connected(i)) {
        if (connected(i))
          throw new IllegalStateException("Port " + i + " is already connected in another IO")
        else {
          connected(i) = true
          event_signal(i) := io.event_signal(i)
        }
      }
    for (i <- 0 until CounterExternal.n)
      if (io.connected_external(i)) {
        if (connected_external(i))
          throw new IllegalStateException("External counter " + i + " is already connected in another IO")
        else {
          connected_external(i) = true
          external_values(i) := io.external_values(i)
        }
      }
  }
}
//定义了一个初始化方法：将CounterEventIO中的event_signal和external_values初始化为零d
object CounterEventIO {
  def init(io: CounterEventIO) = {
    io.event_signal := 0.U.asTypeOf(io.event_signal.cloneType)
    io.external_values := 0.U.asTypeOf(io.external_values.cloneType)
  }
}

class CounterIO(nPerfCounter: Int, counterWidth: Int) extends Bundle {
  val counter_reset = Input(Bool())
  val snapshot = Input(Bool())
  val snapshot_reset = Input(Bool())
  val addr = Input(UInt(log2Ceil(nPerfCounter).W))
  val data = Output(UInt(counterWidth.W))
  val config_address = Flipped(Valid(UInt(log2Ceil(CounterEvent.n).W)))
  val external = Input(Bool())

  val event_io = Flipped(new CounterEventIO)
}

// A simple counter file. Every counter is incremented when the corresponding event signal is high on rising edge.
// There are two type of counters: Built-in counters and external counters. External counters have their value
// stored in other modules and can incremented by arbitary values.
class CounterFile(nPerfCounter: Int, counterWidth: Int) extends Module
{
  val io = IO(new CounterIO(nPerfCounter, counterWidth))

  val config_width = log2Ceil(scala.math.max(CounterEvent.n, CounterExternal.n)) + 1//计算每个配置寄存器的位宽，存放的是地址？
  val counter_config = RegInit(VecInit.tabulate(nPerfCounter)(_ => 0.U(config_width.W)))//建立一个寄存器组，个数为nPerfCounter，每一个位宽为config_width，初始化为0
  val counter_is_external = Reg(Vec(nPerfCounter, Bool()))//建立一个布尔寄存器组，指示每个计数器是否为外部计数器

  io.event_io.external_reset := io.counter_reset

  //开始一个 withReset 代码块，复位信号为模块的 reset 或 io.counter_reset。这个代码块中的寄存器会受复位信号影响。
  //"reset.asBool"是Chisel中的隐式复位信号
  withReset(reset.asBool || io.counter_reset) {
    val counter_snapshot = RegInit(VecInit.tabulate(nPerfCounter)(_ => 0.U(counterWidth.W)))//保存计数器快照
    val counters = RegInit(VecInit.tabulate(nPerfCounter)(_ => 0.U(counterWidth.W)))        //保存计数器当前值
    val snapshot_enable = RegInit(false.B) //控制是否启用快照功能

    // Function to take correct counter value.
    // If the highest bit of the config register is 1, it's an external counter; otherwise, take it from
    // local counter
    val take_value = (config: UInt, counter: UInt) => {
      // Set the width
      val external = io.event_io.external_values(config)
      val is_external = counter_is_external(io.addr)
      Mux(is_external, external, counter)
    }
    // Snapshot: In case a sequence of access instructions get interrupted (i.e. preempted by OS), it is possible
    // to take a snapshot when reading counter value by setting a bit in the instruction. All subsequent readings
    // return the values from the snapshot until it is cleared by a instruction with "clear" bit marked.
    // When the snapshot bit is set, the normal counters are still being incremented.
    when (io.snapshot_reset) {
      snapshot_enable := false.B
    } .elsewhen (io.snapshot) {
      snapshot_enable := true.B
      // Move counter values to snapshot register
      (counter_snapshot zip (counters zip counter_config)) map { case (snapshot, (counter, config)) => {
        snapshot := take_value(config, counter)
      }}
    }

    // Connect read port
    //将数据输出 io.data 设置为当前地址的计数器值。如果快照启用，返回快照值，否则返回正常计数器值
    io.data := Mux(snapshot_enable, counter_snapshot(io.addr), take_value(counter_config(io.addr), counters(io.addr)))

    // Write configuration reg
    when (io.config_address.valid) {
      counter_config(io.addr) := io.config_address.bits
      counter_is_external(io.addr) := io.external
      counters(io.addr) := 0.U
    }

    // Update signal
    //遍历所有计数器并根据事件信号更新计数器值：如果事件信号为真，更新计数器值，如果事件触发且不是配置操作，则将计数器值加 1
    ((counters zip counter_config).zipWithIndex) map { case ((counter, config), idx) => {
      when (io.event_io.event_signal(config)) {
        when (!(io.config_address.valid && io.addr === idx.U)) {
          counter := counter + 1.U
        }
      }}
    }
  }
}

class CounterController(nPerfCounter: Int, counterWidth: Int)(implicit p: Parameters) extends Module {
  val io = IO(new Bundle() {
    val in = Flipped(Decoupled(new RoCCCommand))
    val out = Decoupled(new RoCCResponse)
    val event_io = Flipped(new CounterEventIO)
  })

  if (nPerfCounter > 0) {
    val nCounterIndexBit = log2Ceil(nPerfCounter)//索引计数器所需要的位宽

    val counterfile = Module(new CounterFile(nPerfCounter: Int, counterWidth: Int))
    counterfile.io.event_io <> io.event_io

    val out_reg = Reg(io.out.bits.cloneType)
    val out_valid_reg = RegInit(false.B)

    // Decode access option (assume 8 counters)
    // rs1[0] = Global counter value reset
    // rs1[1] = Snapshot reset
    // rs1[2] = Take snapshot
    // rs1[3] = Change config
    // rs1[6:4] = Counter index
    // rs1[17:12] = new counter address for counter with index specified in rs1[6:4]
    // We can change the number of physical counters up to 256 (which is really large)
    // rs1[31] = External counter flag

    io.in.ready := !out_valid_reg//当输出无效时，输入信号就处于就绪状态
    counterfile.io.addr := io.in.bits.rs1(nCounterIndexBit + 3, 4)
    counterfile.io.counter_reset := io.in.bits.rs1(0) & io.in.fire
    counterfile.io.snapshot_reset := io.in.bits.rs1(1) & io.in.fire
    counterfile.io.snapshot := io.in.bits.rs1(2) & io.in.fire
    counterfile.io.config_address.valid := io.in.bits.rs1(3) & io.in.fire
    counterfile.io.config_address.bits := io.in.bits.rs1(17, 12)
    counterfile.io.external := io.in.bits.rs1(31)

    when (io.out.fire) {
      out_valid_reg := false.B
    } .elsewhen (io.in.fire) {
      out_valid_reg := true.B
      out_reg.rd := io.in.bits.inst.rd
      out_reg.data := 0.U
      out_reg.data := counterfile.io.data
    }

    io.out.valid := out_valid_reg
    io.out.bits := out_reg
  } else {
    io <> DontCare
  }
}
