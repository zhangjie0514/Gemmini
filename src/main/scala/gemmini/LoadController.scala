
package gemmini

import chisel3._
import chisel3.util._
import GemminiISA._
import Util._
import org.chipsalliance.cde.config.Parameters
import midas.targetutils.PerfCounter

// TODO we need to check for WAW errors here
// TODO deal with errors when reading scratchpad responses
class LoadController[T <: Data, U <: Data, V <: Data](config: GemminiArrayConfig[T, U, V], coreMaxAddrBits: Int,
                                                      local_addr_t: LocalAddr)
                               (implicit p: Parameters) extends Module {
  import config._

  val io = IO(new Bundle {
    val cmd = Flipped(Decoupled(new GemminiCmd(reservation_station_entries)))

    val dma = new ScratchpadReadMemIO(local_addr_t, mvin_scale_t_bits)

    val completed = Decoupled(UInt(log2Up(reservation_station_entries).W)) //6.W

    val busy = Output(Bool())

    val counter = new CounterEventIO()
  })

  val waiting_for_command :: waiting_for_dma_req_ready :: sending_rows :: Nil = Enum(3)
  val control_state = RegInit(waiting_for_command)

  //load_states 是一个常量，表示可以同时存在的加载状态的数量。每个加载状态都有以下几个配置参数：
  //表示加载操作的步长（stride）。步长定义了在内存中连续两行数据之间的距离（以字节为单位）。
  //例如，如果步长为 stride，则表示每次加载一行数据后，下一行数据在内存中的地址是当前行地址加上 stride。
  val strides = Reg(Vec(load_states, UInt(coreMaxAddrBits.W)))
  val scales = Reg(Vec(load_states, UInt(mvin_scale_t_bits.W)))//表示加载时的缩放因子（scale），用于对加载进来的数据进行缩放
  val shrinks = Reg(Vec(load_states, Bool())) // Shrink inputs to accumulator;表示是否将输入数据缩小以适应累加器的位宽
  val block_strides = Reg(Vec(load_states, UInt(block_stride_bits.W))) // Spad stride during block move-ins;表示块加载时的步长
  val pixel_repeats = Reg(Vec(load_states, UInt(pixel_repeats_bits.W)))//表示像素重复的次数
  val block_rows = meshRows * tileRows
  val block_cols = meshColumns * tileColumns
  val row_counter = RegInit(0.U(log2Ceil(block_rows).W))//行计数器，初始化为 0。它用于跟踪当前已经加载了多少行数据。在每次加载操作中，row_counter 会递增，直到所有行都加载完成

  val cmd = Queue(io.cmd, ld_queue_length)

  val vaddr = cmd.bits.cmd.rs1//这是从指令中提取的虚拟地址（vaddr），表示要从内存中加载数据的起始地址
  val mvin_rs2 = cmd.bits.cmd.rs2.asTypeOf(new MvinRs2(mvin_rows_bits, mvin_cols_bits, local_addr_t))
  val localaddr = mvin_rs2.local_addr//这是从 mvin_rs2 中提取的本地地址（local_addr），表示数据要加载到片上存储器中的哪个地址
  val cols = mvin_rs2.num_cols//从 mvin_rs2 中提取的列数（num_cols），表示每次加载操作要加载的列数
  val rows = mvin_rs2.num_rows//从 mvin_rs2 中提取的行数（num_rows），表示每次加载操作要加载的行数

  val config_stride = cmd.bits.cmd.rs2

  val config_mvin_rs1 = cmd.bits.cmd.rs1.asTypeOf(new ConfigMvinRs1(mvin_scale_t_bits, block_stride_bits, pixel_repeats_bits))

  val config_scale = config_mvin_rs1.scale
  val config_shrink = config_mvin_rs1.shrink
  val config_block_stride = config_mvin_rs1.stride
  val config_pixel_repeats = config_mvin_rs1.pixel_repeats

  val mstatus = cmd.bits.cmd.status

  val load_state_id = MuxCase(0.U, Seq((cmd.bits.cmd.inst.funct === LOAD2_CMD) -> 1.U,
    (cmd.bits.cmd.inst.funct === LOAD3_CMD) -> 2.U))//根据指令的 funct 字段来确定加载状态的 ID
  val config_state_id = config_mvin_rs1.state_id    //从 config_mvin_rs1 中提取的配置状态 ID
  val state_id = Mux(cmd.bits.cmd.inst.funct === CONFIG_CMD, config_state_id, load_state_id)//通过 Mux 语句确定当前的状态 ID

  //根据 state_id 选择相应的加载配置参数
  val stride = strides(state_id)
  val scale = scales(state_id)
  val shrink = shrinks(state_id)
  val block_stride = block_strides(state_id)
  val pixel_repeat = pixel_repeats(state_id)

  val all_zeros = vaddr === 0.U

  //这是当前本地地址加上行计数器的结果，表示当前要加载的目标地址。每次加载一行数据后，行计数器会递增，更新目标地址
  val localaddr_plus_row_counter = localaddr + row_counter

  //表示实际要读取的行数。如果步长为零且虚拟地址不为零，则只加载一行数据；否则，加载 rows 行数据
  val actual_rows_read = Mux(stride === 0.U && !all_zeros, 1.U, rows)

  val DoConfig = cmd.bits.cmd.inst.funct === CONFIG_CMD//表示当前指令是否是 CONFIG_CMD（配置指令）
  val DoLoad = !DoConfig // TODO change this if more commands are added;不是配置指令就是加载指令了

  cmd.ready := false.B

  // Command tracker instantiation
  //表示 DMACommandTracker 可以同时跟踪的最大加载命令数量。换句话说，它定义了 LoadController 能够同时处理的最大并发 DMA 请求数
  val nCmds = (max_in_flight_mem_reqs / block_rows) + 1

  val deps_t = new Bundle {
    val rob_id = UInt(log2Up(reservation_station_entries).W)//表示当前命令在重排序缓冲区中的位置
  }

  //表示每次 DMA 请求中，读取一行数据所需的最大字节数，它通过比较三者中的最大值来确定
  val maxBytesInRowRequest = config.dma_maxbytes max (block_cols * config.inputType.getWidth / 8) max
    (block_cols * config.accType.getWidth / 8)
  //表示每次 DMA 请求中，读取整个矩阵所需的最大字节数。它是通过将块的行数（block_rows）乘以单行请求的最大字节数（maxBytesInRowRequest）计算得到
  val maxBytesInMatRequest = block_rows * maxBytesInRowRequest

  //这是一个 DMACommandTracker 模块的实例，用于跟踪和管理正在进行的 DMA 加载命令。
  val cmd_tracker = Module(new DMACommandTracker(nCmds, maxBytesInMatRequest, deps_t))

  io.busy := cmd.valid || cmd_tracker.io.busy

  // DMA IO wiring
  io.dma.req.valid := (control_state === waiting_for_command && cmd.valid && DoLoad && cmd_tracker.io.alloc.ready) ||
    control_state === waiting_for_dma_req_ready ||
    (control_state === sending_rows && row_counter =/= 0.U)
  io.dma.req.bits.vaddr := vaddr + row_counter * stride//DMA 请求的虚拟地址，表示从内存中加载数据的起始地址(行地址？)
  io.dma.req.bits.laddr := localaddr_plus_row_counter  //DMA 请求的本地地址，表示数据要加载到片上存储器中的哪个地址(行地址？)
  io.dma.req.bits.cols := cols
  io.dma.req.bits.repeats := Mux(stride === 0.U && !all_zeros, rows - 1.U, 0.U)//DMA 请求中的重复次数，表示是否需要重复加载某些行数据
  io.dma.req.bits.block_stride := block_stride
  io.dma.req.bits.scale := scale
  io.dma.req.bits.has_acc_bitwidth := localaddr_plus_row_counter.is_acc_addr && !shrink//表示加载的数据是否具有累加器的位宽
  io.dma.req.bits.all_zeros := all_zeros
  io.dma.req.bits.status := mstatus
  io.dma.req.bits.pixel_repeats := pixel_repeat

  // Command tracker IO
  cmd_tracker.io.alloc.valid := control_state === waiting_for_command && cmd.valid && DoLoad
  cmd_tracker.io.alloc.bits.bytes_to_read :=
    Mux(io.dma.req.bits.has_acc_bitwidth, cols * actual_rows_read * config.accType.getWidth.U,
      cols * actual_rows_read * config.inputType.getWidth.U) >> 3 // We replaced a very clear "/ 8.U" operation here with a ">> 3" operation, solely to satisfy Verilator's linter
  cmd_tracker.io.alloc.bits.tag.rob_id := cmd.bits.rob_id.bits
  cmd_tracker.io.request_returned.valid := io.dma.resp.fire // TODO use a bundle connect
  cmd_tracker.io.request_returned.bits.cmd_id := io.dma.resp.bits.cmd_id // TODO use a bundle connect
  cmd_tracker.io.request_returned.bits.bytes_read := io.dma.resp.bits.bytesRead
  cmd_tracker.io.cmd_completed.ready := io.completed.ready

  val cmd_id = RegEnableThru(cmd_tracker.io.alloc.bits.cmd_id, cmd_tracker.io.alloc.fire()) // TODO is this really better than a simple RegEnable?
  io.dma.req.bits.cmd_id := cmd_id

  io.completed.valid := cmd_tracker.io.cmd_completed.valid
  io.completed.bits := cmd_tracker.io.cmd_completed.bits.tag.rob_id

  io.busy := cmd.valid || cmd_tracker.io.busy

  // Row counter
  when (io.dma.req.fire) {
    row_counter := wrappingAdd(row_counter, 1.U, actual_rows_read)

    assert(block_stride >= rows)
  }

  // Control logic
  switch (control_state) {
    is (waiting_for_command) {
      when (cmd.valid) {
        when(DoConfig) {
          stride := config_stride
          scale := config_scale
          shrink := config_shrink
          block_stride := config_block_stride
          pixel_repeat := Mux(config_pixel_repeats === 0.U, 1.U, config_pixel_repeats) // TODO this default value was just added to maintain backwards compatibility. we should deprecate and remove it later
          cmd.ready := true.B
        }

        .elsewhen(DoLoad && cmd_tracker.io.alloc.fire()) {
          control_state := Mux(io.dma.req.fire, sending_rows, waiting_for_dma_req_ready)
        }
      }
    }

    is (waiting_for_dma_req_ready) {
      when (io.dma.req.fire) {
        control_state := sending_rows
      }
    }

    is (sending_rows) {
      val last_row = row_counter === 0.U || (row_counter === actual_rows_read-1.U && io.dma.req.fire)

      when (last_row) {
        control_state := waiting_for_command
        cmd.ready := true.B
      }
    }
  }

  // Optimizations based on config parameters
  //如果没有启用第一层优化，则将所有的像素重复次数设置为 1，确保每个像素只被加载一次
  if (!has_first_layer_optimizations)
    pixel_repeats.foreach(_ := 1.U)

  // Performance counter
  CounterEventIO.init(io.counter)
  io.counter.connectEventSignal(CounterEvent.LOAD_ACTIVE_CYCLE, control_state === sending_rows)
  io.counter.connectEventSignal(CounterEvent.LOAD_DMA_WAIT_CYCLE, control_state === waiting_for_dma_req_ready)
  io.counter.connectEventSignal(CounterEvent.LOAD_SCRATCHPAD_WAIT_CYCLE, io.dma.req.valid && !io.dma.req.ready)

  if (use_firesim_simulation_counters) {
    PerfCounter(io.dma.req.valid && !io.dma.req.ready, "load_dma_wait_cycle", "cycles during which load controller is waiting for DMA to be available")
  }

  // Assertions
  assert(!(cmd_tracker.io.alloc.fire() && cmd_tracker.io.alloc.bits.bytes_to_read === 0.U), "A single mvin instruction must load more than 0 bytes")
  assert(has_first_layer_optimizations.B || !(cmd.valid && DoConfig && config_pixel_repeats > 1.U), "If first-layer optimizations are not enabled, then pixel-repeats cannot be greater than 1")
}
