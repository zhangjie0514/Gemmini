package gemmini

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile.{CoreBundle, CoreModule}
import freechips.rocketchip.tilelink.TLEdgeOut

import Util._

import midas.targetutils.PerfCounter

class DecoupledTLBReq(val lgMaxSize: Int)(implicit p: Parameters) extends CoreBundle {
  val tlb_req = new TLBReq(lgMaxSize)
  val status = new MStatus
}

class TLBExceptionIO extends Bundle {
  val interrupt = Output(Bool())
  val flush_retry = Input(Bool())//是否需要刷新 TLB 并重试操作
  val flush_skip = Input(Bool()) //是否需要跳过当前操作而不刷新 TLB

  def flush(dummy: Int = 0): Bool = flush_retry || flush_skip
}

// TODO can we make TLB hits only take one cycle?
class DecoupledTLB(entries: Int, maxSize: Int, use_firesim_simulation_counters: Boolean)(implicit edge: TLEdgeOut, p: Parameters)
  extends CoreModule {

  val lgMaxSize = log2Ceil(maxSize)
  val io = new Bundle {
    val req = Flipped(Valid(new DecoupledTLBReq(lgMaxSize)))
    val resp = new TLBResp
    val ptw = new TLBPTWIO

    val exp = new TLBExceptionIO

    val counter = new CounterEventIO()
  }

  val interrupt = RegInit(false.B)
  io.exp.interrupt := interrupt

  val tlb = Module(new TLB(false, lgMaxSize, TLBConfig(nSets=1, nWays=entries)))
  tlb.io.req.valid := io.req.valid
  tlb.io.req.bits := io.req.bits.tlb_req
  io.resp := tlb.io.resp
  tlb.io.kill := false.B

  tlb.io.sfence.valid := io.exp.flush()
  tlb.io.sfence.bits.rs1 := false.B
  tlb.io.sfence.bits.rs2 := false.B
  tlb.io.sfence.bits.addr := DontCare
  tlb.io.sfence.bits.asid := DontCare
  tlb.io.sfence.bits.hv := false.B
  tlb.io.sfence.bits.hg := false.B

  io.ptw <> tlb.io.ptw
  tlb.io.ptw.status := io.req.bits.status
  val exception = io.req.valid && Mux(io.req.bits.tlb_req.cmd === M_XRD, tlb.io.resp.pf.ld || tlb.io.resp.ae.ld, tlb.io.resp.pf.st || tlb.io.resp.ae.st)
  //检查当前请求是否引发了异常
  when (exception) { interrupt := true.B }
  when (interrupt && tlb.io.sfence.fire) {
    interrupt := false.B//当 interrupt 为真并且 tlb.io.sfence.fire（表示 TLB 的 SFENCE 操作完成）时，将 interrupt 信号重置为 false
  }

  assert(!io.exp.flush_retry || !io.exp.flush_skip, "TLB: flushing with both retry and skip at same time")
  //确保在同一时间不会同时进行 flush_retry 和 flush_skip 操作
  CounterEventIO.init(io.counter)
  io.counter.connectEventSignal(CounterEvent.DMA_TLB_HIT_REQ, io.req.fire && !tlb.io.resp.miss)//当请求被触发且没有 TLB 未命中时，计数一次 TLB 命中请求
  io.counter.connectEventSignal(CounterEvent.DMA_TLB_TOTAL_REQ, io.req.fire)//每当请求被触发时，计数一次总请求数
  io.counter.connectEventSignal(CounterEvent.DMA_TLB_MISS_CYCLE, tlb.io.resp.miss)//每当 TLB 未命中时，计数一次未命中周期数

  if (use_firesim_simulation_counters) {
    PerfCounter(io.req.fire && !tlb.io.resp.miss, "tlb_hits", "total number of tlb hits")
    PerfCounter(io.req.fire, "tlb_reqs", "total number of tlb reqs")
    PerfCounter(tlb.io.resp.miss, "tlb_miss_cycles", "total number of cycles where the tlb is resolving a miss")
  }
}

class FrontendTLBIO(implicit p: Parameters) extends CoreBundle {
  val lgMaxSize = log2Ceil(coreDataBytes)
  // val req = Decoupled(new TLBReq(lgMaxSize))
  val req = Valid(new DecoupledTLBReq(lgMaxSize))
  val resp = Flipped(new TLBResp)
}

class FrontendTLB(nClients: Int, entries: Int, maxSize: Int, use_tlb_register_filter: Boolean, use_firesim_simulation_counters: Boolean, use_shared_tlb: Boolean)
                 (implicit edge: TLEdgeOut, p: Parameters) extends CoreModule {

  val num_tlbs = if (use_shared_tlb) 1 else nClients//TLB 的数量。如果使用共享 TLB，则数量为 1，否则为客户端数量
  val lgMaxSize = log2Ceil(coreDataBytes)

  val io = IO(new Bundle {
    val clients = Flipped(Vec(nClients, new FrontendTLBIO))
    val ptw = Vec(num_tlbs, new TLBPTWIO)
    val exp = Vec(num_tlbs, new TLBExceptionIO)
    val counter = new CounterEventIO()
  })

  val tlbs = Seq.fill(num_tlbs)(Module(new DecoupledTLB(entries, maxSize, use_firesim_simulation_counters)))

  io.ptw <> VecInit(tlbs.map(_.io.ptw))//将 TLB 的 PTW 接口连接到 io 接口
  io.exp <> VecInit(tlbs.map(_.io.exp))//将 TLB 的异常接口连接到 io 接口

  //如果使用共享 TLB，则实例化一个轮询仲裁器（RRArbiter），用于仲裁来自多个客户端的请求
  val tlbArbOpt = if (use_shared_tlb) Some(Module(new RRArbiter(new DecoupledTLBReq(lgMaxSize), nClients))) else None
  
  //如果使用共享 TLB，设置仲裁器的输出连接到 TLB 的请求接口，并设置仲裁器的输出准备信号为高
  if (use_shared_tlb) {
    val tlbArb = tlbArbOpt.get
    val tlb = tlbs.head
    tlb.io.req.valid := tlbArb.io.out.valid
    tlb.io.req.bits := tlbArb.io.out.bits
    tlbArb.io.out.ready := true.B
  }

  io.clients.zipWithIndex.foreach { case (client, i) =>
    val last_translated_valid = RegInit(false.B)//表示上一次转换是否有效
    val last_translated_vpn = RegInit(0.U(vaddrBits.W))//上一次转换的虚拟页号（VPN）
    val last_translated_ppn = RegInit(0.U(paddrBits.W))//上一次转换的物理页号（PPN）
    
    //检查当前请求的虚拟地址是否与上一次转换的虚拟地址匹配，从而判断是否命中 L0 TLB
    val l0_tlb_hit = last_translated_valid && ((client.req.bits.tlb_req.vaddr >> pgIdxBits).asUInt === (last_translated_vpn >> pgIdxBits).asUInt)
    //计算 L0 TLB 的物理地址
    val l0_tlb_paddr = Cat(last_translated_ppn >> pgIdxBits, client.req.bits.tlb_req.vaddr(pgIdxBits-1,0))
    
    //根据是否使用共享 TLB，选择对应的 TLB 实例和请求信号
    val tlb = if (use_shared_tlb) tlbs.head else tlbs(i)
    val tlbReq = if (use_shared_tlb) tlbArbOpt.get.io.in(i).bits else tlb.io.req.bits
    val tlbReqValid = if (use_shared_tlb) tlbArbOpt.get.io.in(i).valid else tlb.io.req.valid
    val tlbReqFire = if (use_shared_tlb) tlbArbOpt.get.io.in(i).fire else tlb.io.req.fire
    
    //将客户端的请求信号传递给 TLB
    tlbReqValid := RegNext(client.req.valid && !l0_tlb_hit)
    tlbReq := RegNext(client.req.bits)
    
    //如果 TLB 请求被触发且未发生未命中，更新 L0 TLB 的状态
    when (tlbReqFire && !tlb.io.resp.miss) {
      last_translated_valid := true.B
      last_translated_vpn := tlbReq.tlb_req.vaddr
      last_translated_ppn := tlb.io.resp.paddr
    }

    //如果发生 TLB 刷新，重置 last_translated_valid
    when (tlb.io.exp.flush()) {
      last_translated_valid := false.B
    }

    //如果 TLB 请求被触发，设置客户端响应为 TLB 的响应。
    //否则，返回 DontCare，并根据 L0 TLB 的命中情况设置响应。
    when (tlbReqFire) {
      client.resp := tlb.io.resp
    }.otherwise {
      client.resp := DontCare
      client.resp.paddr := RegNext(l0_tlb_paddr)
      client.resp.miss := !RegNext(l0_tlb_hit)
    }

    // If we're not using the TLB filter register, then we set this value to always be false
    if (!use_tlb_register_filter) {
      last_translated_valid := false.B
    }
  }

  // TODO Return the sum of the TLB counters, rather than just the counters of the first TLB. This only matters if we're
  // not using the shared TLB
  tlbs.foreach(_.io.counter.external_reset := false.B)//对 tlbs 中的每个 DecoupledTLB 实例，将其 io.counter.external_reset 信号设置为 false.B
  io.counter.collect(tlbs.head.io.counter)//将第一个 TLB 实例的计数器事件收集到 FrontendTLB 的 io.counter 接口中
}
