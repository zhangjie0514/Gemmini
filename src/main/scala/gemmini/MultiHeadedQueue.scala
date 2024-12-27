package gemmini

import chisel3._
import chisel3.util._
import Util._

class MultiHeadedQueue[T <: Data](gen: T, entries: Int, heads: Int, maxpop: Int = 2) extends Module {
  val io = IO(new Bundle {
    val enq = Flipped(Decoupled(gen))

    val deq = new Bundle {
      val valid = Output(Vec(heads, Bool())) //每个头部的有效性
      val bits = Output(Vec(heads, gen))     //输出的数据
      val pop = Input(UInt(log2Ceil((entries min maxpop) + 1).W)) //输入信号，决定了在当前时钟周期中要弹出的条目数
    }

    val len = Output(UInt(log2Ceil(entries+1).W)) //输出当前队列中的数据长度
  })

  assert(heads >= 1)

  val regs = Reg(Vec(entries, gen)) //用于存储队列数据的寄存器数组
  val raddr = RegInit(0.U((log2Ceil(entries) max 1).W)) //读指针，用于跟踪队列的读取位置
  val waddr = RegInit(0.U((log2Ceil(entries) max 1).W)) //写指针，用于跟踪队列的写入位置
  val len = RegInit(0.U(log2Ceil(entries+1).W)) //记录当前队列中的条目数

  io.enq.ready := len < entries.U //当前队列中条目数小于最大容量，可以接受新条目
  io.len := len

  for (i <- 0 until heads) {
    io.deq.valid(i) := len > i.U
    io.deq.bits(i) := regs(wrappingAdd(raddr, i.U, entries))
  }

  // Pushing
  when (io.enq.fire) {
    regs(waddr) := io.enq.bits //数据写入
    waddr := wrappingAdd(waddr, 1.U, entries) //写指针加1
    len := len + 1.U //条目数量加1； 这里为甚不需要考虑出队操作
  }

  // Popping
  when(io.deq.pop > 0.U) {
    raddr := wrappingAdd(raddr, io.deq.pop, entries)
    len := len - io.deq.pop + io.enq.fire
  }

  assert(io.deq.pop <= len && io.deq.pop <= heads.U && io.deq.pop <= maxpop.U)
}

object MultiHeadedQueue {
  def apply[T <: Data](src: ReadyValidIO[T], entries: Int, heads: Int, maxpop: Int=2) = {
    val q = Module(new MultiHeadedQueue(src.bits.cloneType, entries, heads, maxpop=maxpop))
    q.io.enq <> src
    (q.io.deq, q.io.len)
  }
}
