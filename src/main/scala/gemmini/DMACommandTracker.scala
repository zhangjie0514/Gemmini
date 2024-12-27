package gemmini

import chisel3._
import chisel3.util._


// This module is meant to go inside the Load controller, where it can track which commands are currently
// in flight and which are completed
class DMACommandTracker[T <: Data](val nCmds: Int, val maxBytes: Int, tag_t: => T) extends Module {
  def cmd_id_t = UInt((log2Ceil(nCmds) max 1).W)

  val io = IO(new Bundle {
    // TODO is there an existing decoupled interface in the standard library which matches this use-case?
    val alloc = new Bundle {
      val valid = Input(Bool())
      val ready = Output(Bool())

      class BitsT(tag_t: => T, cmd_id_t: UInt) extends Bundle {
        // This was only spun off as its own class to resolve CloneType errors
        val tag = Input(tag_t.cloneType) //输入信号，表示命令的标签
        val bytes_to_read = Input(UInt(log2Up(maxBytes+1).W)) //输入信号，表示需要读取的字节数
        val cmd_id = Output(cmd_id_t.cloneType) //输出信号，表示命令的 ID
      }

      val bits = new BitsT(tag_t.cloneType, cmd_id_t.cloneType)

      def fire(dummy: Int = 0) = valid && ready
    }

    class RequestReturnedT(cmd_id_t: UInt) extends Bundle {
      // This was only spun off as its own class to resolve CloneType errors
      val bytes_read = UInt(log2Up(maxBytes+1).W) //表示已经读取的字节数
      val cmd_id = cmd_id_t.cloneType //表示命令的 ID

    }

    val request_returned = Flipped(Valid(new RequestReturnedT(cmd_id_t.cloneType)))

    class CmdCompletedT(cmd_id_t: UInt, tag_t: T) extends Bundle {
      val cmd_id = cmd_id_t.cloneType //表示命令的 ID
      val tag = tag_t.cloneType //表示命令的标签

    }

    val cmd_completed = Decoupled(new CmdCompletedT(cmd_id_t.cloneType, tag_t.cloneType))

    val busy = Output(Bool())
  })

  class Entry extends Bundle {
    val valid = Bool()
    val tag = tag_t.cloneType
    val bytes_left = UInt(log2Up(maxBytes+1).W)

    def init(dummy: Int = 0): Unit = {
      valid := false.B
    }
  }

  // val cmds = RegInit(VecInit(Seq.fill(nCmds)(entry_init)))
  val cmds = Reg(Vec(nCmds, new Entry)) //一个寄存器向量，用于存储所有命令条目
  val cmd_valids = cmds.map(_.valid)    //提取所有条目的有效性状态

  val next_empty_alloc = MuxCase(0.U, cmd_valids.zipWithIndex.map { case (v, i) => (!v) -> i.U }) //找到下一个可用的命令位置

  io.alloc.ready := !cmd_valids.reduce(_ && _) //如果有空位置，则分配器准备好
  io.alloc.bits.cmd_id := next_empty_alloc     //输出下一个空闲命令的 ID

  io.busy := cmd_valids.reduce(_ || _) //如果任何一个命令条目有效，则模块繁忙

  val cmd_completed_id = MuxCase(0.U, cmds.zipWithIndex.map { case (cmd, i) =>
    (cmd.valid && cmd.bytes_left === 0.U) -> i.U
  }) //找到第一个完成的命令
  io.cmd_completed.valid := cmds.map(cmd => cmd.valid && cmd.bytes_left === 0.U).reduce(_ || _) //如果有命令完成，则输出有效信号
  io.cmd_completed.bits.cmd_id := cmd_completed_id        //输出完成命令的 ID 
  io.cmd_completed.bits.tag := cmds(cmd_completed_id).tag //输出完成命令的标签

  //当分配器触发时，更新命令条目的状态，设置为有效，并记录标签和字节数
  when (io.alloc.fire()) {
    cmds(next_empty_alloc).valid := true.B
    cmds(next_empty_alloc).tag := io.alloc.bits.tag
    cmds(next_empty_alloc).bytes_left := io.alloc.bits.bytes_to_read
  }

  //当请求返回触发时，更新对应命令的剩余字节数。
  //断言用于确保命令条目有效且剩余字节数足够。
  when (io.request_returned.fire) {
    val cmd_id = io.request_returned.bits.cmd_id
    cmds(cmd_id).bytes_left := cmds(cmd_id).bytes_left - io.request_returned.bits.bytes_read

    assert(cmds(cmd_id).valid)
    assert(cmds(cmd_id).bytes_left >= io.request_returned.bits.bytes_read)
  }

  //当命令完成触发时，将对应命令条目设置为无效
  when (io.cmd_completed.fire) {
    cmds(io.cmd_completed.bits.cmd_id).valid := false.B
  }

  //在重置时，初始化所有命令条目，将它们设置为无效
  when (reset.asBool) {
    cmds.foreach(_.init())
  }
}

