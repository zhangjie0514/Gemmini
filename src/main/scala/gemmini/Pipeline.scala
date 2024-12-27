package gemmini

import chisel3._
import chisel3.util._

class Pipeline[T <: Data] (gen: T, latency: Int)(comb: Seq[T => T] = Seq.fill(latency+1)((x: T) => x)) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(gen))
    val out = Decoupled(gen)
    val busy = Output(Bool())
  })

  require(comb.size == latency+1, "length of combinational is incorrect")

  if (latency == 0) {//如果延迟为零，输出直接等于输入
    io.in.ready := io.out.ready
    io.out.valid := io.in.valid
    io.out.bits := comb.head(io.in.bits)
    io.busy := io.in.valid
  } else {
    val stages = Reg(Vec(latency, gen))                      //创建了一个包含 latency 个元素的寄存器向量，每个元素的类型为 gen（即流水线处理的数据类型）
    val valids = RegInit(VecInit(Seq.fill(latency)(false.B)))//初始化为 false 的布尔向量寄存器，用于跟踪每个阶段的数据是否有效
    val stalling = VecInit(Seq.fill(latency)(false.B))       //表示每个阶段是否正在阻塞
    io.busy := io.in.valid || valids.reduce(_||_)            //指示流水线是否忙碌

    // Stall signals
    io.in.ready := !stalling.head//只有当第一个阶段不阻塞时，输入才准备好
    stalling.last := valids.last && !io.out.ready//如果最后一个阶段有效但输出不准备好，则最后一个阶段阻塞
    (stalling.init, stalling.tail, valids.init).zipped.foreach { case (s1, s2, v1) =>
      s1 := v1 && s2
    }//如果当前阶段有效并且下一个阶段阻塞，则当前阶段阻塞

    // Valid signals
    // When the pipeline stage ahead of you isn't stalling, then make yourself invalid
    io.out.valid := valids.last//输出有效信号等于最后一个阶段的有效信号
    when(io.out.ready) {       //当输出准备好时，最后一个阶段的有效信号置为假
      valids.last := false.B
    }
    (valids.init, stalling.tail).zipped.foreach { case (v1, s2) =>
      when(!s2) {              //如果下一个阶段不阻塞，当前阶段的有效信号置为假
        v1 := false.B
      }
    }
    // When the pipeline stage behind you is valid then become true
    when(io.in.fire) {         //当输入有效且准备好时，第一个阶段的有效信号置为真
      valids.head := true.B
    }
    (valids.tail, valids.init).zipped.foreach { case (v2, v1) =>
      when(v1) {               //如果当前阶段有效，下一个阶段的有效信号置为真
        v2 := true.B
      }
    }

    // Stages
    when(io.in.fire) {                   //当输入有效且准备好时，将输入数据经过组合逻辑后存入第一个阶段
      stages.head := comb.head(io.in.bits)
    }
    io.out.bits := comb.last(stages.last)//输出数据等于最后一个阶段的数据经过组合逻辑后的结果
    ((stages.tail zip stages.init) zip (stalling.tail zip comb.tail.init)).foreach { case ((st2, st1), (s2, c1)) =>
      when(!s2) {                        //对每个阶段，若不阻塞，将当前阶段的数据经过组合逻辑传递到下一阶段
        st2 := c1(st1)
      }
    }
  }
}

object Pipeline {
  def apply[T <: Data](in: ReadyValidIO[T], latency: Int, comb: Seq[T => T]): DecoupledIO[T] = {
    val p = Module(new Pipeline(in.bits.cloneType, latency)(comb))
    p.io.in <> in
    p.io.out
  }

  def apply[T <: Data](in: ReadyValidIO[T], latency: Int): DecoupledIO[T] = {
    val p = Module(new Pipeline(in.bits.cloneType, latency)())
    p.io.in <> in
    p.io.out
  }
}
