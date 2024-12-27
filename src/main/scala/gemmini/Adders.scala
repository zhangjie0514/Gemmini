package gemmini

import chisel3._
import chisel3.util._
import GemminiISA._
import Util._
import org.chipsalliance.cde.config.Parameters
import midas.targetutils.PerfCounter

class Adder_2input_1layer extends Module{

    val io = IO(new Bundle {
    val in_1 = Input(SInt(8.W))
    val in_2 = Input(SInt(8.W))
    val sum = Output(SInt(9.W))
  })

  io.sum := io.in_2 + io.in_1 
}

class Adder_2input_2layer extends Module{

    val io = IO(new Bundle {
    val in_1 = Input(SInt(9.W))
    val in_2 = Input(SInt(9.W))
    val sum = Output(SInt(10.W))
  })

  io.sum := io.in_2 + io.in_1 
}

class Adder_2input_3layer extends Module{

    val io = IO(new Bundle {
    val in_1 = Input(SInt(10.W))
    val in_2 = Input(SInt(10.W))
    val sum = Output(SInt(11.W))
  })

  io.sum := io.in_2 + io.in_1 
}

class Adder_2input_4layer extends Module{

    val io = IO(new Bundle {
    val in_1 = Input(SInt(11.W))
    val in_2 = Input(SInt(11.W))
    val sum = Output(SInt(12.W))
  })

  io.sum := io.in_2 + io.in_1 
}

class Adder_4input_1layer extends Module{

    val io = IO(new Bundle {
    val in_1 = Input(SInt(8.W))
    val in_2 = Input(SInt(8.W))
    val in_3 = Input(SInt(8.W))
    val in_4 = Input(SInt(8.W))
    val sum = Output(SInt(10.W))
  })

  io.sum := io.in_4 + io.in_3 + io.in_2 + io.in_1 
}

class Adder_4input_2layer extends Module{

    val io = IO(new Bundle {
    val in_1 = Input(SInt(10.W))
    val in_2 = Input(SInt(10.W))
    val in_3 = Input(SInt(10.W))
    val in_4 = Input(SInt(10.W))
    val sum = Output(SInt(12.W))
  })

  io.sum := io.in_4 + io.in_3 + io.in_2 + io.in_1 
}

class AdderTreeWithoutReg_2 extends Module{
    val io = IO(new Bundle {
    val in = Input(Vec(16, SInt(8.W))) // 16 个 8 位输入
    val out = Output(SInt(12.W))      // 最终结果
  })

  // 第一层：将 16 个输入分为 8 组，每组用一个 Adder_2input 相加
  val addersLayer1 = Seq.fill(8)(Module(new Adder_2input_1layer)) // 8 个 Adder_2input
  for (i <- 0 until 8) {
    addersLayer1(i).io.in_1 := io.in(i * 2)
    addersLayer1(i).io.in_2 := io.in(i * 2 + 1)
  }

  // 第一层输出的 8 个结果
  val layer1Results = addersLayer1.map(_.io.sum)

  // 第二层：将第一层的 8 个结果分为 4 组，用 Adder_2input 相加
  val addersLayer2 = Seq.fill(4)(Module(new Adder_2input_2layer)) // 4 个 Adder_2input
  for (i <- 0 until 4) {
    addersLayer2(i).io.in_1 := layer1Results(i * 2)
    addersLayer2(i).io.in_2 := layer1Results(i * 2 + 1)
  }

  // 第二层输出的 4 个结果
  val layer2Results = addersLayer2.map(_.io.sum)

  // 第三层：将第二层的 4 个结果分为 2 组，用 Adder_2input 相加
  val addersLayer3 = Seq.fill(2)(Module(new Adder_2input_3layer)) // 2 个 Adder_2input
  for (i <- 0 until 2) {
    addersLayer3(i).io.in_1 := layer2Results(i * 2)
    addersLayer3(i).io.in_2 := layer2Results(i * 2 + 1)
  }

  // 第三层输出的 2 个结果
  val layer3Results = addersLayer3.map(_.io.sum)

  // 第四层：将第三层的 2 个结果用一个 Adder_2input 相加
  val adderLayer4 = Module(new Adder_2input_4layer)
  adderLayer4.io.in_1 := layer3Results(0)
  adderLayer4.io.in_2 := layer3Results(1)

  // 最终输出
  io.out := adderLayer4.io.sum

}

class AdderTreeWithReg_2 extends Module{
    val io = IO(new Bundle {
    val in = Input(Vec(16, SInt(8.W))) // 16 个 8 位输入
    val out = Output(SInt(12.W))      // 最终结果
  })

  // 第一层：将 16 个输入分为 8 组，每组用一个 Adder_2input 相加
  val addersLayer1 = Seq.fill(8)(Module(new Adder_2input_1layer)) // 8 个 Adder_2input
  for (i <- 0 until 8) {
    addersLayer1(i).io.in_1 := io.in(i * 2)
    addersLayer1(i).io.in_2 := io.in(i * 2 + 1)
  }

  // 第一层输出的 8 个结果
  val layer1Results = RegNext(VecInit(addersLayer1.map(_.io.sum)))

  // 第二层：将第一层的 8 个结果分为 4 组，用 Adder_2input 相加
  val addersLayer2 = Seq.fill(4)(Module(new Adder_2input_2layer)) // 4 个 Adder_2input
  for (i <- 0 until 4) {
    addersLayer2(i).io.in_1 := layer1Results(i * 2)
    addersLayer2(i).io.in_2 := layer1Results(i * 2 + 1)
  }

  // 第二层输出的 4 个结果
  val layer2Results = RegNext(VecInit(addersLayer2.map(_.io.sum)))

  // 第三层：将第二层的 4 个结果分为 2 组，用 Adder_2input 相加
  val addersLayer3 = Seq.fill(2)(Module(new Adder_2input_3layer)) // 2 个 Adder_2input
  for (i <- 0 until 2) {
    addersLayer3(i).io.in_1 := layer2Results(i * 2)
    addersLayer3(i).io.in_2 := layer2Results(i * 2 + 1)
  }

  // 第三层输出的 2 个结果
  val layer3Results = RegNext(VecInit(addersLayer3.map(_.io.sum)))

  // 第四层：将第三层的 2 个结果用一个 Adder_2input 相加
  val adderLayer4 = Module(new Adder_2input_4layer)
  adderLayer4.io.in_1 := layer3Results(0)
  adderLayer4.io.in_2 := layer3Results(1)

  // 最终输出
  io.out := adderLayer4.io.sum

}

class AdderTreeWithoutReg_4 extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec(16, SInt(8.W))) // 16 个 8 位输入
    val out = Output(SInt(12.W))      // 最终结果
  })

  // 第一层：将 16 个输入分为 4 组，每组用一个 Adder_4input 相加
  val addersLayer1 = Seq.fill(4)(Module(new Adder_4input_1layer)) // 4 个 Adder_4input
  for (i <- 0 until 4) {
    addersLayer1(i).io.in_1 := io.in(i * 4 + 0)
    addersLayer1(i).io.in_2 := io.in(i * 4 + 1)
    addersLayer1(i).io.in_3 := io.in(i * 4 + 2)
    addersLayer1(i).io.in_4 := io.in(i * 4 + 3)
  }

  // 第一层输出的 4 个结果
  val layer1Results = addersLayer1.map(_.io.sum)

  // 第二层：将第一层的 4 个结果用一个 Adder_4input 相加
  val adderLayer2 = Module(new Adder_4input_2layer)
  adderLayer2.io.in_1 := layer1Results(0)
  adderLayer2.io.in_2 := layer1Results(1)
  adderLayer2.io.in_3 := layer1Results(2)
  adderLayer2.io.in_4 := layer1Results(3)

  // 最终输出
  io.out := adderLayer2.io.sum
}

class AdderTreeWithReg_4 extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec(16, SInt(8.W))) // 16 个 8 位输入
    val out = Output(SInt(12.W))      // 最终结果
  })

  // 第一层：将 16 个输入分为 4 组，每组用一个 Adder_4input 相加
  val addersLayer1 = Seq.fill(4)(Module(new Adder_4input_1layer)) // 4 个 Adder_4input
  for (i <- 0 until 4) {
    addersLayer1(i).io.in_1 := io.in(i * 4 + 0)
    addersLayer1(i).io.in_2 := io.in(i * 4 + 1)
    addersLayer1(i).io.in_3 := io.in(i * 4 + 2)
    addersLayer1(i).io.in_4 := io.in(i * 4 + 3)
  }

  // 第一层输出的 4 个结果
  val layer1Results = RegNext(VecInit(addersLayer1.map(_.io.sum)))

  // 第二层：将第一层的 4 个结果用一个 Adder_4input 相加
  val adderLayer2 = Module(new Adder_4input_2layer)
  adderLayer2.io.in_1 := layer1Results(0)
  adderLayer2.io.in_2 := layer1Results(1)
  adderLayer2.io.in_3 := layer1Results(2)
  adderLayer2.io.in_4 := layer1Results(3)

  // 最终输出
  io.out := adderLayer2.io.sum
}