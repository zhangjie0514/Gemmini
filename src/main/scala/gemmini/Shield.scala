package gemmini

import chisel3._
import chisel3.util._
import GemminiISA._
import Util._
import org.chipsalliance.cde.config.Parameters
import midas.targetutils.PerfCounter
import firrtl.PrimOps.Sub
import scala.math.{log, ceil, pow}

class PipelinedAdderTree[T <: Data](accType: T, a: Int, b: Int, stageDelay: Int)(implicit ev: Arithmetic[T]) extends Module {
  import ev._

  require(a > 0, "a must be positive")
  require(b > 0, "b must be positive")
  require(stageDelay >= 0, "stageDelay must >= 0")
  
  private val totalInputs = a * b
  private val inputsPerAdder = math.pow(2, a).toInt
  
  // 计算理论层数
  private val exactLayers = math.log(totalInputs)/math.log(inputsPerAdder)
  // 将理论层数向上取整
  private val numLayers = if (exactLayers.isValidInt) exactLayers.toInt else math.ceil(exactLayers).toInt
  
  require(math.pow(inputsPerAdder, numLayers).toInt >= totalInputs,
    s"Cannot arrange $totalInputs inputs in $numLayers layers")
  // 计算总延迟周期 = 层数 × 每层延迟
  private val totalDelay = numLayers * stageDelay

  val io = IO(new Bundle {
    val inputs = Input(Vec(totalInputs, accType))
    val inputValid = Input(Bool()) 
    val sum = Output(accType)
    val outputValid = Output(Bool())
  })

  dontTouch(io.sum)
  dontTouch(io.outputValid)
  // 构建有效性信号流水线（简单延迟链）
  private def buildValidPipeline(valid: Bool): Bool = {
    val validChain = List.fill(totalDelay)(RegInit(false.B)) // 与层级数相同的寄存器链
    validChain.foldLeft(valid) { (prev, reg) => 
      reg := prev
      reg
    }
  }
  io.outputValid := buildValidPipeline(io.inputValid)

  // 递归构建流水线层
  private def buildLayer(currentLevel: Seq[T], layer: Int): T = {
    if (layer == numLayers) {
      require(currentLevel.length == 1, "Final layer should have exactly 1 element")
      currentLevel.head
    } else {
      val paddedInputs = if (currentLevel.length % inputsPerAdder != 0) {
        val paddingNeeded = inputsPerAdder - (currentLevel.length % inputsPerAdder)
        val padding = Seq.fill(paddingNeeded)(accType.zero)
        currentLevel ++ padding
      } else {
        currentLevel
      }
      
      /* val nextLevel = paddedInputs.grouped(inputsPerAdder).map { group =>
        val sum = group.reduce(_ + _)
        RegNext(sum)
      }.toSeq */
      // 计算当前层的组合逻辑加法结果
      val summed = paddedInputs.grouped(inputsPerAdder)
        .map(_.reduce(_ + _)).toSeq
      // 添加当前层的延迟寄存器
      val delayed = if (stageDelay == 0) {
        summed  // 无寄存器（组合逻辑）
      } else {
        // 添加 stageDelay 个寄存器
        (0 until stageDelay).foldLeft(summed) { (prev, _) => 
          prev.map(x => RegNext(x)) 
        }
      }
         
      buildLayer(delayed, layer + 1)
    }
  }

   io.sum := buildLayer(io.inputs, 0)
}

class VectorMultiplier [T <: Data](cols: Int, inputType: T, accType: T) (implicit ev: Arithmetic[T]) extends Module {
  import ev._

  val io = IO(new Bundle {
    // 两个输入向量，每个包含16个8位有符号数
    val vecA = Input(Vec(cols, accType))
    val vecB = Input(Vec(cols, inputType))
    
    // 输出向量，包含16个16位有符号数乘积结果
    val products = Output(Vec(cols, accType))
  })

  // 并行计算所有对应位的乘积
  io.products.zip(io.vecA.zip(io.vecB)).foreach { 
    case (product, (a, b)) =>
      product := a * b  
  }
}

class Shield [T <: Data](cols: Int, rows: Int, inputType: T, accType: T, tileColumns: Int, meshColumns: Int, stageDelay: Int) (implicit ev: Arithmetic[T]) extends Module{
  import ev._

    val io = IO(new Bundle {
        val dataA = Input(Vec(cols, inputType))
        val dataA_valid = Input(Bool())
        val dataA_colsum = Input(Vec(cols, accType))
        val dataA_colsum_valid = Input(Bool())
        val dataD = Input(Vec(cols, inputType))
        val dataD_valid = Input(Bool())
        val dataD_rowsum = Input(Vec(cols, accType))
        val dataD_rowsum_valid = Input(Bool())

        val Result = Output(accType)
        val Result_valid = Output(Bool())
    })
    dontTouch(io.Result)
    dontTouch(io.Result_valid)
    io.Result_valid := false.B
    val Result_valid = WireInit(false.B)
    //将权重矩阵存储起来
    val dataD_reg = Reg(Vec(rows, Vec(cols, inputType)))
    val counter_1 = RegInit(0.U((log2Ceil(rows) + 1).W))
    val counter_2 = RegInit(0.U((log2Ceil(rows) + 1).W))
    val counter_3 = RegInit(0.U((log2Ceil(rows) + 1).W))
    val dataA_colsum_valid_posted = RegInit(false.B)
    when(io.dataD_valid){
        for (row <- 0 until rows) {
            dataD_reg(row)(counter_1) := io.dataD(row)
        }  
        counter_1 := counter_1 + 1.U
    }
    //将输入矩阵的行和存储起来
    val dataA_colsum_reg = Reg(Vec(rows, accType))
    when(io.dataA_colsum_valid){
        dataA_colsum_reg := io.dataA_colsum
    }
    //将权重矩阵的列和存储起来
    val dataD_rowsum_reg = Reg(Vec(cols, accType))
    val wired = Wire(Vec(cols, accType))
    for (i <- 0 until cols) {
      when (i.U < counter_1) {
      // 计算逆序索引：n-1 - i
      val reverseIdx = counter_1 - 1.U - i.U
      wired(i) := io.dataD_rowsum(reverseIdx)
      }.otherwise {
      wired(i) := accType.zero  // 非有效位置零
      }
    }
    when(io.dataD_rowsum_valid){
      dataD_rowsum_reg := wired
    }

    //实例化乘法器
    val Multiplier = Module(new VectorMultiplier(cols, inputType, accType))
    Multiplier.io.vecA := DontCare
    Multiplier.io.vecB := DontCare
    //状态机
    val idle :: state1 :: state2 :: state3 :: Nil = Enum(4)
    val MainState = RegInit(idle)
    val substate1 :: substate2 :: Nil = Enum(2)
    val SubState = RegInit(substate1)

    switch(MainState){
        is(idle){
            when(io.dataD_valid){
                MainState := state1
            }
        }
        is(state1){
            when(io.dataD_rowsum_valid){
                Multiplier.io.vecA := wired
            }.otherwise{
                Multiplier.io.vecA := dataD_rowsum_reg
            }
            Multiplier.io.vecB := io.dataA

            when(io.dataA_valid){
                Result_valid := true.B
            }
            when(io.dataA_colsum_valid){
                MainState := state2
                counter_3 := counter_1
                counter_1 := 0.U
            }
        }
        is(state2){
            when(counter_2 < (cols - 1).U){
                counter_2 := counter_2 + 1.U
                Result_valid := true.B
            }
            Multiplier.io.vecA := dataA_colsum_reg
            val wired_1 = Wire(Vec(cols, inputType))
            for (i <- 0 until cols) {
                when (i.U < counter_3) {
                val reverseIdx = counter_3 - 1.U - i.U
                wired_1(i) := dataD_reg(counter_2)(reverseIdx)
                }.otherwise {
                wired_1(i) := inputType.zero
                }
            }
            Multiplier.io.vecB := wired_1
            
            when(counter_2 === (cols - 1).U){
                Result_valid := true.B
                MainState := state3
                SubState := substate1
                counter_2 := 0.U
            }
        }
        is(state3){
            switch(SubState){
                is(substate1){
                    Multiplier.io.vecA := dataD_rowsum_reg
                    Multiplier.io.vecB := io.dataA
                    when(io.dataA_valid){
                        Result_valid := true.B
                    }
                    when(io.dataA_colsum_valid){
                        SubState := substate2
                    }
                }
                is(substate2){
                    Multiplier.io.vecA := dataA_colsum_reg
                    when(counter_2 < (cols - 1).U){
                        counter_2 := counter_2 + 1.U
                        Result_valid := true.B
                    }
                    val wired_2 = Wire(Vec(cols, inputType))
                    for (i <- 0 until cols) {
                        when (i.U < counter_3) {
                        val reverseIdx = counter_3 - 1.U - i.U
                        wired_2(i) := dataD_reg(counter_2)(reverseIdx)
                        }.otherwise {
                        wired_2(i) := inputType.zero
                        }
                    }
                    Multiplier.io.vecB := wired_2
                    when(counter_2 === (cols - 1).U){
                        Result_valid := true.B
                        SubState := substate1
                        counter_2 := 0.U
                    }
                }
            }
            when(io.dataD_valid){
                MainState := state1
            }
        }
    }
    val AdderTree = Module(new PipelinedAdderTree(accType, tileColumns, meshColumns, stageDelay))
    AdderTree.io.inputs := Multiplier.io.products
    AdderTree.io.inputValid := Result_valid
    io.Result := AdderTree.io.sum
    io.Result_valid := AdderTree.io.outputValid

}

