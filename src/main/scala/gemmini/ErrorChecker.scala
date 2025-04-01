package gemmini

import chisel3._
import chisel3.util._
import GemminiISA._
import Util._
import org.chipsalliance.cde.config.Parameters
import midas.targetutils.PerfCounter

class ErrorChecker[T <: Data: Arithmetic](cols: Int, rows: Int, inputType: T, accType: T, sp_banks: Int) (implicit ev: Arithmetic[T]) extends Module {
  import ev._

  val io = IO(new Bundle {
    // 输入接口
    val dataIn1 = Input(Vec(cols, inputType))  // 数据输入1，每个周期一个包含16个数字的数组
    val dataIn1_valid = Input(Bool())          // 数据输入1的有效信号

    val dataIn2 = Input(Vec(cols, accType))  // 数据输入2，包含列和和行和
    val dataIn2_valid = Input(Bool())        // 数据输入2的有效信号

    val last = Input(Bool())
    val addr_banks_in = Input(UInt(log2Ceil(sp_banks).W))

    // 输出接口
    val errorDetected = Output(Bool())          // 错误检测输出信号
    val Verification_completed = Output(Bool()) // 检错完成信号
    val addr_banks_out = Output(UInt(log2Ceil(sp_banks).W)) //控制完成信号发向哪一个延迟模块
  })
  
  io.errorDetected := false.B
  io.Verification_completed := false.B
  io.addr_banks_out := 0.U
  dontTouch(io.errorDetected)
  dontTouch(io.Verification_completed)
  dontTouch(io.addr_banks_out)

  val last = RegInit(false.B)
  when(io.last){
    last := true.B
  }
  val addr_banks = RegInit(0.U)
  when(io.dataIn1_valid){
    addr_banks := io.addr_banks_in
  }

  // 内部寄存器和变量定义
  val computedColSums = Reg(Vec(rows, accType)) // 计算得到的列和
  val computedRowSums = Reg(Vec(rows, accType)) // 计算得到的行和

  val inputColSums = Reg(Vec(cols, accType))             // 输入的列和
  val inputRowSums = Reg(Vec(rows, accType))             // 输入的行和

  val dataIn1Counter = RegInit(0.U((log2Ceil(rows) + 1).W))                  // 计数器，用于计数dataIn1有效周期数（0到15）
  val dataIn2Counter = RegInit(0.U(1.W))                                     // 计数器，用于计数dataIn2有效周期数（0或1）

  // 处理dataIn2（列和和行和）
  when(io.dataIn2_valid) {
    when(dataIn2Counter === 0.U) {
      // 第一周期：接收列和
      inputColSums := io.dataIn2
    } .elsewhen(dataIn2Counter === 1.U) {
      // 第二周期：接收行和
      inputRowSums := io.dataIn2
    }
    dataIn2Counter := dataIn2Counter + 1.U
  }

  // 处理dataIn1（矩阵数据）
  when(io.dataIn1_valid) {
    when(dataIn1Counter === 0.U){
      for (i <- 0 until cols) {
        computedColSums(i) := io.dataIn1(i)
      }
      computedRowSums(dataIn1Counter) := io.dataIn1.reduce(_ + _)
      dataIn1Counter := dataIn1Counter + 1.U
    }.elsewhen((dataIn1Counter > 0.U) && (dataIn1Counter < rows.U)) {

      // 更新计算得到的列和
      for (i <- 0 until cols) {
        computedColSums(i) := computedColSums(i) + io.dataIn1(i)
      }

      // 计算并存储当前行的行和
      computedRowSums(dataIn1Counter) := io.dataIn1.reduce(_ + _)

      // 增加计数器
      dataIn1Counter := dataIn1Counter + 1.U
    }
  }
  
  // 在收集完所有矩阵数据后进行比较
  when(dataIn1Counter === rows.U) {

    // 比较计算得到的列和和输入的列和
    val colSumsMatch = computedColSums.zip(inputColSums).map {
      case (computed, input) => computed === input
    }.reduce(_ && _)

    // 比较计算得到的行和和输入的行和
    val rowSumsMatch = computedRowSums.zip(inputRowSums).map {
      case (computed, input) => computed === input
    }.reduce(_ && _)

    // 如果有任何不匹配，则设置错误检测信号
    io.errorDetected := !(colSumsMatch && rowSumsMatch)
    when(last){
      io.Verification_completed := true.B
      io.addr_banks_out := addr_banks
      last := false.B
    }

    dataIn1Counter := 0.U
    dataIn2Counter := 0.U
  }
}

class ErrorChecker_MatmulResult [T <: Data: Arithmetic](cols: Int, rows: Int, accType: T, acc_bank_entries: Int, acc_banks: Int) (implicit ev: Arithmetic[T]) extends Module {
  import ev._

  val io = IO(new Bundle {
    // 输入接口
    val dataIn1 = Input(Vec(cols, accType)) // 数据输入1，每个周期一个包含16个数字的数组
    val dataIn1_valid = Input(Bool())        // 数据输入1的有效信号
    val addr_in = Input(UInt(log2Ceil(acc_bank_entries).W))           // 乘法计算结果将要被送入的地址
    val dataIn2 = Input(accType)          // 数据输入2，包含列和和行和
    val dataIn2_valid = Input(Bool())        // 数据输入2的有效信号
    val addr_banks_in = Input(UInt(log2Ceil(acc_banks).W))

    // 输出接口
    val errorDetected = Output(Bool())       // 错误检测输出信号
    val Verification_completed = Output(Bool())
    val checksum_valid = Output(Bool())
    val addr_out = Output(UInt(log2Ceil(acc_bank_entries).W))
    val checksum = Output(Vec(cols, accType))
    val addr_banks_out = Output(UInt(log2Ceil(acc_banks).W))
    // 接收A的行数
    val a_rows = Input(UInt((log2Ceil(rows) + 1).W))
    val a_rows_valid = Input(Bool())
  })
  io.addr_banks_out := io.addr_banks_in
  io.Verification_completed := false.B
  io.errorDetected := false.B
  io.addr_out := DontCare
  io.checksum := DontCare
  io.checksum_valid := false.B
  dontTouch(io.errorDetected)
  dontTouch(io.addr_out)
  dontTouch(io.checksum)
  dontTouch(io.checksum_valid)
  // 内部寄存器和变量定义
  val computedColSums = Reg(Vec(cols, accType)) // 计算得到的列和
  val computedRowSums = Reg(Vec(cols, accType)) // 计算得到的行和
  when(reset.asBool){
    for {
        j <- 0 until cols
      } {
        computedColSums(j) := accType.zero
        computedRowSums(j) := accType.zero
      }
  }
  
  val inputColSums = Reg(Vec(cols, accType))            // 输入的列和
  val inputRowSums = Reg(Vec(cols, accType))            // 输入的行和
  val inputColSums_1 = Reg(Vec(cols, accType))            // 输入的列和
  val inputRowSums_1 = Reg(Vec(cols, accType))            // 输入的行和
  when(reset.asBool){
    for {
        j <- 0 until cols
      } {
        inputColSums(j) := accType.zero
        inputRowSums(j) := accType.zero
        inputColSums_1(j) := accType.zero
        inputRowSums_1(j) := accType.zero
      }
  }

  val dataIn1Counter = RegInit(0.U((log2Ceil(rows) + 1).W))                  // 计数器，用于计数dataIn1有效周期数（0到15）
  val dataIn2Counter = RegInit(0.U((log2Ceil(rows) + 1).W))                  // 计数器，用于计数dataIn2有效周期数（0或1）
  val storingRowSum = RegInit(true.B)                   //用来切换装行和还是列和
  val switch_signal = RegInit(true.B)                   //用来实现类似双缓冲的切换

  // 有效信号边沿检测
  val Sum_validReg = RegNext(io.dataIn2_valid)
  val Sum_validNegEdge = !io.dataIn2_valid && Sum_validReg
  val MatmulResult_validReg = RegNext(io.dataIn1_valid)
  val MatmulResult_validPosEdge = io.dataIn1_valid && !MatmulResult_validReg
  val MatmulResult_validNegEdge = !io.dataIn1_valid && MatmulResult_validReg
  // 下降沿切换
  when(Sum_validNegEdge) {
    storingRowSum := !storingRowSum
    dataIn2Counter := 0.U
  }
  when(Sum_validNegEdge && !storingRowSum){
    switch_signal := !switch_signal
  }
  // 数据写入逻辑
  when(io.dataIn2_valid) {
    when(switch_signal){
      when(storingRowSum) {
        inputRowSums(dataIn2Counter) := io.dataIn2
      }.otherwise {
        inputColSums(dataIn2Counter) := io.dataIn2
      }
    }
    when(!switch_signal){
      when(storingRowSum) {
        inputRowSums_1(dataIn2Counter) := io.dataIn2
      }.otherwise {
        inputColSums_1(dataIn2Counter) := io.dataIn2
      }
    }
    dataIn2Counter := dataIn2Counter + 1.U  
  }
  // 乘法计算结果写入
  when(io.dataIn1_valid){
  for(i <- 0 until cols){
    computedColSums(i) := computedColSums(i) + io.dataIn1(i)
  }
  computedRowSums(dataIn1Counter) := io.dataIn1.reduce(_ + _)
  dataIn1Counter := dataIn1Counter + 1.U
  }
  
  //状态机用来控制输出校验结果
  val idle :: indicationResult :: Nil = Enum(2)
  val State = RegInit(idle)

  switch(State){
    is(idle){
      when(MatmulResult_validNegEdge){
        State := indicationResult
      }
    }
    is(indicationResult){
      io.Verification_completed := true.B
      // 比较计算得到的列和和输入的列和
      val colSumsMatch = {
        // 根据switch_signal选择要比较的输入源
        val selectedInputColSums = Mux(switch_signal, inputColSums_1, inputColSums)
        computedColSums.zip(selectedInputColSums).map { 
          case (computed, input) => computed === input 
        }.reduce(_ && _)
      }
      // 比较计算得到的行和和输入的行和
      val rowSumsMatch = {
        // 根据switch_signal选择要比较的输入源
        val selectedInputRowSums = Mux(switch_signal, inputRowSums_1, inputRowSums)
        computedRowSums.zip(selectedInputRowSums).map { 
          case (computed, input) => computed === input 
        }.reduce(_ && _)
      }
      // 如果有任何不匹配，则设置错误检测信号
      io.errorDetected := !(colSumsMatch && rowSumsMatch)
      computedRowSums := VecInit(Seq.fill(rows)(accType.zero))
      computedColSums := VecInit(Seq.fill(cols)(accType.zero))
      dataIn1Counter := 0.U
      when(!switch_signal){
        inputRowSums := VecInit(Seq.fill(rows)(accType.zero))
        inputColSums := VecInit(Seq.fill(cols)(accType.zero))
      }.otherwise{
        inputRowSums_1 := VecInit(Seq.fill(rows)(accType.zero))
        inputColSums_1 := VecInit(Seq.fill(cols)(accType.zero))
      }
      State := idle
    }
  }
  //还需要一个独立的状态机来控制校验和结果数据和地址的写入
  val a_rows = RegInit(0.U((log2Ceil(rows) + 1).W))
  val addr = RegInit(0.U(log2Ceil(acc_bank_entries).W))
  val counter = RegInit(0.U)
  when(io.a_rows_valid){
    a_rows := io.a_rows
  }

  when(io.dataIn1_valid){
    addr := io.addr_in
  }

  val idle_1 :: output_result :: Nil = Enum(2)
  val state_1 = RegInit(idle_1)
  switch(state_1){
    is(idle_1){
      when(MatmulResult_validPosEdge){
        state_1 := output_result
      }
    }
    is(output_result){
      io.checksum_valid := true.B
      when(counter === 0.U){
        io.addr_out := (addr / a_rows) * 2.U
        io.checksum := Mux(switch_signal, inputColSums_1, inputColSums)
        counter := 1.U
      }
      when(counter === 1.U){
        io.addr_out := ((addr / a_rows) * 2.U) + 1.U
        io.checksum := Mux(switch_signal, inputRowSums_1, inputRowSums)
        counter := 0.U
        state_1 := idle_1
      }
    }
  }
}

class ErrorChecker_forMvout[T <: Data: Arithmetic](cols: Int, rows: Int, inputType: T, accType: T, sp_banks: Int) (implicit ev: Arithmetic[T]) extends Module {
  import ev._

  val io = IO(new Bundle {
    // 输入接口
    val dataIn1 = Input(Vec(cols, accType))  // 数据输入1，每个周期一个包含16个数字的数组
    val dataIn1_valid = Input(Bool())          // 数据输入1的有效信号

    val dataIn2 = Input(Vec(cols, accType))  // 数据输入2，包含列和和行和
    val dataIn2_valid = Input(Bool())        // 数据输入2的有效信号

    val last = Input(Bool())
    val addr_banks_in = Input(UInt(log2Ceil(sp_banks).W))

    // 输出接口
    val errorDetected = Output(Bool())          // 错误检测输出信号
    val Verification_completed = Output(Bool()) // 检错完成信号
    val addr_banks_out = Output(UInt(log2Ceil(sp_banks).W)) //控制完成信号发向哪一个延迟模块
  })
  
  io.errorDetected := false.B
  io.Verification_completed := false.B
  io.addr_banks_out := 0.U
  dontTouch(io.errorDetected)
  dontTouch(io.Verification_completed)
  dontTouch(io.addr_banks_out)

  val last = RegInit(false.B)
  when(io.last){
    last := true.B
  }
  val addr_banks = RegInit(0.U)
  when(io.dataIn1_valid){
    addr_banks := io.addr_banks_in
  }

  // 内部寄存器和变量定义
  val computedColSums = Reg(Vec(rows, accType)) // 计算得到的列和
  val computedRowSums = Reg(Vec(rows, accType)) // 计算得到的行和

  val inputColSums = Reg(Vec(cols, accType))             // 输入的列和
  val inputRowSums = Reg(Vec(rows, accType))             // 输入的行和

  val dataIn1Counter = RegInit(0.U((log2Ceil(rows) + 1).W))                  // 计数器，用于计数dataIn1有效周期数（0到15）
  val dataIn2Counter = RegInit(0.U(1.W))                                     // 计数器，用于计数dataIn2有效周期数（0或1）

  // 处理dataIn2（列和和行和）
  when(io.dataIn2_valid) {
    when(dataIn2Counter === 0.U) {
      // 第一周期：接收列和
      inputColSums := io.dataIn2
    } .elsewhen(dataIn2Counter === 1.U) {
      // 第二周期：接收行和
      inputRowSums := io.dataIn2
    }
    dataIn2Counter := dataIn2Counter + 1.U
  }

  // 处理dataIn1（矩阵数据）
  when(io.dataIn1_valid) {
    when(dataIn1Counter === 0.U){
      for (i <- 0 until cols) {
        computedColSums(i) := io.dataIn1(i)
      }
      computedRowSums(dataIn1Counter) := io.dataIn1.reduce(_ + _)
      dataIn1Counter := dataIn1Counter + 1.U
    }.elsewhen((dataIn1Counter > 0.U) && (dataIn1Counter < rows.U)) {

      // 更新计算得到的列和
      for (i <- 0 until cols) {
        computedColSums(i) := computedColSums(i) + io.dataIn1(i)
      }

      // 计算并存储当前行的行和
      computedRowSums(dataIn1Counter) := io.dataIn1.reduce(_ + _)

      // 增加计数器
      dataIn1Counter := dataIn1Counter + 1.U
    }
  }

  val data_validReg = RegNext(io.dataIn1_valid)
  val data_validNegEdge = !io.dataIn1_valid && data_validReg

  // 在收集完所有矩阵数据后进行比较
  when(data_validNegEdge) {

    // 比较计算得到的列和和输入的列和
    val colSumsMatch = computedColSums.zip(inputColSums).map {
      case (computed, input) => computed === input
    }.reduce(_ && _)

    // 比较计算得到的行和和输入的行和
    val rowSumsMatch = computedRowSums.zip(inputRowSums).map {
      case (computed, input) => computed === input
    }.reduce(_ && _)

    // 如果有任何不匹配，则设置错误检测信号
    io.errorDetected := !(colSumsMatch && rowSumsMatch)
    when(last){
      io.Verification_completed := true.B
      io.addr_banks_out := addr_banks
      last := false.B
    }

    dataIn1Counter := 0.U
    dataIn2Counter := 0.U
    computedRowSums := VecInit(Seq.fill(rows)(accType.zero))
    computedColSums := VecInit(Seq.fill(cols)(accType.zero))
  }
}
