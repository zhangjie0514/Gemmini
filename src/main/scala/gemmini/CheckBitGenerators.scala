package gemmini

import chisel3._
import chisel3.util._
import GemminiISA._
import Util._
import org.chipsalliance.cde.config.Parameters
import midas.targetutils.PerfCounter

class CheckBitGenerator [T <: Data: Arithmetic](cols: Int, rows: Int, inputType: T, accType: T) (implicit ev: Arithmetic[T]) extends Module {
  import ev._

  val io = IO(new Bundle {
    // 输入接口
    val dataIn = Input(Vec(cols, inputType))  // 一组16个8位无符号整数
    val validIn = Input(Bool())              // 输入数据的有效信号

    // 输出接口
    val rowSums = Output(Vec(cols, accType))    // 行和，最多16个32位无符号整数
    val colSums = Output(Vec(cols, accType))    // 列和，16个32位无符号整数
    val validOut = Output(Bool())                // 输出结果的有效信号
  })

  dontTouch(io.colSums)
  dontTouch(io.rowSums)
  dontTouch(io.validOut)

  // 内部寄存器和变量定义
  val maxRows = rows                               // 矩阵的最大行数
  val rowSumsReg = Reg(Vec(maxRows, accType)) // 行和寄存器
  val colSumsReg = Reg(Vec(maxRows, accType)) // 列和寄存器
  val rowCounter = RegInit(0.U((log2Ceil(rows) + 1).W))             // 用于计数有效周期数（0到15）
  val counter    = RegInit(0.U(2.W))             // 数据收集完成标志

  // 默认输出
  io.rowSums := rowSumsReg
  io.colSums := colSumsReg
  io.validOut := false.B

  when(io.validIn) {
    // 计算当前行的和并存储
    rowSumsReg(rowCounter) := io.dataIn.reduce(_ + _)

    // 更新列和
    for (i <- 0 until cols) {
      colSumsReg(i) := colSumsReg(i) + io.dataIn(i)
    }

    // 增加行计数器
    rowCounter := rowCounter + 1.U

    // 还未完成数据收集
    counter := 0.U
  } .otherwise {
    when(counter === 0.U) {
      counter := 1.U
      io.validOut := true.B
      // 准备下一次数据收集，重置寄存器
      rowCounter := 0.U
      rowSumsReg := VecInit(Seq.fill(maxRows)(accType.zero))
      colSumsReg := VecInit(Seq.fill(maxRows)(accType.zero))
    }.otherwise {
      counter := 2.U
    }
  }
}
class CheckBitGenerator_acc extends Module {
  val io = IO(new Bundle {
    // 输入接口
    val dataIn = Input(Vec(16, SInt(32.W)))   // 一组16个8位无符号整数
    val validIn = Input(Bool())              // 输入数据的有效信号

    // 输出接口
    val rowSums = Output(Vec(16, SInt(36.W)))    // 行和，最多16个32位无符号整数
    val colSums = Output(Vec(16, SInt(36.W)))    // 列和，16个32位无符号整数
    val validOut = Output(Bool())                // 输出结果的有效信号
  })

  dontTouch(io.colSums)
  dontTouch(io.rowSums)
  dontTouch(io.validOut)

  // 内部寄存器和变量定义
  val maxRows = 16                               // 矩阵的最大行数
  val rowSumsReg = Reg(Vec(maxRows, SInt(36.W))) // 行和寄存器
  val colSumsReg = Reg(Vec(maxRows, SInt(36.W))) // 列和寄存器
  val rowCounter = RegInit(0.U(5.W))             // 用于计数有效周期数（0到15）
  val counter    = RegInit(0.U(2.W))             // 数据收集完成标志

  // 默认输出
  io.rowSums := rowSumsReg
  io.colSums := colSumsReg
  io.validOut := false.B

  when(io.validIn) {
    // 计算当前行的和并存储
    rowSumsReg(rowCounter) := io.dataIn.reduce(_ + _)

    // 更新列和
    for (i <- 0 until 16) {
      colSumsReg(i) := colSumsReg(i) + io.dataIn(i)
    }

    // 增加行计数器
    rowCounter := rowCounter + 1.U

    // 还未完成数据收集
    counter := 0.U
  } .otherwise {
    when(counter === 0.U) {
      counter := 1.U
      io.validOut := true.B
      // 准备下一次数据收集，重置寄存器
      rowCounter := 0.U
      rowSumsReg := VecInit(Seq.fill(maxRows)(0.S(36.W)))
      colSumsReg := VecInit(Seq.fill(maxRows)(0.S(36.W)))
    }.otherwise {
      counter := 2.U
    }
  }
}

class HashedFirstTimeChecker(val hashSize: Int, val dataWidth: Int) extends Module {
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val num_in = Input(UInt(dataWidth.W))
    val first_time = Output(Bool())
  })

  class HashEntry extends Bundle {
    val data = UInt(dataWidth.W)
    val valid = Bool()
  }

  val hashTable = SyncReadMem(hashSize, new HashEntry)

  // 初始化过程的计数器
  val initCounter = RegInit(0.U(log2Ceil(hashSize + 1).W))
  val initDone = RegInit(false.B)

  // 初始化过程
  when (!initDone) {
    // 创建一个全零的 HashEntry
    val zeroEntry = Wire(new HashEntry)
    zeroEntry.data := 0.U
    zeroEntry.valid := false.B

    // 对 hashTable 的每个地址进行初始化
    hashTable.write(initCounter, zeroEntry)
    
    // 更新计数器
    initCounter := initCounter + 1.U

    // 当计数器达到 hashSize 时，表示初始化完成
    when (initCounter === (hashSize - 1).U) {
      initDone := true.B
    }
  }

  // 正常操作的使能信号，在初始化完成后才能进行正常操作
  val operationEnable = initDone

  // 阶段 1：计算哈希值并读取哈希表
  val hash = io.num_in(log2Ceil(hashSize)-1, 0)
  val readEntry = Wire(new HashEntry)
  readEntry := DontCare

  when (operationEnable) {
    readEntry := hashTable.read(hash, io.valid)
  }

  // 将输入信号延迟一拍，使其与读取的数据对齐
  val numReg = RegNext(io.num_in)
  val hashReg = RegNext(hash)
  val validReg = RegNext(io.valid)

  // 阶段 2：比较数据并决定是否插入
  val isFirstTime = WireDefault(false.B)

  when (operationEnable && validReg) {
    when (readEntry.valid && (readEntry.data === numReg)) {
      // 已经存在
      isFirstTime := false.B
    } .otherwise {
      // 不存在，插入新数据
      isFirstTime := true.B
      val newEntry = Wire(new HashEntry)
      newEntry.data := numReg
      newEntry.valid := true.B
      hashTable.write(hashReg, newEntry)
    }
  }

  io.first_time := isFirstTime
}

class SegmentAccumulator[T <: Data: Arithmetic](n_sp: Int, n_acc: Int, cols: Int, inputType: T, accType: T, acc_banks: Int) (implicit ev: Arithmetic[T]) extends Module {
  import ev._
  
  val io = IO(new Bundle {
    val addr = Input(UInt(log2Ceil(n_sp).W))                    // 输入地址
    val dataIn = Input(Vec(cols, inputType))                    // 输入数据
    val maskIn = Input(Vec(cols, Bool()))                       // 掩码位，表示哪些数据有效
    val validIn = Input(Bool())                                 // 总有效位 
    val addr_banks_in = Input(UInt(log2Ceil(acc_banks).W))

    val gemminiWrite = Decoupled(new Bundle {
      val addr = UInt(log2Ceil(n_acc).W)                          // 要写入 Gemmini 累加器的地址
      val addr_bank_out = UInt(log2Ceil(acc_banks).W)
      val data = Vec(cols, accType)                      // 要写入的数据
      val is_acc = Bool()
    })

    val writeDone = Output(Bool())                              // 指示写入 Gemmini 完成
  })
  dontTouch(io.gemminiWrite.bits.addr_bank_out)
  io.gemminiWrite.bits.addr_bank_out := io.addr_banks_in
  // 定义参数
  val numAccumulators = 5                                       // 累加器数量
  val ZERO_THRESHOLD = 10.U                                     // 连续 0 的阈值

  // 累加器组初始化
  val accumulators = Reg(Vec(numAccumulators, (Vec(cols, accType))))
  val accumulators_1 = Reg(Vec(numAccumulators, (Vec(cols, accType))))
  when(reset.asBool){
    for {
        i <- 0 until numAccumulators
        j <- 0 until cols
      } {
        accumulators(i)(j) := 0.S
        accumulators_1(i)(j) := 0.S
      }
  }
  // 地址表(控制累加器)
  class AddressEntry(acc_addrWidth: Int) extends Bundle {
    val valid = Bool()
    val addr = UInt(acc_addrWidth.W)
  }

  val addressTable = RegInit(VecInit(Seq.fill(numAccumulators)(0.U.asTypeOf(new AddressEntry(log2Ceil(n_acc))))))
  val addressTable_1 = RegInit(VecInit(Seq.fill(numAccumulators)(0.U.asTypeOf(new AddressEntry(log2Ceil(n_acc))))))

  // 数据段检测逻辑
  val zeroCounter = RegInit(0.U(5.W))
  val inSegment = RegInit(false.B)

  when (io.validIn) {
    zeroCounter := 0.U
    inSegment := true.B
  } .otherwise {
    when (inSegment) {
      when (zeroCounter >= ZERO_THRESHOLD) {
        inSegment := false.B
        zeroCounter := 0.U
      } .otherwise {
        zeroCounter := zeroCounter + 1.U
      }
    }
  }

  // 状态机定义
  val sIdle :: sWritingSegment :: sWritingSegment_1 :: Nil = Enum(3)
  val state = RegInit(sIdle)
  val writeIndex = RegInit(0.U(log2Ceil(numAccumulators).W))    // 用于遍历累加器进行写回

  // 函数：获取累加器索引
  def getAccumulatorIndex(addr: UInt): (UInt) = {
    val idx = Wire(UInt(log2Ceil(numAccumulators).W))
    // 默认值
    idx := 0.U

    // 命中检查
    val hitVec = Wire(Vec(numAccumulators, Bool()))
    for (i <- 0 until numAccumulators) {
      hitVec(i) := addressTable(i).valid && (addressTable(i).addr === addr)
    }
    val hit = hitVec.asUInt.orR //按位或归约，判断是否命中
    val hitIndex = PriorityEncoder(hitVec) //返回第一个true，找到索引
    
    // 空闲累加器检查
    val freeVec = Wire(Vec(numAccumulators, Bool()))
    for (i <- 0 until numAccumulators) {
      freeVec(i) := !addressTable(i).valid
    }
    val hasFree = freeVec.asUInt.orR //按位或归约，判断是否有空的累加器
    val freeIndex = PriorityEncoder(freeVec) //返回第一个空累加器索引

    when (hit) {
      // 命中累加器，返回索引
      idx := hitIndex
    } .elsewhen (hasFree) {
      // 未命中，有空闲累加器
      idx := freeIndex
      // 分配新的累加器
      addressTable(freeIndex).valid := true.B
      addressTable(freeIndex).addr := addr
    }
    // 返回结果
    idx
  }

  // 函数：获取累加器索引
  def getAccumulatorIndex_1(addr: UInt): (UInt) = {
    val idx = Wire(UInt(log2Ceil(numAccumulators).W))
    // 默认值
    idx := 0.U

    // 命中检查
    val hitVec = Wire(Vec(numAccumulators, Bool()))
    for (i <- 0 until numAccumulators) {
      hitVec(i) := addressTable_1(i).valid && (addressTable_1(i).addr === addr)
    }
    val hit = hitVec.asUInt.orR //按位或归约，判断是否命中
    val hitIndex = PriorityEncoder(hitVec) //返回第一个true，找到索引
    
    // 空闲累加器检查
    val freeVec = Wire(Vec(numAccumulators, Bool()))
    for (i <- 0 until numAccumulators) {
      freeVec(i) := !addressTable_1(i).valid
    }
    val hasFree = freeVec.asUInt.orR //按位或归约，判断是否有空的累加器
    val freeIndex = PriorityEncoder(freeVec) //返回第一个空累加器索引

    when (hit) {
      // 命中累加器，返回索引
      idx := hitIndex
    } .elsewhen (hasFree) {
      // 未命中，有空闲累加器
      idx := freeIndex
      // 分配新的累加器
      addressTable_1(freeIndex).valid := true.B
      addressTable_1(freeIndex).addr := addr
    }
    // 返回结果
    idx
  }
  
  io.writeDone := DontCare
  io.gemminiWrite.bits := DontCare
  io.gemminiWrite.valid := false.B
  io.gemminiWrite.bits.is_acc := DontCare

  val HashedFirstTimeChecker_1 = Module(new HashedFirstTimeChecker(512, 9))
  HashedFirstTimeChecker_1.io.num_in := DontCare
  HashedFirstTimeChecker_1.io.valid := false.B
  val addr_countre = RegInit(0.U(1.W))
  val addr_countre_1 = RegInit(0.U(1.W))

  val maskedData = (io.dataIn zip io.maskIn).map { case (data, mask) =>
    Mux(mask, data, inputType.zero)
  }
  
  // 主逻辑
  switch(state) {
    is(sIdle) {
      io.gemminiWrite.valid := false.B//应该可以注销掉
      io.gemminiWrite.bits := DontCare
      io.writeDone := false.B

      when (io.validIn) {
        // 计算结果索引
        val idx = getAccumulatorIndex(((io.addr/16.U) * 2.U)(8, 0))
        val idx_row = getAccumulatorIndex_1(((io.addr/16.U) * 2.U + 1.U)(8, 0))          
        // 正常累加
        for (i <- 0 until 16) {
          when (io.maskIn(i)) {
            accumulators(idx)(i) := accumulators(idx)(i) + io.dataIn(i)
          }
        }
        accumulators_1(idx_row)(io.addr % 16.U) := accumulators_1(idx_row)(io.addr % 16.U) + maskedData.reduce(_ + _)
      }

      // 检查数据段结束
      when (!inSegment && addressTable.map(_.valid).reduce(_ || _)) {
        // 数据段结束且有有效累加器，开始写回
        state := sWritingSegment
        writeIndex := 0.U
      }
    }

    is(sWritingSegment) {
      // 写回所有有效的累加器
      when (writeIndex < numAccumulators.U) {
        val idx = writeIndex
        when (addressTable(idx).valid) {
          when(addr_countre === 1.U){
          io.gemminiWrite.valid := true.B       
          io.gemminiWrite.bits.addr := addressTable(idx).addr
          io.gemminiWrite.bits.data := accumulators(idx)
          io.gemminiWrite.bits.is_acc := !HashedFirstTimeChecker_1.io.first_time
          addr_countre := 0.U
          }.otherwise{
          HashedFirstTimeChecker_1.io.num_in := addressTable(idx).addr
          HashedFirstTimeChecker_1.io.valid := true.B
          addr_countre := 1.U
          }

          when (io.gemminiWrite.fire()) {
            // 清空累加器及其地址表
            accumulators(idx).foreach(_ := 0.S)
            addressTable(idx).valid := false.B
            writeIndex := writeIndex + 1.U
          }
        } .otherwise {
          // 如果当前累加器无效，直接跳到下一个
          writeIndex := writeIndex + 1.U
        }
      } .otherwise {
        state := sWritingSegment_1
        //io.writeDone := true.B
        writeIndex := 0.U
      }
    }
    is(sWritingSegment_1){
      // 写回所有有效的累加器
      when (writeIndex < numAccumulators.U) {
        val idx = writeIndex
        when (addressTable_1(idx).valid) {

          when(addr_countre_1 === 1.U){
          io.gemminiWrite.valid := true.B       
          io.gemminiWrite.bits.addr := addressTable_1(idx).addr
          io.gemminiWrite.bits.data := accumulators_1(idx)
          io.gemminiWrite.bits.is_acc := !HashedFirstTimeChecker_1.io.first_time
          addr_countre_1 := 0.U
          }.otherwise{
          //io.gemminiWrite.bits.is_acc := !HashedFirstTimeChecker_1.io.first_time
          HashedFirstTimeChecker_1.io.num_in := addressTable_1(idx).addr
          HashedFirstTimeChecker_1.io.valid := true.B
          addr_countre_1 := 1.U
          }

          when (io.gemminiWrite.fire()) {
            // 清空累加器及其地址表
            accumulators_1(idx).foreach(_ := 0.S)
            addressTable_1(idx).valid := false.B
            writeIndex := writeIndex + 1.U
          }
        } .otherwise {
          // 如果当前累加器无效，直接跳到下一个
          writeIndex := writeIndex + 1.U
        }
      } .otherwise {
        state := sIdle
        io.writeDone := true.B
        writeIndex := 0.U
      }
    }
  }
}

//稍微简化一下
class SegmentAccumulator_Simplify[T <: Data: Arithmetic](dataWidth: Int, accWidth: Int, spad_addrWidth: Int, acc_addrWidth: Int) (implicit ev: Arithmetic[T]) extends Module {
  import ev._
  
  val io = IO(new Bundle {
    val addr = Input(UInt(spad_addrWidth.W))                    // 输入地址
    val dataIn = Input(Vec(16, SInt(dataWidth.W)))              // 输入数据
    val maskIn = Input(Vec(16, Bool()))                         // 掩码位，表示哪些数据有效
    val validIn = Input(Bool())                                 // 总有效位 

    val gemminiWrite = Decoupled(new Bundle {
      val addr = UInt(acc_addrWidth.W)                          // 要写入 Gemmini 累加器的地址
      val data = Vec(16, SInt(accWidth.W))                      // 要写入的数据
    })

    val writeDone = Output(Bool())                              // 指示写入 Gemmini 完成
  })

  dontTouch(io.gemminiWrite.bits)
  dontTouch(io.gemminiWrite.valid)
  dontTouch(io.gemminiWrite.ready)
  // 定义参数
  val numAccumulators = 4                                       // 累加器数量
  val ZERO_THRESHOLD = 15.U                                     // 连续 0 的阈值

  // 累加器组初始化
  val accumulators = RegInit(VecInit(Seq.fill(numAccumulators)(VecInit(Seq.fill(16)(0.S(accWidth.W))))))
  val accumulators_1 = RegInit(VecInit(Seq.fill(numAccumulators)(VecInit(Seq.fill(16)(0.S(accWidth.W))))))
  // 地址表(控制累加器)
  class AddressEntry(acc_addrWidth: Int) extends Bundle {
    val valid = Bool()
    val addr = UInt(acc_addrWidth.W)
  }

  val addressTable = RegInit(VecInit(Seq.fill(numAccumulators)(0.U.asTypeOf(new AddressEntry(acc_addrWidth)))))
  val addressTable_1 = RegInit(VecInit(Seq.fill(numAccumulators)(0.U.asTypeOf(new AddressEntry(acc_addrWidth)))))

  // 检测连续0的个数
  val zeroCounter = RegInit(0.U(6.W))
  val inSegment = RegInit(false.B)
  when (io.validIn) {
    zeroCounter := 0.U
    inSegment := true.B
  } .otherwise {
    when (inSegment) {
      when (zeroCounter >= ZERO_THRESHOLD) {
        inSegment := false.B
        zeroCounter := 0.U
      } .otherwise {
        zeroCounter := zeroCounter + 1.U
      }
    }
  }

  // 函数：获取累加器索引
  def getAccumulatorIndex(addr: UInt): (UInt) = {
    val idx = Wire(UInt(log2Ceil(numAccumulators).W))
    // 默认值
    idx := 0.U

    // 命中检查
    val hitVec = Wire(Vec(numAccumulators, Bool()))
    for (i <- 0 until numAccumulators) {
      hitVec(i) := addressTable(i).valid && (addressTable(i).addr === addr)
    }
    val hit = hitVec.asUInt.orR //按位或归约，判断是否命中
    val hitIndex = PriorityEncoder(hitVec) //返回第一个true，找到索引
    
    // 空闲累加器检查
    val freeVec = Wire(Vec(numAccumulators, Bool()))
    for (i <- 0 until numAccumulators) {
      freeVec(i) := !addressTable(i).valid
    }
    val hasFree = freeVec.asUInt.orR //按位或归约，判断是否有空的累加器
    val freeIndex = PriorityEncoder(freeVec) //返回第一个空累加器索引

    when (hit) {
      // 命中累加器，返回索引
      idx := hitIndex
    } .elsewhen (hasFree) {
      // 未命中，有空闲累加器
      idx := freeIndex
      // 分配新的累加器
      addressTable(freeIndex).valid := true.B
      addressTable(freeIndex).addr := addr
    }
    // 返回结果
    idx
  }

  // 函数：获取累加器索引，和上面的一模一样
  def getAccumulatorIndex_1(addr: UInt): (UInt) = {
    val idx = Wire(UInt(log2Ceil(numAccumulators).W))
    idx := 0.U
    val hitVec = Wire(Vec(numAccumulators, Bool()))
    for (i <- 0 until numAccumulators) {
      hitVec(i) := addressTable_1(i).valid && (addressTable_1(i).addr === addr)
    }
    val hit = hitVec.asUInt.orR
    val hitIndex = PriorityEncoder(hitVec)
    val freeVec = Wire(Vec(numAccumulators, Bool()))
    for (i <- 0 until numAccumulators) {
      freeVec(i) := !addressTable_1(i).valid
    }
    val hasFree = freeVec.asUInt.orR
    val freeIndex = PriorityEncoder(freeVec)

    when (hit) {
      idx := hitIndex
    } .elsewhen (hasFree) {
      idx := freeIndex
      addressTable_1(freeIndex).valid := true.B
      addressTable_1(freeIndex).addr := addr
    }
    idx
  }
  
  io.writeDone := DontCare
  io.gemminiWrite.bits := DontCare
  io.gemminiWrite.valid := false.B

  val maskedData = (io.dataIn zip io.maskIn).map { case (data, mask) =>
    Mux(mask, data, 0.S)
  }
  
  // 状态机定义
  val sIdle :: sWritingSegment :: sWritingSegment_1 :: Nil = Enum(3)
  val state = RegInit(sIdle)
  val writeIndex = RegInit(0.U((log2Ceil(numAccumulators) + 1).W))    // 用于遍历累加器进行写回

  // 主逻辑
  switch(state) {
    is(sIdle) {
      io.gemminiWrite.valid := false.B//应该可以注销掉
      io.gemminiWrite.bits := DontCare
      io.writeDone := false.B

      when (io.validIn) {
        // 计算结果索引
        val idx = getAccumulatorIndex(((io.addr/16.U) * 2.U)(8, 0))
        val idx_row = getAccumulatorIndex_1(((io.addr/16.U) * 2.U + 1.U)(8, 0))          
        // 正常累加
        for (i <- 0 until 16) {
          when (io.maskIn(i)) {
            accumulators(idx)(i) := accumulators(idx)(i) + io.dataIn(i)
          }
        }
        accumulators_1(idx_row)(io.addr % 16.U) := accumulators_1(idx_row)(io.addr % 16.U) + maskedData.reduce(_ + _)
      }

      // 检查数据段结束
      when (!inSegment && addressTable.map(_.valid).reduce(_ || _)) {
        // 数据段结束且有有效累加器，开始写回
        state := sWritingSegment
        writeIndex := 0.U
      }
    }

    is(sWritingSegment) {
      // 写回所有有效的累加器
      when (writeIndex < numAccumulators.U) {
        val idx = writeIndex
        when (addressTable(idx).valid) {
          io.gemminiWrite.valid := true.B       
          io.gemminiWrite.bits.addr := addressTable(idx).addr
          io.gemminiWrite.bits.data := accumulators(idx)

          when (io.gemminiWrite.fire()) {
            // 清空累加器及其地址表
            accumulators(idx).foreach(_ := 0.S)
            addressTable(idx).valid := false.B
            writeIndex := writeIndex + 1.U
          }
        } .otherwise {
          // 如果当前累加器无效，直接跳到下一个
          writeIndex := writeIndex + 1.U
        }
      } .otherwise {
        state := sWritingSegment_1
        //io.writeDone := true.B
        writeIndex := 0.U
      }
    }
    is(sWritingSegment_1){
      // 写回所有有效的累加器
      when (writeIndex < numAccumulators.U) {
        val idx = writeIndex
        when (addressTable_1(idx).valid) {
          io.gemminiWrite.valid := true.B       
          io.gemminiWrite.bits.addr := addressTable_1(idx).addr
          io.gemminiWrite.bits.data := accumulators_1(idx)

          when (io.gemminiWrite.fire()) {
            // 清空累加器及其地址表
            accumulators_1(idx).foreach(_ := 0.S)
            addressTable_1(idx).valid := false.B
            writeIndex := writeIndex + 1.U
          }
        } .otherwise {
          // 如果当前累加器无效，直接跳到下一个
          writeIndex := writeIndex + 1.U
        }
      } .otherwise {
        state := sIdle
        io.writeDone := true.B
        writeIndex := 0.U
      }
    }
  }
}

class SegmentAccumulator_Simplify_DoubleBuffer[T <: Data: Arithmetic](cols: Int, inputType: T, accType: T, sp_bank_entries: Int)(implicit ev: Arithmetic[T]) extends Module {
  import ev._
  
  val io = IO(new Bundle {
    val addr = Input(UInt(log2Ceil(sp_bank_entries).W))
    val dataIn = Input(Vec(cols, inputType))
    val maskIn = Input(Vec(cols * (inputType.getWidth / 8), Bool()))
    val validIn = Input(Bool())

    val gemminiWrite = Decoupled(new Bundle {
      val addr = UInt(log2Ceil(sp_bank_entries / 8).W)
      val data = Vec(cols, accType)
    })

    val writeDone = Output(Bool())
  })

  dontTouch(io.gemminiWrite.bits)
  dontTouch(io.gemminiWrite.valid)
  dontTouch(io.gemminiWrite.ready)
  // 定义参数
  val numAccumulators = 5                                       // 累加器数量
  val ZERO_THRESHOLD = 15.U                                     // 连续 0 的阈值

  // 累加器组初始化
  val accumulators_colsums = Reg(Vec(numAccumulators, (Vec(cols, accType))))
  val accumulators_rowsums = Reg(Vec(numAccumulators, (Vec(cols, accType))))
  val accumulators_colsums_1 = Reg(Vec(numAccumulators, (Vec(cols, accType))))
  val accumulators_rowsums_1 = Reg(Vec(numAccumulators, (Vec(cols, accType))))

  when(reset.asBool){
    for {
        i <- 0 until numAccumulators
        j <- 0 until cols
      } {
        accumulators_colsums(i)(j) := accType.zero
        accumulators_rowsums(i)(j) := accType.zero
        accumulators_colsums_1(i)(j) := accType.zero
        accumulators_rowsums_1(i)(j) := accType.zero
      }
  }
  // 地址表(控制累加器)
  class AddressEntry(acc_addrWidth: Int) extends Bundle {
    val valid = Bool()
    val addr = UInt(acc_addrWidth.W)
  }
  // 地址表
  val addressTable_colsums = RegInit(VecInit(Seq.fill(numAccumulators)(0.U.asTypeOf(new AddressEntry(log2Ceil(sp_bank_entries / 8))))))
  val addressTable_rowsums = RegInit(VecInit(Seq.fill(numAccumulators)(0.U.asTypeOf(new AddressEntry(log2Ceil(sp_bank_entries / 8))))))
  val addressTable_colsums_1 = RegInit(VecInit(Seq.fill(numAccumulators)(0.U.asTypeOf(new AddressEntry(log2Ceil(sp_bank_entries / 8))))))
  val addressTable_rowsums_1 = RegInit(VecInit(Seq.fill(numAccumulators)(0.U.asTypeOf(new AddressEntry(log2Ceil(sp_bank_entries / 8))))))
  // 读写指针
  val readBuffer = RegInit(false.B)
  val writeBuffer = RegInit(false.B)
  // 检测连续0的个数
  val zeroCounter = RegInit(0.U(6.W))
  val inSegment = RegInit(false.B)
  when (io.validIn) {
    zeroCounter := 0.U
    inSegment := true.B
  } .otherwise {
    when (inSegment) {
      when (zeroCounter >= ZERO_THRESHOLD) {
        inSegment := false.B
        zeroCounter := 0.U
      } .otherwise {
        zeroCounter := zeroCounter + 1.U
      }
    }
  }

  // 新增：通用函数获取索引及更新信息
  def getAccumulatorIndex(
    addr: UInt,
    addressTable: Vec[AddressEntry]
  ): (UInt, Bool, UInt) = { // 返回（索引，是否需要分配，freeIndex）
    val hitVec = Wire(Vec(numAccumulators, Bool()))
    for (i <- 0 until numAccumulators) {
      hitVec(i) := addressTable(i).valid && (addressTable(i).addr === addr)
    }
    val hit = hitVec.asUInt.orR
    val hitIndex = PriorityEncoder(hitVec)

    val freeVec = Wire(Vec(numAccumulators, Bool()))
    for (i <- 0 until numAccumulators) {
      freeVec(i) := !addressTable(i).valid
    }
    val hasFree = freeVec.asUInt.orR
    val freeIndex = PriorityEncoder(freeVec)

    val idx = Mux(hit, hitIndex, Mux(hasFree, freeIndex, 0.U)) // 默认处理无空闲情况
    (idx, !hit && hasFree, freeIndex)
  }
  // 输出初始化
  io.writeDone := DontCare
  io.gemminiWrite.bits := DontCare
  io.gemminiWrite.valid := false.B

  val maskedData = (io.dataIn zip io.maskIn).map { case (data, mask) =>
    Mux(mask, data, inputType.zero)
  }
  
  // 状态机定义
  val sIdle :: sWritingSegment :: sWritingSegment_1 :: Nil = Enum(3)
  val state = RegInit(sIdle)
  val writeIndex = RegInit(0.U((log2Ceil(numAccumulators) + 1).W))// 用于遍历累加器进行写回

  when (io.validIn) {
    // 计算两个地址
    val baseAddr = (io.addr / 16.U) * 2.U
    val addr0 = baseAddr(8, 0)
    val addr1 = (baseAddr + 1.U)(8, 0)

    // 获取索引及更新信息
    val (idx, needsAlloc, freeIdx) = getAccumulatorIndex(addr0, Mux(writeBuffer, addressTable_colsums_1, addressTable_colsums))
    val (idx_row, needsAlloc_row, freeIdx_row) = getAccumulatorIndex(addr1, Mux(writeBuffer, addressTable_rowsums_1, addressTable_rowsums))

    // 更新地址表
    when(needsAlloc) {
      when(!writeBuffer){
        addressTable_colsums(freeIdx).valid := true.B
        addressTable_colsums(freeIdx).addr := addr0
      }.elsewhen(writeBuffer){
        addressTable_colsums_1(freeIdx).valid := true.B
        addressTable_colsums_1(freeIdx).addr := addr0
      }
    }
    when(needsAlloc_row) {
      when(!writeBuffer){
        addressTable_rowsums(freeIdx_row).valid := true.B
        addressTable_rowsums(freeIdx_row).addr := addr1
      }.elsewhen(writeBuffer){
        addressTable_rowsums_1(freeIdx_row).valid := true.B
        addressTable_rowsums_1(freeIdx_row).addr := addr1
      }
    }        
    // 正常累加
    when(!writeBuffer){
      for (i <- 0 until cols) {
        when (io.maskIn(i)) {
          accumulators_colsums(idx)(i) := accumulators_colsums(idx)(i) + io.dataIn(i)
        }
      }
      accumulators_rowsums(idx_row)(io.addr % 16.U) := accumulators_rowsums(idx_row)(io.addr % 16.U) + maskedData.reduce(_ + _)
    }.elsewhen(writeBuffer){
      for (i <- 0 until cols) {
        when (io.maskIn(i)) {
          accumulators_colsums_1(idx)(i) := accumulators_colsums_1(idx)(i) + io.dataIn(i)
        }
      }
      accumulators_rowsums_1(idx_row)(io.addr % 16.U) := accumulators_rowsums_1(idx_row)(io.addr % 16.U) + maskedData.reduce(_ + _)
    }
  }
  // 主逻辑
  switch(state) {
    is(sIdle) {
      // 检查数据段结束
      when (!inSegment && ((!writeBuffer && addressTable_colsums.map(_.valid).reduce(_ || _)) || (writeBuffer && addressTable_colsums_1.map(_.valid).reduce(_ || _)))) {
        // 数据段结束且有有效累加器，开始写回
        state := sWritingSegment
        writeIndex := 0.U
        readBuffer := writeBuffer
        writeBuffer := !writeBuffer
      }
    }

    is(sWritingSegment) {
      // 写回所有有效的累加器
      when (writeIndex < numAccumulators.U) {
        val idx = writeIndex
        when(!readBuffer){
          when (addressTable_colsums(idx).valid) {
            io.gemminiWrite.valid := true.B       
            io.gemminiWrite.bits.addr := addressTable_colsums(idx).addr
            io.gemminiWrite.bits.data := accumulators_colsums(idx)
            // 清空累加器及其地址表
            when (io.gemminiWrite.fire()) { 
              accumulators_colsums(idx).foreach(_ := accType.zero)
              addressTable_colsums(idx).valid := false.B
              writeIndex := writeIndex + 1.U
            }
          } .otherwise {
            // 如果当前累加器无效，直接跳到下一个
            writeIndex := writeIndex + 1.U
          }
        }.elsewhen(readBuffer){
            when (addressTable_colsums_1(idx).valid) {
            io.gemminiWrite.valid := true.B       
            io.gemminiWrite.bits.addr := addressTable_colsums_1(idx).addr
            io.gemminiWrite.bits.data := accumulators_colsums_1(idx)
            // 清空累加器及其地址表
            when (io.gemminiWrite.fire()) { 
              accumulators_colsums_1(idx).foreach(_ := accType.zero)
              addressTable_colsums_1(idx).valid := false.B
              writeIndex := writeIndex + 1.U
            }
          } .otherwise {
            // 如果当前累加器无效，直接跳到下一个
            writeIndex := writeIndex + 1.U
          }
        }
      } .otherwise {
        state := sWritingSegment_1
        writeIndex := 0.U
      }
    }
    is(sWritingSegment_1){
      when (writeIndex < numAccumulators.U) {
        val idx = writeIndex
        when(!readBuffer){
          when (addressTable_rowsums(idx).valid) {
            io.gemminiWrite.valid := true.B       
            io.gemminiWrite.bits.addr := addressTable_rowsums(idx).addr
            io.gemminiWrite.bits.data := accumulators_rowsums(idx)
            when (io.gemminiWrite.fire()) {
              accumulators_rowsums(idx).foreach(_ := accType.zero)
              addressTable_rowsums(idx).valid := false.B
              writeIndex := writeIndex + 1.U
            }
          } .otherwise {
            writeIndex := writeIndex + 1.U
          }
        }.elsewhen(readBuffer){
          when (addressTable_rowsums_1(idx).valid) {
            io.gemminiWrite.valid := true.B       
            io.gemminiWrite.bits.addr := addressTable_rowsums_1(idx).addr
            io.gemminiWrite.bits.data := accumulators_rowsums_1(idx)
            when (io.gemminiWrite.fire()) {
              accumulators_rowsums_1(idx).foreach(_ := accType.zero)
              addressTable_rowsums_1(idx).valid := false.B
              writeIndex := writeIndex + 1.U
            }
          } .otherwise {
            writeIndex := writeIndex + 1.U
          }
        }
      } .otherwise {
        state := sIdle
        io.writeDone := true.B
        writeIndex := 0.U
      }
    }
  }

}
