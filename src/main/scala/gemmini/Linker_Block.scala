package gemmini

import chisel3._
import chisel3.util._

class LookupEntry extends Bundle {
  val start_addr = UInt(16.W)
  val end_addr = UInt(16.W)
  val region2_addr = UInt(16.W)
}

class SingleBank(entries: Int) extends Module {
  val io = IO(new Bundle {
    // 写端口
    val wr_en = Input(Bool())
    val wr_addr = Input(UInt(log2Ceil(entries).W))
    val wr_data = Input(new LookupEntry)
    
    // 读端口
    val rd_addr = Input(UInt(log2Ceil(entries).W))
    val rd_data = Output(new LookupEntry)
  })

  // 存储器实例化
  val mem = SyncReadMem(entries, new LookupEntry)

  // 写逻辑
  when(io.wr_en) {
    mem.write(io.wr_addr, io.wr_data)
  }

  // 读逻辑（组合输出，实际使用时需要时序控制）
  io.rd_data := mem.read(io.rd_addr)
}

class MultiBankSystem(numBanks: Int, entriesPerBank: Int) extends Module {
  val io = IO(new Bundle {
    // 统一写端口
    val wr_en = Input(Bool())
    val wr_bank = Input(UInt(log2Ceil(numBanks).W))
    val wr_addr = Input(UInt(log2Ceil(entriesPerBank).W))
    val wr_data = Input(new LookupEntry)
    // 统一读端口（并行读取所有Bank）
    val rd_addr = Input(UInt(log2Ceil(entriesPerBank).W))
    val rd_data = Output(Vec(numBanks, new LookupEntry))
    //查询端口
    val query_valid = Input(Bool())
    val query_addr = Input(UInt(16.W))
    val found = Output(Bool())
    val result_addr = Output(UInt(16.W))
  })

  // Bank阵列实例化
  val banks = Seq.tabulate(numBanks) { i =>
    Module(new SingleBank(entriesPerBank))
  }

  // 控制逻辑
  val sIdle :: sSearch :: Nil = Enum(2)
  val state = RegInit(sIdle)
  val searchIndex = RegInit(0.U(log2Ceil(entriesPerBank).W))
  val foundReg = RegInit(false.B)
  val resultReg = RegInit(0.U(16.W))

  // 写/读逻辑连接
  banks.zipWithIndex.foreach { case (bank, idx) =>
    bank.io.wr_en := io.wr_en && (io.wr_bank === idx.U)
    bank.io.wr_addr := io.wr_addr
    bank.io.wr_data := io.wr_data
    bank.io.rd_addr := Mux(state === sSearch, searchIndex, io.rd_addr)
  }

  // 并行比较逻辑
  val currentEntries = VecInit(banks.map(_.io.rd_data))
  val matches = currentEntries.map { entry =>
    io.query_addr >= entry.start_addr && io.query_addr <= entry.end_addr
  }
  val anyMatch = matches.reduce(_ || _)
  val hitIndex = PriorityEncoder(matches)
  
  // 状态机
  switch(state) {
    is(sIdle) {
      when(io.query_valid) {
        state := sSearch
        searchIndex := 0.U
        foundReg := false.B
      }
    }
    is(sSearch) {
      when(anyMatch) {
        foundReg := true.B
        resultReg := currentEntries(hitIndex).region2_addr
        state := sIdle
      }.elsewhen(searchIndex === (entriesPerBank-1).U) {
        state := sIdle
      }.otherwise {
        searchIndex := searchIndex + 1.U
      }
    }
  }

  // 输出连接
  io.rd_data := currentEntries
  io.found := foundReg
  io.result_addr := resultReg
}