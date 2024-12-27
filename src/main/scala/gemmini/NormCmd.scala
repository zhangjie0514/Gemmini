
package gemmini

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum

object NormCmd extends ChiselEnum {
  val RESET, SUM, MEAN, VARIANCE, INV_STDDEV, MAX, SUM_EXP, INV_SUM_EXP = Value
  //RESET：重置命令
  //SUM：求和命令
  //MEAN：均值命令
  //VARIANCE：方差命令
  //INV_STDDEV：逆标准差命令
  //MAX：最大值命令
  //SUM_EXP：求和指数命令
  //INV_SUM_EXP：逆求和指数命令

  //这个方法用于判断某个命令是否会导致写入主内存的操作
  def writes_to_main_memory(cmd: Type): Bool = {
    cmd === RESET
  }
  
  //检查 cmd 是否匹配特定的值，并返回对应的命令
  def non_reset_version(cmd: Type): Type = {
    MuxCase(cmd, Seq(       //如果 cmd 的值不匹配下述任何情况，则返回原始的 cmd 值
      (cmd === MEAN) -> SUM,//如果 cmd 是 MEAN，则返回 SUM
      (cmd === MAX) -> MAX, //如果 cmd 是 MAX，则返回 MAX（保持不变）
      (cmd === INV_STDDEV) -> VARIANCE,//如果 cmd 是 INV_STDDEV，则返回 VARIANCE
      (cmd === INV_SUM_EXP) -> SUM_EXP //如果 cmd 是 INV_SUM_EXP，则返回 SUM_EXP
    ))
  }
}
