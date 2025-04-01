package gemmini

import org.chipsalliance.cde.config.{Config, Parameters}
import chisel3._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem.SystemBusKey
import freechips.rocketchip.tile.BuildRoCC


object GemminiCustomConfigs {
  // Default configurations
  val defaultConfig = GemminiConfigs.defaultConfig
  val defaultFpConfig = GemminiFPConfigs.defaultFPConfig
  //val defaultFpConfig = GemminiFPConfigs.FP32DefaultConfig

  // Create your own configs here
  val baselineInferenceConfig = defaultConfig.copy(
    has_training_convs = false,
  )

  val highPerfInferenceConfig = defaultConfig.copy(
    meshRows = 32,
    meshColumns = 32,

    has_training_convs = false,

    sp_capacity = CapacityInKilobytes(512),
    acc_capacity = CapacityInKilobytes(128),
  )

  val trainingConfig = defaultFpConfig.copy(
    inputType = Float(expWidth = 8, sigWidth = 24),
    accType = Float(expWidth = 8, sigWidth = 24),

    meshRows = 8,
    meshColumns = 8,

    has_training_convs = true,
    has_max_pool =  false,

    sp_capacity = CapacityInKilobytes(512),
    acc_capacity = CapacityInKilobytes(128),
  )

  val ibertInferenceConfig = defaultConfig.copy(
    has_training_convs = false,
    has_max_pool =  false,
    has_normalizations = true,

    acc_capacity = CapacityInKilobytes(128),

  )

  val testConfig6 = defaultConfig.copy(
  meshRows = 32, meshColumns = 32,
  tileRows = 4, tileColumns = 4,
  sp_capacity = CapacityInKilobytes(2048), acc_capacity = CapacityInKilobytes(512),
  has_training_convs = false,
  // max_in_flight_mem_reqs = 64,
  // dma_maxbytes = 256,
  // dma_buswidth = 512,
  // tlb_size = 16,
  )
  val testConfig7 = defaultConfig.copy(
    has_training_convs = false,
    has_max_pool =  false,
    has_normalizations = true,
    meshRows = 8, meshColumns = 8,
    tileRows = 1, tileColumns = 1,
    sp_capacity = CapacityInKilobytes(128), acc_capacity = CapacityInKilobytes(64),
  )
  val testConfig1 = defaultConfig.copy(
    meshRows = 8, meshColumns = 8,
    tileRows = 1, tileColumns = 1,
    sp_capacity = CapacityInKilobytes(128), acc_capacity = CapacityInKilobytes(32),
  )
  val testConfig2 = defaultConfig.copy(
    meshRows = 16, meshColumns = 16,
    tileRows = 4, tileColumns = 4,
    sp_capacity = CapacityInKilobytes(1024), acc_capacity = CapacityInKilobytes(256),
  )
  val testConfig3 = defaultConfig.copy(
    has_training_convs = false,
    has_max_pool =  false,
    has_normalizations = true,
    meshRows = 16, meshColumns = 16,
    tileRows = 4, tileColumns = 4,
    sp_capacity = CapacityInKilobytes(1024), acc_capacity = CapacityInKilobytes(512),
  )
  // Specify which of your custom configs you want to build here
  val customConfig = testConfig6
  val customConfig1 = baselineInferenceConfig
  val customConfig2 = testConfig7
  val customConfig3 = testConfig1
  val customConfig4 = testConfig2
  val customConfig5 = testConfig3
}


class GemminiCustomConfig[T <: Data : Arithmetic, U <: Data, V <: Data](
  gemminiConfig: GemminiArrayConfig[T,U,V] = GemminiCustomConfigs.customConfig
  //gemminiConfig: GemminiArrayConfig[T,U,V] = GemminiCustomConfigs.defaultFpConfig
) extends Config((site, here, up) => {
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p: Parameters) => {
      implicit val q = p
      val gemmini = LazyModule(new Gemmini(gemminiConfig))
      gemmini
    }
  )
})

class GemminiCustomConfig1[T <: Data : Arithmetic, U <: Data, V <: Data](
  gemminiConfig: GemminiArrayConfig[T,U,V] = GemminiCustomConfigs.customConfig1
  //gemminiConfig: GemminiArrayConfig[T,U,V] = GemminiCustomConfigs.defaultFpConfig
) extends Config((site, here, up) => {
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p: Parameters) => {
      implicit val q = p
      val gemmini = LazyModule(new Gemmini(gemminiConfig))
      gemmini
    }
  )
})
class GemminiCustomConfig2[T <: Data : Arithmetic, U <: Data, V <: Data](
  gemminiConfig: GemminiArrayConfig[T,U,V] = GemminiCustomConfigs.customConfig2
  //gemminiConfig: GemminiArrayConfig[T,U,V] = GemminiCustomConfigs.defaultFpConfig
) extends Config((site, here, up) => {
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p: Parameters) => {
      implicit val q = p
      val gemmini = LazyModule(new Gemmini(gemminiConfig))
      gemmini
    }
  )
})
class GemminiCustomConfig3[T <: Data : Arithmetic, U <: Data, V <: Data](
  gemminiConfig: GemminiArrayConfig[T,U,V] = GemminiCustomConfigs.customConfig3
  //gemminiConfig: GemminiArrayConfig[T,U,V] = GemminiCustomConfigs.defaultFpConfig
) extends Config((site, here, up) => {
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p: Parameters) => {
      implicit val q = p
      val gemmini = LazyModule(new Gemmini(gemminiConfig))
      gemmini
    }
  )
})
class GemminiCustomConfig4[T <: Data : Arithmetic, U <: Data, V <: Data](
  gemminiConfig: GemminiArrayConfig[T,U,V] = GemminiCustomConfigs.customConfig4
  //gemminiConfig: GemminiArrayConfig[T,U,V] = GemminiCustomConfigs.defaultFpConfig
) extends Config((site, here, up) => {
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p: Parameters) => {
      implicit val q = p
      val gemmini = LazyModule(new Gemmini(gemminiConfig))
      gemmini
    }
  )
})
class GemminiCustomConfig5[T <: Data : Arithmetic, U <: Data, V <: Data](
  gemminiConfig: GemminiArrayConfig[T,U,V] = GemminiCustomConfigs.customConfig5
  //gemminiConfig: GemminiArrayConfig[T,U,V] = GemminiCustomConfigs.defaultFpConfig
) extends Config((site, here, up) => {
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p: Parameters) => {
      implicit val q = p
      val gemmini = LazyModule(new Gemmini(gemminiConfig))
      gemmini
    }
  )
})