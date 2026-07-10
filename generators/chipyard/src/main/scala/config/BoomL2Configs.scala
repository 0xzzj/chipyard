package chipyard

import org.chipsalliance.cde.config.Config

private class WithBoomL2ExperimentMemory extends Config(
  new chipyard.config.WithExtMemIdBits(6) ++
  new freechips.rocketchip.subsystem.WithEdgeDataBits(256))

class BoomCPL2Config extends Config(
  new chipyard.booml2.WithBoomCoupledL2 ++
  new WithBoomL2ExperimentMemory ++
  new boom.v3.common.WithNMediumBooms(1) ++
  new chipyard.config.AbstractConfig)

class BoomInclusiveCacheConfig extends Config(
  new chipyard.booml2.WithBoomInclusiveCache ++
  new WithBoomL2ExperimentMemory ++
  new boom.v3.common.WithNMediumBooms(1) ++
  new chipyard.config.AbstractConfig)
