package chipyard.booml2

import chisel3._
import chisel3.util.MuxCase

import org.chipsalliance.cde.config.{Config, Parameters}
import org.chipsalliance.diplomacy.lazymodule.{InModuleBody, LazyModule, LazyModuleImp}

import freechips.rocketchip.devices.tilelink.PhysicalFilter
import freechips.rocketchip.subsystem._
import freechips.rocketchip.subsystem.CoherenceManagerWrapper.CoherenceManagerInstantiationFn
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{BundleField, ElaborationArtefacts}

import sifive.blocks.inclusivecache._

import coupledL2._
import coupledL2.prefetch.BOPParameters
import coupledL2.tl2tl.TL2TLCoupledL2
import huancun.{PrefetchField, PrefetchKey}
import utility.{
  LogUtilsOptions,
  LogUtilsOptionsKey,
  MemReqSource,
  PerfCounterOptions,
  PerfCounterOptionsKey,
  ReqSourceField,
  ReqSourceKey,
  XSPerfLevel
}

object BoomL2Experiment {
  val RefillMonitorAddress: BigInt = 0x10050000L
  val ReplacementMonitorAddress: BigInt = 0x10051000L

  val CacheBlockBytes: Int = 64
  val MemoryBeatBytes: Int = 32
  val CapacityKiB: Int = 256
}

class BoomL2MetadataAdapter(implicit p: Parameters) extends LazyModule {
  val node = TLAdapterNode(clientFn = { clients =>
    clients.v2copy(requestFields = BundleField.union(
      clients.requestFields ++ Seq(ReqSourceField(), PrefetchField())))
  })

  lazy val module = new LazyModuleImp(this) {
    node.in.zip(node.out).foreach { case ((in, edgeIn), (out, _)) =>
      Connectable.waiveUnmatched(out, in) match {
        case (sink, source) => sink :<>= source
      }

      def sourceFrom(clients: Seq[TLMasterParameters], source: UInt): Bool = {
        clients.map(_.sourceId.contains(source)).reduceOption(_ || _).getOrElse(false.B)
      }

      val dcacheClients = edgeIn.client.clients.filter { client =>
        client.name.contains("DCache") || client.nodePath.exists(_.name == "dcache.node")
      }
      val icacheClients = edgeIn.client.clients.filter { client =>
        client.name.contains("ICache") || client.nodePath.exists(_.name == "frontend.icache")
      }
      val ptwClients = edgeIn.client.clients.filter { client =>
        client.name.contains("PTW") || client.nodePath.exists(_.name.toLowerCase.contains("ptw"))
      }

      val dcacheRequest = sourceFrom(dcacheClients, in.a.bits.source)
      val icacheRequest = sourceFrom(icacheClients, in.a.bits.source)
      val ptwRequest = sourceFrom(ptwClients, in.a.bits.source)
      val atomicRequest = in.a.bits.opcode === TLMessages.ArithmeticData ||
        in.a.bits.opcode === TLMessages.LogicalData
      val hintRequest = in.a.bits.opcode === TLMessages.Hint
      val writeRequest = in.a.bits.opcode === TLMessages.PutFullData ||
        in.a.bits.opcode === TLMessages.PutPartialData || edgeIn.needT(in.a.bits)

      val dcacheSource = MuxCase(MemReqSource.CPULoadData.id.U, Seq(
        atomicRequest -> MemReqSource.CPUAtomicData.id.U,
        hintRequest -> MemReqSource.L1DataPrefetch.id.U,
        writeRequest -> MemReqSource.CPUStoreData.id.U))
      val requestSource = MuxCase(MemReqSource.NoWhere.id.U, Seq(
        ptwRequest -> MemReqSource.PTW.id.U,
        icacheRequest -> Mux(
          hintRequest,
          MemReqSource.L1InstPrefetch.id.U,
          MemReqSource.CPUInst.id.U),
        dcacheRequest -> dcacheSource))

      out.a.bits.user.lift(ReqSourceKey).foreach(_ := requestSource)
      out.a.bits.user.lift(PrefetchKey).foreach(_ := false.B)
      out.c.bits.user.lift(ReqSourceKey).foreach(_ := MemReqSource.NoWhere.id.U)
      out.c.bits.user.lift(PrefetchKey).foreach(_ := false.B)
    }
  }
}

private object BoomL2Topology {
  import BoomL2Experiment._

  def skipDCacheMMIO(client: TLMasterParameters): Option[TLMasterParameters] = {
    val dcacheMMIO = client.requestFifo &&
      client.sourceId.start % 2 == 1 &&
      client.nodePath.last.name == "dcache.node"
    if (dcacheMMIO) None else Some(client)
  }

  def attachMonitorControl(
    context: HasTileLinkLocations,
    monitor: L2LatencyMonitor,
    name: String
  )(implicit p: Parameters): Unit = {
    val cbus = context.tlBusWrapperLocationMap.lift(CBUS)
      .getOrElse(context.tlBusWrapperLocationMap(SBUS))
    monitor.controlNode := cbus.coupleTo(name) {
      TLBuffer(1) := TLFragmenter(cbus, Some(name)) := _
    }
  }

  def newMonitors(context: HasTileLinkLocations)(implicit p: Parameters) = {
    val cbus = context.tlBusWrapperLocationMap.lift(CBUS)
      .getOrElse(context.tlBusWrapperLocationMap(SBUS))
    val refill = LazyModule(new L2LatencyMonitor(
      RefillLatency,
      RefillMonitorAddress,
      cbus.beatBytes))
    val replacement = LazyModule(new L2LatencyMonitor(
      ReplacementLatency,
      ReplacementMonitorAddress,
      cbus.beatBytes))
    attachMonitorControl(context, refill, "boom_l2_refill_monitor")
    attachMonitorControl(context, replacement, "boom_l2_replacement_monitor")
    (refill, replacement)
  }

  val coupled: CoherenceManagerInstantiationFn = { context =>
    implicit val p: Parameters = context.p
    val cbus = context.tlBusWrapperLocationMap.lift(CBUS)
      .getOrElse(context.tlBusWrapperLocationMap(SBUS))
    val filter = LazyModule(new TLFilter(cfilter = skipDCacheMMIO))
    val metadata = LazyModule(new BoomL2MetadataAdapter)
    val l2 = LazyModule(new TL2TLCoupledL2)
    val cork = LazyModule(new TLCacheCork)
    val (refillMonitor, replacementMonitor) = newMonitors(context)

    metadata.node :*= filter.node
    refillMonitor.node :*= metadata.node
    l2.node :*= refillMonitor.node
    replacementMonitor.node :*= l2.node
    cork.node :*= replacementMonitor.node

    InModuleBody {
      val io = l2.module.io
      val prefetchControl = WireDefault(0.U.asTypeOf(io.pfCtrlFromCore))
      prefetchControl.l2_pf_master_en := true.B
      prefetchControl.l2_pbop_en := true.B
      prefetchControl.l2_pf_delay_latency := 0.U

      io.hartId := 0.U
      io.pfCtrlFromCore := prefetchControl
      io.debugTopDown.robTrueCommit := 0.U
      io.debugTopDown.robHeadPaddr.valid := false.B
      io.debugTopDown.robHeadPaddr.bits := 0.U
      io.l2_tlb_req.req.ready := true.B
      io.l2_tlb_req.resp.valid := false.B
      io.l2_tlb_req.resp.bits := DontCare
      io.l2_tlb_req.pmp_resp := 0.U.asTypeOf(io.l2_tlb_req.pmp_resp)
    }

    val params = p(L2ParamKey)
    val banks = p(SubsystemBankedCoherenceKey).nBanks
    ElaborationArtefacts.add("boom-cpl2.json",
      s"""{"banks":$banks,"sets_per_bank":${params.sets},"ways":${params.ways},"block_bytes":${params.blockBytes},"capacity_kib":${banks * params.sets * params.ways * params.blockBytes / 1024},"replacement":"${params.replacement}","prefetch":"PBOP"}""")

    (filter.node, cork.node, None)
  }

  val inclusive: CoherenceManagerInstantiationFn = { context =>
    implicit val p: Parameters = context.p
    val sbus = context.tlBusWrapperLocationMap(SBUS)
    val cbus = context.tlBusWrapperLocationMap.lift(CBUS).getOrElse(sbus)
    val InclusiveCacheParams(
      ways,
      sets,
      writeBytes,
      portFactor,
      memCycles,
      physicalFilter,
      hintsSkipProbe,
      bankedControl,
      ctrlAddr,
      bufInnerInterior,
      bufInnerExterior,
      bufOuterInterior,
      bufOuterExterior) = p(InclusiveCacheKey)

    val control = ctrlAddr.map { address =>
      InclusiveCacheControlParameters(
        address = address,
        beatBytes = cbus.beatBytes,
        bankedControl = bankedControl)
    }
    val l2 = LazyModule(new InclusiveCache(
      CacheParameters(
        level = 2,
        ways = ways,
        sets = sets,
        blockBytes = sbus.blockBytes,
        beatBytes = sbus.beatBytes,
        hintsSkipProbe = hintsSkipProbe),
      InclusiveCacheMicroParameters(
        writeBytes = writeBytes,
        portFactor = portFactor,
        memCycles = memCycles,
        innerBuf = bufInnerInterior,
        outerBuf = bufOuterInterior),
      control))
    val filter = LazyModule(new TLFilter(cfilter = skipDCacheMMIO))
    val metadata = LazyModule(new BoomL2MetadataAdapter)
    val innerBuffer = bufInnerExterior()
    val outerBuffer = bufOuterExterior()
    val cork = LazyModule(new TLCacheCork)
    val (refillMonitor, replacementMonitor) = newMonitors(context)

    metadata.node :*= filter.node
    refillMonitor.node :*= metadata.node
    innerBuffer.node :*= refillMonitor.node
    l2.node :*= innerBuffer.node
    outerBuffer.node :*= l2.node
    replacementMonitor.node :*= outerBuffer.node

    physicalFilter match {
      case None => cork.node :*= replacementMonitor.node
      case Some(filterParams) =>
        val physicalFilter = LazyModule(new PhysicalFilter(
          filterParams.copy(controlBeatBytes = cbus.beatBytes)))
        cork.node :*= physicalFilter.node :*= replacementMonitor.node
        physicalFilter.controlNode := cbus.coupleTo("boom_l2_physical_filter") {
          TLBuffer(1) := TLFragmenter(cbus, Some("BoomL2PhysicalFilter")) := _
        }
    }

    l2.ctrls.foreach {
      _.ctrlnode := cbus.coupleTo("boom_l2_inclusive_control") {
        TLBuffer(1) := TLFragmenter(cbus, Some("BoomL2InclusiveControl")) := _
      }
    }

    ElaborationArtefacts.add("l2.json", l2.module.json)
    ElaborationArtefacts.add("boom-inclusive-cache.json",
      s"""{"banks":${p(SubsystemBankedCoherenceKey).nBanks},"sets_per_bank":$sets,"ways":$ways,"block_bytes":${sbus.blockBytes},"capacity_kib":${p(SubsystemBankedCoherenceKey).nBanks * sets * ways * sbus.blockBytes / 1024}}""")

    (filter.node, cork.node, None)
  }
}

class WithBoomCoupledL2 extends Config((site, here, up) => {
  case L2ParamKey => L2Param(
    name = "boom-cpl2",
    ways = 8,
    sets = 128,
    blockBytes = BoomL2Experiment.CacheBlockBytes,
    channelBytes = TLChannelBeatBytes(BoomL2Experiment.MemoryBeatBytes),
    replacement = "drrip",
    mshrs = 16,
    releaseData = 0,
    prefetch = Seq(BOPParameters(virtualTrain = false)),
    enablePerf = false,
    enableRollingDB = false,
    enableMonitor = false,
    enableTLLog = false,
    enableCHILog = false,
    elaboratedTopDown = false)
  case BankBitsKey => 2
  case L2NBanksKey => 4
  case LogUtilsOptionsKey => LogUtilsOptions(
    enableDebug = false,
    enablePerf = false,
    fpgaPlatform = false,
    enableXMR = false)
  case PerfCounterOptionsKey => PerfCounterOptions(
    enablePerfPrint = false,
    enablePerfDB = false,
    perfLevel = XSPerfLevel.VERBOSE,
    perfDBHartID = 0)
  case SubsystemBankedCoherenceKey => up(SubsystemBankedCoherenceKey, site).copy(
    nBanks = 4,
    coherenceManager = BoomL2Topology.coupled)
})

class WithBoomInclusiveCache extends Config((site, here, up) => {
  case InclusiveCacheKey => InclusiveCacheParams(
    ways = 8,
    sets = 512,
    writeBytes = 8,
    portFactor = 4,
    memCycles = 30,
    ctrlAddr = None)
  case SubsystemBankedCoherenceKey => up(SubsystemBankedCoherenceKey, site).copy(
    nBanks = 1,
    coherenceManager = BoomL2Topology.inclusive)
})
