package chipyard.booml2

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.diplomacy.lazymodule.{LazyModule, LazyModuleImp}

import freechips.rocketchip.diplomacy.AddressSet
import freechips.rocketchip.regmapper.RegField
import freechips.rocketchip.resources.SimpleDevice
import freechips.rocketchip.tilelink.{TLAdapterNode, TLMessages, TLRegisterNode}

sealed trait L2LatencyMetric {
  def name: String
  def categories: Int
}

case object RefillLatency extends L2LatencyMetric {
  val name = "refill"
  val categories = 1
}

case object ReplacementLatency extends L2LatencyMetric {
  val name = "replacement"
  val categories = 2
}

object L2LatencyMonitor {
  val BucketUpperBounds: Seq[Int] = Seq(13, 14, 16, 20, 30, 50, 100, 500, 1000, 3000)
  val BucketCount: Int = BucketUpperBounds.size + 1

  val CommandClear: Int = 1
  val CommandEnable: Int = 2
  val CommandSnapshot: Int = 4

  val ControlOffset: Int = 0x00
  val StatusOffset: Int = 0x08
  val DuplicateErrorOffset: Int = 0x10
  val MissingErrorOffset: Int = 0x18
  val UnfinishedErrorOffset: Int = 0x20
  val TotalEventsOffset: Int = 0x28
  val FirstCategoryTotalOffset: Int = 0x30
  val CounterOverflowOffset: Int = 0x40
  val BucketBaseOffset: Int = 0x100
  val CategoryStride: Int = 0x80
}

class L2LatencyMonitor(
  metric: L2LatencyMetric,
  controlAddress: BigInt,
  controlBeatBytes: Int
)(implicit p: Parameters) extends LazyModule {
  import L2LatencyMonitor._

  val node = TLAdapterNode()
  val device = new SimpleDevice(s"boom-l2-${metric.name}-monitor", Seq(s"chipyard,boom-l2-${metric.name}-monitor"))
  val controlNode = TLRegisterNode(
    address = Seq(AddressSet(controlAddress, 0xfff)),
    device = device,
    beatBytes = controlBeatBytes,
    concurrency = 1)

  lazy val module = new Impl

  class Impl extends LazyModuleImp(this) {
    val enabled = RegInit(false.B)
    val cycle = RegInit(0.U(64.W))
    val cycleOverflow = RegInit(false.B)

    cycle := cycle + 1.U
    when (cycle.andR) {
      cycleOverflow := true.B
    }

    val controlWrite = Wire(Decoupled(UInt(64.W)))
    controlWrite.ready := true.B
    val commandFire = controlWrite.fire
    val clearCommand = commandFire && controlWrite.bits(0)
    val enableCommand = commandFire && controlWrite.bits(1)
    val snapshotCommand = commandFire && controlWrite.bits(2)

    case class EdgeEvents(
      completed: Bool,
      latency: UInt,
      category: UInt,
      duplicate: Bool,
      missing: Bool,
      inflight: Seq[Bool])

    val edgeEvents = node.in.zip(node.out).zipWithIndex.map {
      case (((in, edgeIn), (out, _)), bank) =>
        out <> in

        val sourceCount = edgeIn.client.endSourceId
        require(sourceCount > 0, s"${metric.name} monitor bank $bank has no TileLink sources")

        val inflight = RegInit(VecInit(Seq.fill(sourceCount)(false.B)))
        val pendingFirstValid = RegInit(VecInit(Seq.fill(sourceCount)(false.B)))
        val requestFired = RegInit(VecInit(Seq.fill(sourceCount)(false.B)))
        val startCycle = Reg(Vec(sourceCount, UInt(64.W)))
        val requestCategory = RegInit(VecInit(Seq.fill(sourceCount)(0.U(1.W))))

        val requestValid = Wire(Bool())
        val requestFirstFire = Wire(Bool())
        val requestSource = Wire(UInt(edgeIn.bundle.sourceBits.W))
        val requestCategoryNow = Wire(UInt(1.W))
        val responseValid = Wire(Bool())
        val responseSource = Wire(UInt(edgeIn.bundle.sourceBits.W))

        requestValid := false.B
        requestFirstFire := false.B
        requestSource := 0.U
        requestCategoryNow := 0.U
        responseValid := false.B
        responseSource := 0.U

        metric match {
          case RefillLatency =>
            val dcacheClients = edgeIn.client.clients.filter { client =>
              client.supports.probe &&
                (client.name.contains("DCache") || client.nodePath.exists(_.name == "dcache.node"))
            }
            require(dcacheClients.nonEmpty, s"refill monitor bank $bank cannot identify a cached DCache client")
            val cachedRequestSource = dcacheClients
              .map(_.sourceId.contains(in.a.bits.source))
              .reduce(_ || _)
            val cachedResponseSource = dcacheClients
              .map(_.sourceId.contains(in.d.bits.source))
              .reduce(_ || _)
            val first = edgeIn.first(in.a)
            val acquireBlock = in.a.bits.opcode === TLMessages.AcquireBlock

            requestValid := enabled && in.a.valid && first && acquireBlock && cachedRequestSource
            requestFirstFire := in.a.fire && first && acquireBlock && cachedRequestSource
            requestSource := in.a.bits.source
            responseValid := enabled && in.d.fire && edgeIn.last(in.d) &&
              in.d.bits.opcode === TLMessages.GrantData && cachedResponseSource
            responseSource := in.d.bits.source

          case ReplacementLatency =>
            val first = edgeIn.first(in.c)
            val release = in.c.bits.opcode === TLMessages.Release
            val releaseData = in.c.bits.opcode === TLMessages.ReleaseData

            requestValid := enabled && in.c.valid && first && (release || releaseData)
            requestFirstFire := in.c.fire && first && (release || releaseData)
            requestSource := in.c.bits.source
            requestCategoryNow := releaseData
            responseValid := enabled && in.d.fire && in.d.bits.opcode === TLMessages.ReleaseAck
            responseSource := in.d.bits.source
        }

        val responseMatched = inflight(responseSource)
        val sameCycleNewRequest = requestValid && requestSource === responseSource && !inflight(requestSource)
        val completed = responseValid && (responseMatched || sameCycleNewRequest)
        val missing = responseValid && !responseMatched && !sameCycleNewRequest
        val latency = Mux(responseMatched, cycle - startCycle(responseSource), 0.U)
        val completedCategory = Mux(responseMatched, requestCategory(responseSource), requestCategoryNow)

        val sourceCompletesNow = completed && responseSource === requestSource
        val sourceAvailable = !inflight(requestSource) || sourceCompletesNow
        val newRequest = requestValid && sourceAvailable
        val duplicate = requestValid && inflight(requestSource) &&
          requestFired(requestSource) && !sourceCompletesNow

        when (completed) {
          inflight(responseSource) := false.B
          pendingFirstValid(responseSource) := false.B
          requestFired(responseSource) := false.B
        }

        when (newRequest) {
          inflight(requestSource) := true.B
          pendingFirstValid(requestSource) := !requestFirstFire
          requestFired(requestSource) := requestFirstFire
          startCycle(requestSource) := cycle
          requestCategory(requestSource) := requestCategoryNow
        } .elsewhen (requestFirstFire && inflight(requestSource) && pendingFirstValid(requestSource)) {
          pendingFirstValid(requestSource) := false.B
          requestFired(requestSource) := true.B
        }

        when (clearCommand) {
          inflight.foreach(_ := false.B)
          pendingFirstValid.foreach(_ := false.B)
          requestFired.foreach(_ := false.B)
        }

        EdgeEvents(completed, latency, completedCategory, duplicate, missing, inflight.toSeq)
    }

    require(edgeEvents.nonEmpty, s"${metric.name} monitor is not connected to a TileLink edge")

    def bucketIndex(latency: UInt): UInt = {
      MuxCase(BucketUpperBounds.size.U, BucketUpperBounds.zipWithIndex.map {
        case (upper, index) => (latency < upper.U) -> index.U
      })
    }

    def addOverflows(counter: UInt, increment: UInt): Bool = {
      require(increment.getWidth <= counter.getWidth)
      (counter +& increment)(counter.getWidth)
    }

    val bucketCounters = RegInit(VecInit(Seq.fill(metric.categories)(
      VecInit(Seq.fill(BucketCount)(0.U(64.W))))))
    val totalEvents = RegInit(0.U(64.W))
    val duplicateErrors = RegInit(0.U(64.W))
    val missingErrors = RegInit(0.U(64.W))
    val unfinishedErrors = RegInit(0.U(64.W))
    val counterOverflow = RegInit(false.B)

    val totalIncrement = PopCount(VecInit(edgeEvents.map(_.completed)))
    val duplicateIncrement = PopCount(VecInit(edgeEvents.map(_.duplicate)))
    val missingIncrement = PopCount(VecInit(edgeEvents.map(_.missing)))
    val unfinishedNow = PopCount(VecInit(edgeEvents.flatMap(_.inflight)))

    val bucketIncrements = Seq.tabulate(metric.categories, BucketCount) { case (category, bucket) =>
      PopCount(VecInit(edgeEvents.map { event =>
        event.completed && event.category === category.U && bucketIndex(event.latency) === bucket.U
      }))
    }

    when (clearCommand) {
      enabled := false.B
      totalEvents := 0.U
      duplicateErrors := 0.U
      missingErrors := 0.U
      unfinishedErrors := 0.U
      cycleOverflow := false.B
      counterOverflow := false.B
      bucketCounters.foreach(_.foreach(_ := 0.U))
    } .otherwise {
      when (enableCommand) {
        enabled := true.B
      }
      when (snapshotCommand) {
        enabled := false.B
        unfinishedErrors := unfinishedErrors + unfinishedNow
      }

      totalEvents := totalEvents + totalIncrement
      duplicateErrors := duplicateErrors + duplicateIncrement
      missingErrors := missingErrors + missingIncrement

      for (category <- 0 until metric.categories; bucket <- 0 until BucketCount) {
        val increment = bucketIncrements(category)(bucket)
        bucketCounters(category)(bucket) := bucketCounters(category)(bucket) + increment
        when (increment =/= 0.U && addOverflows(bucketCounters(category)(bucket), increment)) {
          counterOverflow := true.B
        }
      }
      when ((totalIncrement =/= 0.U && addOverflows(totalEvents, totalIncrement)) ||
        (duplicateIncrement =/= 0.U && addOverflows(duplicateErrors, duplicateIncrement)) ||
        (missingIncrement =/= 0.U && addOverflows(missingErrors, missingIncrement))) {
        counterOverflow := true.B
      }
    }

    val categoryTotals = VecInit(bucketCounters.map(_.reduce(_ + _)))
    val bucketTotal = categoryTotals.reduce(_ + _)
    val bucketMismatch = bucketTotal =/= totalEvents
    val anyErrors = duplicateErrors.orR || missingErrors.orR || unfinishedErrors.orR ||
      cycleOverflow || counterOverflow || bucketMismatch
    val status = Cat(
      0.U(56.W),
      bucketMismatch,
      counterOverflow,
      cycleOverflow,
      unfinishedErrors.orR,
      missingErrors.orR,
      duplicateErrors.orR,
      anyErrors,
      enabled)

    val fixedRegisters = Seq(
      ControlOffset -> Seq(RegField.w(64, controlWrite)),
      StatusOffset -> Seq(RegField.r(64, status)),
      DuplicateErrorOffset -> Seq(RegField.r(64, duplicateErrors)),
      MissingErrorOffset -> Seq(RegField.r(64, missingErrors)),
      UnfinishedErrorOffset -> Seq(RegField.r(64, unfinishedErrors)),
      TotalEventsOffset -> Seq(RegField.r(64, totalEvents)),
      CounterOverflowOffset -> Seq(RegField.r(64, counterOverflow.asUInt)))
    val categoryRegisters = categoryTotals.zipWithIndex.map { case (count, category) =>
      (FirstCategoryTotalOffset + category * 8) -> Seq(RegField.r(64, count))
    }
    val bucketRegisters = bucketCounters.zipWithIndex.flatMap { case (counters, category) =>
      counters.zipWithIndex.map { case (count, bucket) =>
        (BucketBaseOffset + category * CategoryStride + bucket * 8) -> Seq(RegField.r(64, count))
      }
    }

    controlNode.regmap((fixedRegisters ++ categoryRegisters ++ bucketRegisters): _*)
  }
}
