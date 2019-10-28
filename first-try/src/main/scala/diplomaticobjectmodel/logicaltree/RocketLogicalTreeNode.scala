// See LICENSE.SiFive for license details.

package freechips.rocketchip.diplomaticobjectmodel.logicaltree

import freechips.rocketchip.diplomacy.{LazyModule, ResourceBindings, ResourceBindingsMap, SimpleDevice}
import freechips.rocketchip.diplomaticobjectmodel.DiplomaticObjectModelAddressing
import freechips.rocketchip.diplomaticobjectmodel.model._
import freechips.rocketchip.rocket.{DCacheParams, Frontend, HellaCache, ICache, ICacheParams, ScratchpadSlavePort}
import freechips.rocketchip.tile.{RocketTileParams, TileParams, XLen}


/**
 * Represents either a DCache or a DTIM.
 *
 * The data memory subsystem is assumed to be a DTIM if and only if deviceOpt is
 * a Some(SimpleDevice), as a DCache would not create a Device.
 */
class DCacheLogicalTreeNode(dcache: HellaCache, deviceOpt: Option[SimpleDevice], params: DCacheParams) extends LogicalTreeNode(() => deviceOpt) {
  def getOMComponents(resourceBindings: ResourceBindings, children: Seq[OMComponent]): Seq[OMComponent] = {
    deviceOpt.foreach {
      device => require(!resourceBindings.map.isEmpty, s"""ResourceBindings map for ${device.devname} is empty""")
    }
    Seq(
      OMDCache(
        memoryRegions = DiplomaticObjectModelAddressing.getOMMemoryRegions("DTIM", resourceBindings),
        interrupts = Nil,
        nSets = params.nSets,
        nWays = params.nWays,
        blockSizeBytes = params.blockBytes,
        dataMemorySizeBytes = params.nSets * params.nWays * params.blockBytes,
        dataECC = params.dataECC.map(OMECC.fromString),
        tagECC = params.tagECC.map(OMECC.fromString),
        nTLBEntries = params.nTLBEntries,
        memories = dcache.getOMSRAMs(),
      )
    )
  }
}


class ICacheLogicalTreeNode(icache: ICache, deviceOpt: Option[SimpleDevice], params: ICacheParams) extends LogicalTreeNode(() => deviceOpt) {
  override def getOMComponents(resourceBindings: ResourceBindings, children: Seq[OMComponent] = Nil): Seq[OMComponent] = {
    Seq(
      OMICache(
        memoryRegions = DiplomaticObjectModelAddressing.getOMMemoryRegions("ITIM", resourceBindings),
        interrupts = Nil,
        nSets = params.nSets,
        nWays = params.nWays,
        blockSizeBytes = params.blockBytes,
        dataMemorySizeBytes = params.nSets * params.nWays * params.blockBytes,
        dataECC = params.dataECC.map(OMECC.fromString),
        tagECC = params.tagECC.map(OMECC.fromString),
        nTLBEntries = params.nTLBEntries,
        maxTimSize = params.nSets * (params.nWays-1) * params.blockBytes,
        memories = icache.module.data_arrays.map(_._2),
      )
    )
  }
}

class RocketLogicalTreeNode(
  device: SimpleDevice,
  rocketParams: RocketTileParams,
  dtim_adapter: Option[ScratchpadSlavePort],
  XLen: Int
) extends LogicalTreeNode(() => Some(device)) {

  def getOMInterruptTargets(): Seq[OMInterruptTarget] = {
    Seq(OMInterruptTarget(
      hartId = rocketParams.hartId,
      modes = OMModes.getModes(rocketParams.core.useVM)
    ))
  }

  override def getOMComponents(resourceBindings: ResourceBindings, components: Seq[OMComponent]): Seq[OMComponent] = {
    val coreParams = rocketParams.core

    // Expect that one of the components passed in is the DCache/DTIM.
    val omDCache = components.collectFirst { case x: OMDCache => x }.get

    // Expect that one of the components passed in is the ICache.
    val omICache = components.collectFirst { case x: OMICache => x }.get

    Seq(OMRocketCore(
      isa = OMISA.rocketISA(coreParams, XLen),
      mulDiv =  coreParams.mulDiv.map{ md => OMMulDiv.makeOMI(md, XLen)},
      fpu = coreParams.fpu.map{f => OMFPU(fLen = f.fLen)},
      performanceMonitor = PerformanceMonitor.perfmon(coreParams),
      pmp = OMPMP.pmp(coreParams),
      documentationName = rocketParams.name.getOrElse("rocket"),
      hartIds = Seq(rocketParams.hartId),
      hasVectoredInterrupts = true,
      interruptLatency = 4,
      nLocalInterrupts = coreParams.nLocalInterrupts,
      nBreakpoints = coreParams.nBreakpoints,
      branchPredictor = rocketParams.btb.map(OMBTB.makeOMI),
      dcache = Some(omDCache),
      icache = Some(omICache),
      hasClockGate = coreParams.clockGate,
      hasSCIE = coreParams.useSCIE
    ))
  }
}

class RocketTileLogicalTreeNode(
  getOMRocketInterruptTargets: () => Seq[OMInterruptTarget]) extends LogicalTreeNode(() => None) {

  def getIndex(cs: Seq[OMComponent]): Seq[(OMComponent, Int)] = {
    cs.zipWithIndex.filter(_._1.isInstanceOf[OMPLIC])
  }

  def updatePlic(plic: OMPLIC): OMPLIC = {
    val omRocketInterruptTargets: Seq[OMInterruptTarget] = getOMRocketInterruptTargets()

    plic.copy(targets = omRocketInterruptTargets)
  }

  def addIntsToPlic(resourceBindings: ResourceBindings, components: Seq[OMComponent]): Seq[OMComponent] = {
    val cs = getIndex(components)

    require(cs.size <= 1, "Too many Plic's")

    cs.flatMap {
      case (plic, index) =>
        val omplic = plic.asInstanceOf[OMPLIC]
        val updatedPlic = updatePlic(omplic)
        components.updated(index, updatedPlic)
    }
  }

  override def getOMComponents(resourceBindings: ResourceBindings, components: Seq[OMComponent]): Seq[OMComponent] = {
    components
  }
}

