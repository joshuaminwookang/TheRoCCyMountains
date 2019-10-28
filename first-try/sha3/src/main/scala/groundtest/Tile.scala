// See LICENSE.SiFive for license details.
// See LICENSE.Berkeley for license details.

package freechips.rocketchip.groundtest

import Chisel._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.rocket.{DCache, NonBlockingDCache, RocketCoreParams}
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import scala.collection.mutable.ListBuffer

trait GroundTestTileParams extends TileParams {
  val memStart: BigInt
  val maxRequests: Int
  val numGens: Int

  def build(i: Int, p: Parameters): GroundTestTile
  
  val icache = None
  val btb = None
  val rocc = Nil
  val core = RocketCoreParams(nPMPs = 0) //TODO remove this
  val cached = if(dcache.isDefined) 1 else 0
  val dataScratchpadBytes = 0
}

case object GroundTestTilesKey extends Field[Seq[GroundTestTileParams]]

abstract class GroundTestTile private (params: GroundTestTileParams, x: ClockCrossingType, q: Parameters)
    extends BaseTile(params, x, HartsWontDeduplicate(params), q)
{
  def this(params: GroundTestTileParams)(implicit p: Parameters) = this(params, SynchronousCrossing(), p)
  val intInwardNode: IntInwardNode = IntIdentityNode()
  val intOutwardNode: IntOutwardNode = IntIdentityNode()
  val slaveNode: TLInwardNode = TLIdentityNode()
  val ceaseNode: IntOutwardNode = IntIdentityNode()
  val haltNode: IntOutwardNode = IntIdentityNode()
  val wfiNode: IntOutwardNode = IntIdentityNode()

  val dcacheOpt = params.dcache.map { dc => LazyModule(
    if (dc.nMSHRs == 0) new DCache(hartId, crossing)
    else new NonBlockingDCache(hartId))
  }

  override lazy val module = new GroundTestTileModuleImp(this)
}

class GroundTestTileModuleImp(outer: GroundTestTile) extends BaseTileModuleImp(outer) {
  val status = IO(new GroundTestStatus)
  val halt_and_catch_fire = None

  outer.dcacheOpt foreach { dcache =>
    val ptw = Module(new DummyPTW(1))
    ptw.io.requestors.head <> dcache.module.io.ptw
  }
}
