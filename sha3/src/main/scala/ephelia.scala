//see LICENSE for license

package ephelia

import Chisel._

import freechips.rocketchip.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._
import freechips.rocketchip.util.InOrderArbiter

class EpheliaAccel(opcodes: OpcodeSet,val n: Int = 4)(implicit p: Parameters) extends LazyRoCC(
    opcodes) {
  override lazy val module = new EpheliaAccelImp(this)

}

class EpheliaAccelImp(outer: EpheliaAccel)(implicit p: Parameters) extends LazyRoCCModuleImp(outer) {
  val regfile = Mem(outer.n, UInt(width = 64))
  val busy = Reg(init = Vec.fill(outer.n){Bool(false)})

  val cmd = Queue(io.cmd)
  val funct = cmd.bits.inst.funct
  val val1 = cmd.bits.rs1
  val val2 = cmd.bits.rs2
  val doAdd = funct === UInt(0)

  // datapath
  val accum = regfile(0)
  val wdata = val1 + val2

  when (cmd.fire() && doAdd) {
    regfile(0) := wdata
  }

  val doResp = cmd.bits.inst.xd
  val stallResp = doResp && !io.resp.ready

  cmd.ready := !stallResp
    // command resolved if no stalls AND not issuing a load that will need a request

  // PROC RESPONSE INTERFACE
  io.resp.valid := cmd.valid && doResp 
    // valid response if valid command, need a response, and no stalls
  io.resp.bits.rd := cmd.bits.inst.rd
    // Must respond with the appropriate tag or undefined behavior
  io.resp.bits.data := wdata
    // Semantics is to always send out prior accumulator register value

  io.busy := cmd.valid || busy.reduce(_||_)
    // Be busy when have pending memory requests or committed possibility of pending requests
  io.interrupt := Bool(false)
    // Set this true to trigger an interrupt on the processor (please refer to supervisor documentation)
} 

// class OpcodeSet(val opcodes: Seq[UInt]) {
//   def |(set: OpcodeSet) =
//     new OpcodeSet(this.opcodes ++ set.opcodes)

//   def matches(oc: UInt) = opcodes.map(_ === oc).reduce(_ || _)
// }

// object OpcodeSet {
//   def custom0 = new OpcodeSet(Seq(Bits("b0001011")))
// }

class WithEpheliaAccel extends Config ((site, here, up) => {

  case BuildRoCC => Seq(
    (p: Parameters) => {
      val ephelia = LazyModule.apply(new EpheliaAccel(OpcodeSet.custom2)(p))
      ephelia
    }
  )
})
