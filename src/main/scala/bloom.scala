//see LICENSE for license

package bloom

import Chisel._

import freechips.rocketchip.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._
import freechips.rocketchip.util.InOrderArbiter

class BloomAccel(opcodes: OpcodeSet, val m: Int = 20000, val k: Int = 5)
(implicit p: Parameters) extends LazyRoCC(
    opcodes) {
  override lazy val module = new BloomAccelImp(this)
}

class BloomAccelImp(outer: BloomAccel)(implicit p: Parameters) extends LazyRoCCModuleImp(outer) {
  // accelerator memory 
  val bloom_bit_array = Reg(init = Vec.fill(outer.m)(0.U(1.W)))
  val miss_counter = RegInit(0.U(64.W))
  val busy = Reg(init = Vec.fill(outer.m){Bool(false)})

  val cmd = Queue(io.cmd)
  val funct = cmd.bits.inst.funct
  val hashed_string = cmd.bits.rs1

  // decode RoCC custom function
  val doInit = funct === UInt(0)
  val doMap = funct === UInt(1)
  val doTest = funct === UInt(2)

  // val wdata = UInt(0)
  val testMatch = RegInit(Bool(true))
  val mapModule = Module(new MapBloomModule(outer.m,outer.k))
  val testModule = Module(new TestBloomModule(outer.m,outer.k)) 

  val map_counter = RegInit(0.U(64.W))
  val test_counter = RegInit(0.U(64.W))

  when (cmd.fire()) {
    when (doInit) {
      bloom_bit_array := Reg(init = Vec.fill(outer.m)(0.U(1.W)))
      miss_counter := RegInit(0.U(64.W))
    }
    when (doMap) {
      mapModule.io.input_value := hashed_string
      mapModule.io.input_reset := true.B
      bloom_bit_array := mapModule.io.output_hashBits 
      map_counter := mapModule.io.output_hashIndex
    } otherwise {
      mapModule.io.input_reset := false.B
    }
    when (doTest) {
      testModule.io.input_value := hashed_string
      testModule.io.input_bit_array := bloom_bit_array
      testModule.io.input_reset := true.B
      miss_counter := Mux(testModule.io.output_bit === 1.U(1.W), miss_counter, miss_counter+1.U(64.W))
      // miss_counter := miss_counter+1.U(64.W))
    } otherwise {
      testModule.io.input_reset := false.B
    }
  }

  // PROCESSOR RESPONSE INTERFACE
  // Control for communicate accelerator response back to host processor
  val doResp = cmd.bits.inst.xd
  val stallResp = doResp && !io.resp.ready

  cmd.ready := !stallResp 
    // Command resolved if no stalls AND not issuing a load that will need a request
  io.resp.valid := cmd.valid && doResp 
    // Valid response if valid command, need a response, and no stalls
  io.resp.bits.rd := cmd.bits.inst.rd
    // Write to specified destination register address
  //io.resp.bits.data := miss_counter
  io.resp.bits.data := Mux(doMap, map_counter, miss_counter)
    // Send out 
  io.busy := cmd.valid || busy.reduce(_||_)
    // Be busy when have pending memory requests or committed possibility of pending requests
  io.interrupt := Bool(false)
    // Set this true to trigger an interrupt on the processor (not the case for our current simplified implementation)
} 

// class OpcodeSet(val opcodes: Seq[UInt]) {
//   def |(set: OpcodeSet) =
//     new OpcodeSet(this.opcodes ++ set.opcodes)

//   def matches(oc: UInt) = opcodes.map(_ === oc).reduce(_ || _)
// }

// object OpcodeSet {
//   def custom0 = new OpcodeSet(Seq(Bits("b0001011")))
// }

class WithBloomAccel extends Config ((site, here, up) => {

  case BuildRoCC => Seq(
    (p: Parameters) => {
      val bloom = LazyModule.apply(new BloomAccel(OpcodeSet.custom2)(p))
      bloom
    }
  )
})
