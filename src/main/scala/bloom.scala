//see LICENSE for license

package bloom

import chisel3._
import chisel3.util._

import freechips.rocketchip.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._
import freechips.rocketchip.util.InOrderArbiter

class BloomAccel(opcodes: OpcodeSet)
(implicit p: Parameters) extends LazyRoCC(
    opcodes) {
  override lazy val module = new BloomAccelImp(this)
}

class BloomAccelImp(outer: BloomAccel)(implicit p: Parameters) extends LazyRoCCModuleImp(outer) {
  // accelerator memory 
  val bloom_bit_array = RegInit(VecInit(Seq.fill(20000)(0.U(1.W))))
  val miss_counter = RegInit(0.U(64.W))
  val busy = RegInit(Bool(false))

  val cmd = Queue(io.cmd)
  val funct = cmd.bits.inst.funct
  val hashed_string = cmd.bits.rs1

  // decode RoCC custom function
  val doInit = funct === UInt(0)
  val doMap = funct === UInt(1)
  val doTest = funct === UInt(2)

  // constant value registers
  // val bloom_param_m = 20000.U(64.W)

  // val mapModule = Module(new MapBloomModule(outer.m,outer.k))
  // val testModule = Module(new TestBloomModule(outer.m,outer.k)) 
  val mapModule = Module(new MapBloomModule)
  val testModule = Module(new TestBloomModule)
  val debug = RegInit(0.U(64.W))

  // Hash computation
  val x0  = Wire(UInt())
  val y0  = Wire(UInt())

  val x1  = Wire(UInt())
  val y1  = Wire(UInt())

  val x2  = Wire(UInt())
  val y2  = Wire(UInt())

  val x3  = Wire(UInt())
  val y3  = Wire(UInt())

  val x4  = Wire(UInt())
  val y4  = Wire(UInt())

  val x5  = Wire(UInt())
  val y5  = Wire(UInt())

  x0 := hashed_string
  y0 := hashed_string >> 4

  x1 := (x0 + y0) % 20000.U(64.W)
  y1 := (y0 + 0.U(64.W)) % 20000.U(64.W)

  x2 := (x1 + y1) % 20000.U(64.W)
  y2 := (y1 + 1.U(64.W)) % 20000.U(64.W)

  x3 := (x2 + y2) % 20000.U(64.W)
  y3 := (y2 + 2.U(64.W)) % 20000.U(64.W)

  x4 := (x3 + y3) % 20000.U(64.W)
  y4 := (y3 + 3.U(64.W)) % 20000.U(64.W)

  x5 := (x4 + y4) % 20000.U(64.W)
  y5 := (y4 + 4.U(64.W)) % 20000.U(64.W)

  // val found1 = RegInit(1.U(1.W))
  // val found2 = RegInit(1.U(1.W))
  // val found3 = RegInit(1.U(1.W))
  // val found4 = RegInit(1.U(1.W))
  // val found5 = RegInit(1.U(1.W))

  val found1 = Wire(UInt())
  val found2 = Wire(UInt())
  val found3 = Wire(UInt())
  val found4 = Wire(UInt())
  val found5 = Wire(UInt())

  found1 := bloom_bit_array(x1)
  found2 := bloom_bit_array(x2)
  found3 := bloom_bit_array(x3)
  found4 := bloom_bit_array(x4)
  found5 := bloom_bit_array(x5)

  // Custom function behaviors
  when (cmd.fire()) {
    when (doInit) {
      bloom_bit_array := Reg(init = Vec.fill(20000)(0.U(1.W)))
      miss_counter := RegInit(0.U(64.W))
    }
    when (doMap) {
      bloom_bit_array(x1) := 1.U(1.W)
      bloom_bit_array(x2) := 1.U(1.W)
      bloom_bit_array(x3) := 1.U(1.W)
      bloom_bit_array(x4) := 1.U(1.W)
      bloom_bit_array(x5) := 1.U(1.W)
    } 
    when (doTest) {
      miss_counter := miss_counter + (found1 & found2 & found3 & found4 & found5)
    } 
  } 

  // when (cmd.fire()) {
  //   when (doInit) {
  //     bloom_bit_array := RegInit(VecInit(Seq.fill(20000)(0.U(1.W))))
  //     miss_counter := RegInit(0.U(64.W))
  //   }
  //   when (doMap) {
  //     mapModule.io.input_value := hashed_string
  //     // mapModule.io.input_reset := true.B
  //     bloom_bit_array := mapModule.io.output_hashBits 
  //     debug := mapModule.io.output_hashIndex
  //   } 
  //   when (doTest) {
  //     testModule.io.input_value := hashed_string
  //     testModule.io.input_bit_array := bloom_bit_array
  //     // testModule.io.input_reset := true.B
  //     miss_counter := miss_counter + ~testModule.io.output_found
  //     debug := testModule.io.output_debug
  //   } 
  // } 


  when (io.resp.fire()){
    busy := Bool(false)
  }

  // bloom_bit_array := mapModule.io.output_hashBits 
  // testModule.io.input_bit_array := bloom_bit_array
  busy := mapModule.io.output_busy || testModule.io.output_busy

  // PROCESSOR RESPONSE INTERFACE
  // Control for communicate accelerator response back to host processor
  val doResp = cmd.bits.inst.xd
  val stallResp = doResp && !io.resp.ready 

  cmd.ready := !stallResp 
    // Command resolved if no stalls AND not issuing a load that will need a request
  io.resp.valid := cmd.valid && doResp && (!busy)
    // Valid response if valid command, need a response, and no stalls
  io.resp.bits.rd := cmd.bits.inst.rd
    // Write to specified destination register address
  // io.resp.bits.data := bloom_bit_array(7081.U(64.W))*1000.U(64.W) + bloom_bit_array(9951.U(64.W))*100.U(64.W)
  io.resp.bits.data := x5*100000.U(64.W) + found1*10000.U(64.W)+ found2*1000.U(64.W)+found3*100.U(64.W)+found4*10.U(64.W)+found5
  // io.resp.bits.data := Mux(doMap, debug, miss_counter)
    // Send out 
  io.busy := cmd.valid || busy
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
