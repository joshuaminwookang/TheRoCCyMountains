/**
 * String comparison hardware.  (c) duane a. bailey
 * This hardware performs a C-style string compare and is a suitable
 * replacement to strcmp.  Typically, we use
 *    custom0  d,s1,s2
 * to compare strings pointed to by registers s1 and s2, with the first
 * difference (s1[i]-s2[i]) returned in register d.  If the strings are
 * identical, the result is 0.
 * This hardware performs only slightly better than software, even on very
 * long strings.  Power performance has yet to be measured.
 */

package freechips.rocketchip.tile

import Chisel._
import freechips.rocketchip.tilelink._ //For LazyRoCC
import freechips.rocketchip.config._ //For Config
import freechips.rocketchip.diplomacy._ //For LazyModule
import freechips.rocketchip.rocket._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.util.InOrderArbiter

// ADAPTED for SOAR (c) 2020 Josh Kang

/**
 * Lazy module for accelerator for comparing two strings.
 */
class FullStrcmp(implicit p: Parameters) extends LazyRoCC {
   override lazy val module = new FullStrcmpImp(this)
}

/**
 * This accelerator compares two strings; the result is the byte difference
 * the first differing bytes, or 0.  This implementation assumes that strings
 * are resident in virtual memory.  Calls to this instruction may require mapping
 * and/or locking pages to physical memory.
 */
class FullStrcmpImp(outer: FullStrcmp)(implicit p: Parameters)
   extends LazyRoCCModule(outer) with HasCoreParameters
{
   // states of the state machine
   val idleState :: compareState :: doneState :: Nil = Enum(Bits(),3)
   val state = Reg(init = idleState) // idle -> compare* -> done -> idle
   val aPtr = RegInit(0.U(64.W))  // pointer into the string
   val bPtr = RegInit(0.U(64.W))  // pointer into the string
   val aVal = Reg(UInt(8.W))      // the character read
   val bVal = Reg(UInt(8.W))      // the character read
   val diff = aVal - io.mem.resp.bits.data  // computed difference of a and b
   val done = ((aVal === 0.U) ||  // finish predicate
               (io.mem.resp.bits.data === 0.U) || (diff =/= 0.U))
   val source = Reg(init=0.U(1.W))  // which (A=0 or B=1) string is the source
   val destReg = Reg(init=io.cmd.bits.inst.rd)  // register to write to
   val request = Reg(init=false.B)  // true iff request is in transit to mem

   // back pressure to core: only ready when not computing
   io.cmd.ready := (state === idleState)   // when is this accelerator ready?
   // when accelerator uses mem; perhaps reduced to just compare state?
   io.busy := (state =/= idleState)
   // request validity is stored in "request"
   io.mem.req.valid := request

   /**
    * when we make memory requests, we're reading a single byte from memory
    * (or cache) at a time.  These bytes are unsigned (if they'd been signed
    * then the data field would be sign-extended).  The addresses we use are
    * virtual addresses; we depend on the caching system to perform the page
    * table walking for us (this is important because a translation failure may
    * require re-play of the request).
    */
   // static portions of the request
   io.mem.req.bits.cmd := 0.U   // read
//   io.mem.req.bits.size := 0.U  // log2(n); n is one byte
//   io.mem.req.bits.signed := false.B  // value is unsigned


   io.mem.req.bits.data := Bits(0)  // data written (never used)
   io.mem.req.bits.phys := false.B  // pointers are virtual addresses
   io.mem.req.bits.addr := Mux(source === 0.U, aPtr, bPtr)  //
   io.mem.req.bits.tag := source   // identify the source string (A=0, B=1)

   /**
    * The state machine.
    * This machine starts in idle.  When a command is received, the request
    * to memory is made and is held valid for the ready cycle and one more.
    * Response must be collected when valid.
    */
   switch (state) {
     is (idleState) {
        when (io.cmd.fire()) {
           destReg := io.cmd.bits.inst.rd  // get destination register
           aPtr := io.cmd.bits.rs1 // first String
           bPtr := io.cmd.bits.rs2 // second string
           request := true.B
           state := compareState                // move to first stage request
        }
     }
     is (compareState) {
       // request is now *not* valid; await response is valid
       when (RegNext(io.mem.req.fire())) {
         request := false.B
       }
       when (io.mem.resp.valid && !RegNext(io.mem.resp.valid)) {          // memory as response data
          request := true.B
          when (source === 0.U) {
             aVal := io.mem.resp.bits.data
             aPtr := aPtr+1.U
          } .otherwise {
             bVal := io.mem.resp.bits.data
             bPtr := bPtr+1.U
             when (done) {
                request := false.B;
                state := doneState
             }
          }
          source := ~source
       }       
     }
     is (doneState) {
       when (io.resp.fire()) {
         state := idleState
       }
     }
   }
   // combinational circuitry for response building
   io.resp.bits.rd := destReg
   io.resp.bits.data := diff
   io.resp.valid := state === doneState
   io.interrupt := false.B
}

/**
 * Add the following to the example generator's RocketConfigs file:

class FullStrcmpRocketConfig extends Config(
    new WithTop ++
    new WithBootROM ++
    new freechips.rocketchip.subsystem.WithInclusiveCache ++
    new williams.WithFullStrcmp ++
    new freechips.rocketchip.subsystem.WithNBigCores(1) ++
    new freechips.rocketchip.system.BaseConfig)

 */
