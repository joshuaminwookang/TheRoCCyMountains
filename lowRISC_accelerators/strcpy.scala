/**
 * A simple string copying routine. Assumes that source and destination
 * are valid locations for reading a string and writing a similar output.
 * (c) 2020 duane a. bailey
 */

// VERSION USED FOR SOAR (Nexys FPGA Impl) Josh Kang
package freechips.rocketchip.tile

import Chisel._
import freechips.rocketchip.tilelink._ //For LazyRoCC
import freechips.rocketchip.config._ //For Config
import freechips.rocketchip.diplomacy._ //For LazyModule
import freechips.rocketchip.rocket._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.util.InOrderArbiter

/**
 * Lazy module for accelerator for comparing two strings.
 */
class Strcpy(implicit p: Parameters) extends LazyRoCC {
   override lazy val module = new StrcpyImp(this)
}

/**
 * This accelerator compares two strings; the result is the byte difference
 * the first differing bytes, or 0.  This implementation assumes that strings
 * are resident in virtual memory.  Calls to this instruction may require mapping
 * and/or locking pages to physical memory.
 */
class StrcpyImp(outer: Strcpy)(implicit p: Parameters)
   extends LazyRoCCModuleImp(outer) with HasCoreParameters
{
   // states of the state machine
  val idleState :: readState :: writeState :: doneState :: Nil = Enum(Bits(),4)
   val state = Reg(init = idleState) // idle -> compare* -> done -> idle
   val srcPtr = RegInit(0.U(64.W))  // pointer into the string
   val dstPtr = RegInit(0.U(64.W))  // pointer into the string
   val strVal = Reg(UInt(8.W))      // the character read
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
   val R = 0.U
   val W = 1.U
   val cmd = Reg(init = R)
   val data = Reg(init = 0.U(8.W))
   val ptr = Reg(init = 0.U(64.W))
   val size = Reg(init = 0.U(4.W))

   val done = (state === writeState) && (!request) && (data === 0.U)

   io.mem.req.bits.cmd := cmd   // R/W
   io.mem.req.bits.size := 0.U  // log2(n); n is one byte
   io.mem.req.bits.signed := false.B  // value is unsigned
   io.mem.req.bits.data := data  // write data
   io.mem.req.bits.phys := false.B  // pointers are virtual addresses
   io.mem.req.bits.addr := ptr   // R/W address
   io.mem.req.bits.tag := 0.U   // identify the source string (A=0, B=1)

   /**
    * The state machine.
    * This machine starts in idle.  When a command is received, the request
    * to memory is made and is held valid for the ready cycle and one more.
    * Response must be collected when valid.
    */
   switch (state) {
     is (idleState) {
        when (io.cmd.fire()) {
           srcPtr := io.cmd.bits.rs1 // first String
           dstPtr := io.cmd.bits.rs2 // second string
	   // now, set up initial request: read from source string
           request := true.B
	   cmd := R
	   ptr := io.cmd.bits.rs1
	   data := 0.U
           state := readState                // move to first stage request
        }
     }
     is (readState) {
       // request is now *not* valid; await response is valid
       when (RegNext(io.mem.req.fire())) {
         request := false.B
       }
       // on "rising edge" of response:
       when (io.mem.resp.valid && !RegNext(io.mem.resp.valid)) {          // memory as response data
          srcPtr := srcPtr+1.U  // move source along
          // set up write request to destination
          cmd := W              
	  ptr := dstPtr
          data := io.mem.resp.bits.data
          val ch = io.mem.resp.bits.data(7,0)
          printf("Writing '%c' to %x\n",ch,dstPtr);
          state := writeState
          request := true.B
       }       
     }
     is (writeState) {
       // request is now *not* valid; await response is valid
       when (RegNext(io.mem.req.fire())) {
         request := false.B
       }
       when (!request) {
          dstPtr := dstPtr+1.U
          // (possibly) set up next read:
          cmd := R
	  ptr := srcPtr
          data := 0.U
          state := Mux(done,idleState,readState)
          request := !done  // if we're not done, we're reading
       }
     }
   }
   // combinational circuitry for response building
   io.resp.valid := false.B
   io.interrupt := false.B
}

/**
 * Add the following to the example generator's RocketConfigs file:

class StrcpyRocketConfig extends Config(
    new WithTop ++
    new WithBootROM ++
    new freechips.rocketchip.subsystem.WithInclusiveCache ++
    new williams.WithStrcpy ++
    new freechips.rocketchip.subsystem.WithNBigCores(1) ++
    new freechips.rocketchip.system.BaseConfig)

 */
