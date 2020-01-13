// 2020 (c) Josh Kang @ Williams College

package freechips.rocketchip.tile

import Chisel._

import freechips.rocketchip.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.InOrderArbiter

class EpheliaAccel(implicit p: Parameters) extends LazyRoCC {
  override lazy val module = new EpheliaAccelImp(this)
}

class EpheliaAccelImp(outer: EpheliaAccel, n: Int = 4)(implicit p: Parameters) extends LazyRoCCModule(outer) with HasCoreParameters {
  val regfile = Mem(n, UInt(width = 64))
  val busy = Reg(init = Vec.fill(n){Bool(false)})

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
