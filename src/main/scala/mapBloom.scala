//see LICENSE for license
package bloom

import Chisel._

import chisel3.util._
import chisel3.iotesters.PeekPokeTester
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class MapBloomModule(val M: Int, val K: Int) extends Module {
  //val W = 64
  val io = new Bundle { 
    val input_value = Input(UInt(64.W))
    val input_reset = Input(Bool())
    val output_hashBits = Output(Reg(Vec(M,UInt(1.W))))
    val output_hashIndex = Output(UInt(64.W))
  }
     // Local variables
    val x  = RegInit(0.U(64.W))
    val y  = RegInit(0.U(64.W))
    val i  = RegInit(0.U(64.W))
    val done = (i === K.asUInt(64.W))
  
    // Hash computation
    when(io.input_reset){
      i := RegInit(0.U(64.W))
      x := io.input_value
      y := x >> 4
    } otherwise {
      i := i + 1.U(64.W)
      x := Mux(~done, (x + y) % K.asUInt(64.W), x)  
      y := Mux(~done, (y + i) % K.asUInt(64.W), y)
      io.output_hashBits(x) := 1.U(1.W)
      io.output_hashIndex := x
    }
}