//see LICENSE for license
//authors: Colin Schmidt, Adam Izraelevitz
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
    val output_hashBits = Output(Reg(Vec(M,UInt(1.W))))
  }
     // Local variables
    val x  = RegInit(0.U(64.W))
    val y  = RegInit(0.U(64.W))
    val i  = RegInit(0.U(64.W))
    val done = (i === K.asUInt(64.W))
  
    // Hash computation
 
    when(i === 0.U(64.W)){
      x := io.input_value
      y := x >> 4
    } otherwise {
      x := Mux(~done, (x + y) % K.asUInt(64.W), x)  
      y := Mux(~done, (y + i) % K.asUInt(64.W), y)
      io.output_hashBits(x) := 1.U(1.W)
      printf("x: %d\n", x)
    }
}