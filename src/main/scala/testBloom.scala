//see LICENSE for license
//authors: Colin Schmidt, Adam Izraelevitz
package bloom

import Chisel._

import chisel3.util._
import chisel3.iotesters.PeekPokeTester
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class TestBloomModule(val M: Int, val K: Int) extends Module {
  //val W = 64
  val io = new Bundle { 
    val input_value = Input(UInt(64.W))
    val input_bit_array = Input(Reg(Vec(M,UInt(1.W))))
    val output_boolean = Output(Bool())
  }
     // Local variables
    val x  = RegInit(0.U(64.W))
    val y  = RegInit(0.U(64.W))
    val i  = RegInit(0.U(64.W))
    val found = RegInit(true.B)
    val done = (i === K.asUInt(64.W)) || (!found)
  
    // Hash computation
 
    when(i === 0.U(64.W)){
      x := io.input_value
      y := x >> 4
    } otherwise {
      x := Mux(~done, (x + y) % K.asUInt(64.W), x)  
      y := Mux(~done, (y + i) % K.asUInt(64.W), y)
      found:= (io.input_bit_array(x) === 1.U(1.W))
      io.output_boolean := found
    }
}