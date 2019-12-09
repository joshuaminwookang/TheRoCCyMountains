//see LICENSE for license
//authors: Colin Schmidt, Adam Izraelevitz
package bloom

import Chisel._
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

    val x = RegNext(io.input_value)
    val y = RegInit(0.U(64.W))

    for(i <- 0 until K) {
        x := (x + y) % UInt(K)
        y := (y + UInt(i)) % UInt(K)
        io.output_hashBits(x) := UInt(1)
    }
}