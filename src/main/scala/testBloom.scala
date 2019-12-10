//see LICENSE for license
package bloom

import Chisel._

// import chisel3.util._
import chisel3.iotesters.PeekPokeTester
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class TestBloomModule(val M: Int, val K: Int) extends Module {
  //val W = 64
  val io = new Bundle { 
    val input_value = Input(UInt(64.W))
    val input_bit_array = Input(Reg(Vec(M,UInt(1.W))))
    val input_reset = Input(Bool())
    val output_bit = Output(UInt(1.W))
    val output_busy = Output(Bool())
  }
    // states of this module 
    val s_idle :: s_hash :: s_resp :: Nil = Enum(Bits(), 3)
    val state_reg = RegInit(s_idle)

     // Local variables
    val x  = RegInit(0.U(64.W))
    val y  = RegInit(0.U(64.W))
    val i  = RegInit(0.U(64.W))
    val bit = RegInit(1.U(1.W))
    val done = RegInit(Bool(false))
    
    done := (i === K.asUInt(64.W)) || (bit === 0.U(1.W))

    switch(state_reg) {
      is (s_idle) {
        when(io.input_reset){
          i := RegInit(0.U(64.W))
          x := io.input_value
          y := io.input_value >> 4
          bit := RegInit(1.U(1.W))
          io.output_bit := 0.U(1.W)
          // get into hashing state
          state_reg := s_hash
          io.output_busy := true.B
        } otherwise {
          io.output_busy := false.B
        }
      }
      is (s_hash) {
        i := i + 1.U(64.W)
        x := (x + y) % K.asUInt(64.W)  
        y := (y + i) % K.asUInt(64.W)
        bit := io.input_bit_array(x)
        io.output_bit := ~bit
        io.output_busy := true.B
        when (done) {
          state_reg := s_resp
        }
      }
      is (s_resp) {
        bit := io.input_bit_array(x)
        io.output_bit := ~bit
        io.output_busy := false.B
        state_reg := s_idle
      }
    }
}