//see LICENSE for license
package bloom

// import Chisel._

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
    val output_busy = Output(Bool())
  }
    // states of this module 
    val s_idle :: s_hash :: s_resp :: Nil = Enum(Bits(), 3)
    val state_reg = RegInit(s_idle)

     // Local variables
    val x  = RegInit(0.U(64.W))
    val y  = RegInit(0.U(64.W))
    val i  = RegInit(0.U(64.W))
    val done = (i === K.asUInt(64.W))
  
    // Hash computation
    // when(io.input_reset){
    //   i := RegInit(0.U(64.W))
    //   x := io.input_value
    //   y := io.input_value >> 4
    // } otherwise {
    //   i := i + 1.U(64.W)
    //   x := Mux(~done, (x + y) % K.asUInt(64.W), x)  
    //   y := Mux(~done, (y + i) % K.asUInt(64.W), y)
    //   io.output_hashBits(x) := 1.U(1.W)
    //   io.output_hashIndex := x
    // }

    io.output_busy := !done

    switch(state_reg) {
      is (s_idle) {
        when(io.input_reset){
          i := RegInit(0.U(64.W))
          x := io.input_value
          y := io.input_value >> 4
          io.output_hashBits(x) := 1.U(1.W)
          io.output_hashIndex := x
          done := (i === K.asUInt(64.W)) || (bit === 0.U(1.W))
          // get into hashing state
          state_reg := s_hash
        }
      }
      is (s_hash) {
        i := i + 1.U(64.W)
        x := (x + y) % K.asUInt(64.W)  
        y := (y + i) % K.asUInt(64.W)
        io.output_hashBits(x) := 1.U(1.W)
        io.output_hashIndex := x
        done := (i === K.asUInt(64.W)) || (bit === 0.U(1.W))

        // are we done hashing?
        when (done) {
          state_reg := s_resp
        }
      }
      // respond to main module 
      is (s_resp) {
        io.output_hashBits(x) := 1.U(1.W)
        io.output_hashIndex := x
      }
    }
}