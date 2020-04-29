//Simple Combinational Accelerator, based on the fortyTwo accelerator template.
// (c) Maddie Burbage, 2020, for the Bailey Research Group at Williams
// Adapted to be compatible with lowRISC v.0.6-refresh by Josh Kang
package freechips.rocketchip.tile

import Chisel._
import freechips.rocketchip.tilelink._ //For LazyRoCC
import freechips.rocketchip.config._ //For Config
import freechips.rocketchip.diplomacy._ //For LazyModule
import freechips.rocketchip.rocket._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.util.InOrderArbiter


//Wrapper for the accelerator
class Combinations(implicit p: Parameters) extends LazyRoCC {
    override lazy val module = new CombinationsImp(this)
}

//Main accelerator class, directs instruction inputs to functions for computation
class CombinationsImp(outer: Combinations)(implicit p: Parameters) extends LazyRoCCModule(outer) with HasCoreParameters{
    //Accelerator states: idle, busy (accessing memory), resp (sending response)
    val s_idle :: s_busy :: s_resp :: Nil = Enum(Bits(), 3)
    val state = Reg(init = s_idle) //State idle until handling an instruction
    val tryStore = state === s_busy

    //Instruction inputs
    val length = Reg(init = io.cmd.bits.rs1) //Length of binary string
    val previous = Reg(init = io.cmd.bits.rs2) //Previous binary string
    val rd = Reg(init = io.cmd.bits.inst.rd) //Output location
    val function = Reg(init = io.cmd.bits.inst.funct) //Specific operation
    val currentAddress = Reg(UInt(64.W)) //The address to use for memory stores
    //Always-updated versions of the inputs
    val fastLength = Mux(io.cmd.fire(), io.cmd.bits.rs1, length)
    val fastPrevious = Mux(io.cmd.fire(), io.cmd.bits.rs2, previous)

    //Answers for each function: FixedWeight, General, Ranged, then memory versions of each (functions 0-6)
    val outputs = Array(nextCombination.fixedWeight(fastLength(5,0), fastPrevious), nextCombination.generalCombinations(fastLength(5,0), fastPrevious), nextCombination.rangedCombinations(fastLength(5,0), fastPrevious, fastLength(11,6), fastLength(17,12)))
    
    //Command and response states
    io.cmd.ready := state === s_idle
    io.resp.valid := state === s_resp

    //Accelerator response data
    val summedReturns = Reg(init = 0.U(64.W))
    //For a 3-bit function code, bit 2 sets whether memory is used or not, and bits 1 and 0 set which combination to use
    val lookups = Array(0.U->outputs(0),1.U->outputs(1), 2.U->outputs(2),
        4.U->summedReturns, 5.U->summedReturns, 6.U->summedReturns)
    io.resp.bits.data := MuxLookup(function, outputs(0), lookups)
    io.resp.bits.rd := rd


    //State control
    //Setup for processing commands
    when(io.cmd.fire()) {
        //Capture inputs
    	length := io.cmd.bits.rs1
    	rd := io.cmd.bits.inst.rd
    	function := io.cmd.bits.inst.funct

        //Whether it's a memory-using instruction or not (bit 2 set in the function code)
    	when(io.cmd.bits.inst.funct(2)===1.U) {
    	  state := s_busy
    	  summedReturns := 0.U
    	  currentAddress := io.cmd.bits.rs2
    	} .otherwise {
            previous := io.cmd.bits.rs2
    	    state := s_resp
    	}
    }

    //When done with an instruction
    when(io.resp.fire()) {
        state := s_idle
    }


    //Memory-access state
    val memAccesses = Reg(init = 0.U(4.W)) //Whether all memory requests have been resolved
    val accessesChange = Wire(UInt(4.W))
    accessesChange := (io.mem.req.fire() & 1.U(4.W)) - (io.mem.resp.valid & 1.U(4.W))//The latest amount of memory accesses either started or finished

    //Source of new combination data
    val nextCombinations = Array(memoryAccess.cycleCombinations(fastLength, io.mem.req.fire(), io.cmd.fire(), 0), memoryAccess.cycleCombinations(fastLength, io.mem.req.fire(), io.cmd.fire(), 1), memoryAccess.cycleCombinations(fastLength, io.mem.req.fire(), io.cmd.fire(), 2))
    val memLookups = Array(0.U -> nextCombinations(0), 1.U -> nextCombinations(1), 2.U -> nextCombinations(2))
    val combinationStream =  Wire(UInt(64.W))
    combinationStream := MuxLookup(function(1,0), nextCombinations(0), memLookups)

    //Request and response controls
    //When a request is sent, set up next cycle's response data
    when(io.mem.req.fire()) {
        memAccesses := memAccesses + accessesChange
        currentAddress := currentAddress + 8.U
        printf("next: %x address %x mem: %x\n", combinationStream, currentAddress, memAccesses)
    }

    //When a response is received, save response data
    when(io.mem.resp.valid) {
        memAccesses := memAccesses + accessesChange
        summedReturns := summedReturns + io.mem.resp.bits.data
        printf("tag: %x addr: %x mem %x\n", io.mem.resp.bits.tag, io.mem.resp.bits.addr, memAccesses)
    }


    //Controls for accessing memory
    val cycleOver = combinationStream === nextCombination.doneSignal
    val finished = cycleOver //&& memAccesses === 0.U

    //Switch out of memory mode when finished
    when(tryStore && finished) {
	    state := s_resp
    }


    //Memory request interface
    io.mem.req.valid := tryStore && !cycleOver
    io.busy := tryStore
    io.mem.req.bits.addr := currentAddress
    io.mem.req.bits.tag :=  combinationStream(5,0) //Change for out-of-order
    io.mem.req.bits.cmd := 1.U
    io.mem.req.bits.data := combinationStream //combinationStream
//    io.mem.req.bits.size := log2Ceil(8).U
//    io.mem.req.bits.signed := Bool(false)
    io.mem.req.bits.phys := Bool(false)

    //Always false
    io.interrupt := Bool(false)
}



//Generates binary string combinations based on input constraints and saves them to memory.
object memoryAccess {
    //Depending on the type for cycleCombinations, a different combination pattern will be used.
    def cycleCombinations(constraints: UInt, getNext: Bool, reset: Bool, kind: Int) : UInt = {
        val initial = Wire(UInt(64.W)) //The first value of the cycle
        if(kind == 1) { //The general cycle starts and ends with all 1s
            initial := (1.U << constraints(5,0)) - 1.U
        } else { //The other cycles start with lower 1s filled according to allowed weights
            initial := (1.U << constraints(11,6)) - 1.U
        }

        val nextSent = Reg(UInt(64.W)) //The value currently saved for storing to memory
        val result = kind match { //Calculate next value as the last is being stored
            case 0 => nextCombination.fixedWeight(constraints(5,0), nextSent)
            case 1 => nextCombination.generalCombinations(constraints(5,0), nextSent)
            case 2 => nextCombination.rangedCombinations(constraints(5,0), nextSent, constraints(11,6), constraints(17,12))
        }

        //Cycle by one when next value requested
        when(getNext) {
    	  nextSent := result
    	}

        //Start new cycle of the requested length when a reset is requested
    	when(reset) {
    	  nextSent := initial
    	}
    	nextSent
    }
}

//These methods generate the next combination for a certain function with the given parameters
object nextCombination {
    def doneSignal = UInt("hffffffff") //Signal to return upon a finished cycle (64 bit -1)

    //Generates a fixed-weight binary string based on a previous string of the same
    //weight and length. Binary strings up to length 32 will work.
    def fixedWeight(length: UInt, previous: UInt) : UInt = {
        //Calculations to generate the next combination (From Knuth's algorithm for Williams' 'cool' ordering)
        //Mask up to the right-most '10' of bits
        val trimmed = previous & (previous + 1.U) //Remove trailing 1s
        val trailed = trimmed ^ (trimmed - 1.U) //Make a mask for the right-most '10' onwards (if no '10', mask everything)
        val indexTrailed = trailed & previous //Mask the previous string

        //Create a duplicate of the mask if the bit before the last '10' is a 1
        val indexShift = trailed + 1.U //Set the bit to the left of the mask (or nothing set if mask is of everything)
        val subtracted = (indexShift & previous) - 1.U //If the bit masked by indexShift is 1, subtracted is the mask, if it is 0, subtracted has all bits set
        val fixed = Mux(subtracted.asSInt < 0.S, 0.U, subtracted) //If no '10', or the bit at indexShift isn't 1, fixed is 0, otherwise it is subtracted

        //Rotate masked bits to get the result, return if the cycle isn't over yet
        val result = previous + indexTrailed - fixed //Rotate the right side of the string starting from indexShift, or the whole string if indexShift not set
        val stopper = 1.U(1.W) << length(5,0) //Set the bit to the left of the binary string

        //Fill result with all 1s if finished
        Mux(result >> length =/= 0.U, doneSignal, result % stopper) //The end of the cycle has been reached if the bit at stopper is set in the new string
    }

    //Generates the next binary string of a certain length based on the cool-er ordering
    def generalCombinations(length: UInt, previous: UInt) : UInt = {
        //Calculations to generate the next combination (Algorithm by Maddie to generate Stevens' and Williams' 'cooler' orderings)
        //Mask up to the right-most '01' before the end of the string
        val trimmed = previous(31,1) | (previous(31,1) - 1.U) //Remove trailing 0s
        val trailed = trimmed ^ (trimmed + 1.U) //Make a mask for the right-most 01 onwards
        val mask = Wire(UInt(32.W)) //Shift the mask to a 32 bit wire instead of 31
        mask := (trailed << 1.U) + 1.U

        //Find the last spot in the mask, to use for rotating the 0th bit
        val lastTemp = Wire(UInt(32.W))
        lastTemp :=  trailed + 1.U //If there is a valid 01, this is the last bit
        val lastLimit = 1.U << (length(5,0) - 1.U) //Otherwise use the final bit
        val lastPosition = Mux(lastTemp > lastLimit || lastTemp === 0.U, lastLimit, lastTemp) //Choose which bit position to use

        val cap = 1.U << length(5,0) //One bit beyond the width of the string
        val first = Mux(mask < cap, 1.U & previous, 1.U & ~previous) //Flip the first bit if there is no valid 01
        val shifted = (previous & mask) >> 1.U //Shift the masked region
        val rotated = Mux(first === 1.U, shifted | lastPosition, shifted) //Move the first bit to the end of the shifting
        val result = rotated | (~mask & previous) //Combine the rotated and non-rotated parts of the string

        Mux(result === (cap - 1.U), doneSignal, result) //If finished, the result is all 1s
    }

    //Generates the next binary string within a weight range, based on cool-est ordering
    def rangedCombinations(length: UInt, previous: UInt, minWeight: UInt, maxWeight: UInt) : UInt = {
        //Calculations to generate the next combination (Algorithm by Maddie to generate Stevens' and Williams' 'coolest' orderings)
        //Mask up to the right-most '01' before the end of the string
        val trimmed = previous(31,1) | (previous(31,1) - 1.U) //Remove trailing 1s
        val trailed = trimmed ^ (trimmed + 1.U) //Make a mask for the right-most 01 onwards
        val mask = Wire(UInt(32.W)) //Shift the mask to a 32 bit wire instead of a 31 bit wire
        mask := (trailed << 1.U) + 1.U

        //Find the last spot in the mask, used for rotating the 0th bit
        val lastTemp = Wire(UInt(32.W))
        lastTemp :=  trailed + 1.U //If there is a valid '01', this is the last bit
        val lastLimit = 1.U << (length(5,0) - 1.U) //Otherwise use the string's final bit
        val lastPosition = Mux(lastTemp > lastLimit || lastTemp === 0.U, lastLimit, lastTemp) //Choose which bit position to use

        val count = Wire(UInt(32.W))
	    count := PopCount(previous(31,0)) //Count the number of set bits in the string, which should be within the weight constraints

        val cap = 1.U << length(5,0) //Set a bit one beyond the string's width
        val flipped = 1.U & ~previous //Take the complement of the 0th bit
        val valid = Mux(flipped === 0.U, count > minWeight, count < maxWeight) //Check if still a valid weight with that bit changed
        val first = Mux(mask < cap || !valid, 1.U & previous, flipped) //Flip the first bit if there is no valid 01
        val shifted = (previous & mask) >> 1.U //Shift the masked region

        //Flip the bit while rotating if no 01 and new string is valid
        val rotated = Mux(first === 1.U, shifted | lastPosition, shifted) //Move the first bit
        val result = rotated | (~mask & previous) //Add the first bit to the final result

        Mux(result === (1.U << minWeight) - 1.U, doneSignal, result) //Return -1 if finished
    }
}

// //Setup for the accelerator
// class WithCombinations extends Config((site, here, up) => {
//     case BuildRoCC => Seq((p: Parameters) => {
//         val Combinations = LazyModule.apply(new Combinations(OpcodeSet.custom0) (p))
//         Combinations
//     })
// })

/**
 * Add this into the example project's RocketConfigs.scala file:

class CombinationsRocketConfig extends Config(
    new WithTop ++
    new WithBootROM ++
    new freechips.rocketchip.subsystem.WithInclusiveCache ++
    new combinations.WithCombinations ++
    new freechips.rocketchip.subsystem.WithNBigCores(1) ++
    new freechips.rocketchip.system.BaseConfig
)

 */
