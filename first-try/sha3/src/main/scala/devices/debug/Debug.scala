// See LICENSE.SiFive for license details.

package freechips.rocketchip.devices.debug


import Chisel._
import chisel3.experimental._
import freechips.rocketchip.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.rocket.Instructions
import freechips.rocketchip.tile.MaxHartIdBits
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property._
import freechips.rocketchip.devices.debug.systembusaccess._
import freechips.rocketchip.diplomaticobjectmodel.logicaltree.{DebugLogicalTreeNode, LogicalModuleTree}
import freechips.rocketchip.diplomaticobjectmodel.model._
import freechips.rocketchip.amba.apb.{APBToTL, APBFanout}
import freechips.rocketchip.util.BooleanToAugmentedBoolean

object DsbBusConsts {
  def sbAddrWidth = 12
  def sbIdWidth   = 10 

}

object DsbRegAddrs{

  // These are used by the ROM.
  def HALTED       = 0x100
  def GOING        = 0x104
  def RESUMING     = 0x108
  def EXCEPTION    = 0x10C

  def WHERETO      = 0x300
  // This needs to be aligned for up to lq/sq

  
  // This shows up in HartInfo, and needs to be aligned
  // to enable up to LQ/SQ instructions.
  def DATA         = 0x380

  // We want DATA to immediately follow PROGBUF so that we can
  // use them interchangeably. Leave another slot if there is an
  // implicit ebreak.
  def PROGBUF(cfg:DebugModuleParams) = {
    val tmp = DATA - (cfg.nProgramBufferWords * 4)
    if (cfg.hasImplicitEbreak) (tmp - 4) else tmp
  }
  // This is unused if hasImpEbreak is false, and just points to the end of the PROGBUF.
  def IMPEBREAK(cfg: DebugModuleParams) = { DATA - 4 }

  // We want abstract to be immediately before PROGBUF
  // because we auto-generate 2 instructions.
  def ABSTRACT(cfg:DebugModuleParams) = PROGBUF(cfg) - 8

  def FLAGS        = 0x400
  def ROMBASE      = 0x800
 
}

/** Enumerations used both in the hardware
  * and in the configuration specification.
  */

object DebugModuleAccessType extends scala.Enumeration {
  type DebugModuleAccessType = Value
  val Access8Bit, Access16Bit, Access32Bit, Access64Bit, Access128Bit = Value
}
import DebugModuleAccessType._

object DebugAbstractCommandError extends scala.Enumeration {
  type DebugAbstractCommandError = Value
  val Success, ErrBusy, ErrNotSupported, ErrException, ErrHaltResume = Value
}
import DebugAbstractCommandError._

object DebugAbstractCommandType extends scala.Enumeration {
  type DebugAbstractCommandType = Value
  val AccessRegister, QuickAccess  = Value
}
import DebugAbstractCommandType._

/** Parameters exposed to the top-level design, set based on
  * external requirements, etc.
  *
  *  This object checks that the parameters conform to the 
  *  full specification. The implementation which receives this
  *  object can perform more checks on what that implementation
  *  actually supports.
  *  nComponents : The number of components to support debugging.
  *  nDMIAddrSize : Size of the Debug Bus Address
  *  nAbstractDataWords: Number of 32-bit words for Abstract Commands
  *  nProgamBufferWords: Number of 32-bit words for Program Buffer
  *  hasBusMaster: Whether or not a bus master should be included
  *  maxSupportedSBAccess: Maximum transaction size supported by System Bus Access logic.
  *  supportQuickAccess : Whether or not to support the quick access command.
  *  supportHartArray : Whether or not to implement the hart array register (if >1 hart).
  *  hasImplicitEbreak: There is an additional RO program buffer word containing an ebreak
  **/

case class DebugModuleParams (
  nDMIAddrSize  : Int = 7,
  nProgramBufferWords: Int = 16,
  nAbstractDataWords : Int = 4,
  nScratch : Int = 1,
  hasBusMaster : Boolean = false,
  clockGate : Boolean = true,
  maxSupportedSBAccess : Int = 32,
  supportQuickAccess : Boolean = false,
  supportHartArray   : Boolean = true,
  nHaltGroups        : Int = 1,
  nExtTriggers       : Int = 0,
  hasHartResets      : Boolean = false,
  hasImplicitEbreak  : Boolean = false,
  hasAuthentication  : Boolean = false
) {

  require ((nDMIAddrSize >= 7) && (nDMIAddrSize <= 32), s"Legal DMIAddrSize is 7-32, not ${nDMIAddrSize}")

  require ((nAbstractDataWords  > 0)  && (nAbstractDataWords  <= 16), s"Legal nAbstractDataWords is 0-16, not ${nAbstractDataWords}")
  require ((nProgramBufferWords >= 0) && (nProgramBufferWords <= 16), s"Legal nProgramBufferWords is 0-16, not ${nProgramBufferWords}")

  require (nHaltGroups < 32, s"Legal nHaltGroups is 0-31, not ${nHaltGroups}")
  require (nExtTriggers <= 16, s"Legal nExtTriggers is 0-16, not ${nExtTriggers}")

  if (supportQuickAccess) {
    // TODO: Check that quick access requirements are met.
  }

  def address = AddressSet(0, 0xFFF) // This is required for correct functionality; it's not configurable.
}

object DefaultDebugModuleParams {

  def apply(xlen:Int /*TODO , val configStringAddr: Int*/): DebugModuleParams = {
    new DebugModuleParams().copy(
      nAbstractDataWords   = (if (xlen == 32) 1 else if (xlen == 64) 2 else 4),
      maxSupportedSBAccess = xlen
    )
  }
}

case object DebugModuleKey extends Field[Option[DebugModuleParams]](Some(DebugModuleParams()))

/** Functional parameters exposed to the design configuration.
  *
  *  hartIdToHartSel: For systems where hart ids are not 1:1 with hartsel, provide the mapping.
  *  hartSelToHartId: Provide inverse mapping of the above
  **/
case class DebugModuleHartSelFuncs (
  hartIdToHartSel : (UInt) => UInt = (x:UInt) => x,
  hartSelToHartId : (UInt) => UInt = (x:UInt) => x
)

case object DebugModuleHartSelKey extends Field(DebugModuleHartSelFuncs())

class DebugExtTriggerOut (nExtTriggers: Int) extends Bundle {
  val req = Output(UInt(nExtTriggers.W))
  val ack = Input(UInt(nExtTriggers.W))
}

class DebugExtTriggerIn (nExtTriggers: Int) extends Bundle {
  val req = Input(UInt(nExtTriggers.W))
  val ack = Output(UInt(nExtTriggers.W))
}

class DebugExtTriggerIO () (implicit val p: Parameters) extends ParameterizedBundle()(p) {
  val out = new DebugExtTriggerOut(p(DebugModuleKey).get.nExtTriggers)
  val in  = new DebugExtTriggerIn (p(DebugModuleKey).get.nExtTriggers)
}

class DebugAuthenticationIO () (implicit val p: Parameters) extends ParameterizedBundle()(p) {
  val dmactive    = Output(Bool())
  val dmAuthWrite = Output(Bool())
  val dmAuthRead  = Output(Bool())
  val dmAuthWdata = Output(UInt(32.W))
  val dmAuthBusy  = Input(Bool())
  val dmAuthRdata = Input(UInt(32.W))
  val dmAuthenticated = Input(Bool())
}

// *****************************************
// Module Interfaces
// 
// *****************************************

/* structure for passing hartsel between the "Outer" and "Inner"
 */

class DebugInternalBundle (val nComponents: Int)(implicit val p: Parameters) extends ParameterizedBundle()(p) {
  val resumereq    = Bool()
  val hartsel      = UInt(10.W)
  val ackhavereset = Bool()
  val hasel        = Bool()
  val hamask       = Vec(nComponents, Bool())
  val hrmask       = Vec(nComponents, Bool())
}

/* structure for top-level Debug Module signals which aren't the bus interfaces.
 */

class DebugCtrlBundle (nComponents: Int)(implicit val p: Parameters) extends ParameterizedBundle()(p) {
  val debugUnavail    = Vec(nComponents, Bool()).asInput
  val ndreset         = Bool(OUTPUT)
  val dmactive        = Bool(OUTPUT)
}

// *****************************************
// Debug Module 
// 
// *****************************************

/** Parameterized version of the Debug Module defined in the
  *  RISC-V Debug Specification 
  *  
  *  DebugModule is a slave to two asynchronous masters:
  *    The Debug Bus (DMI) -- This is driven by an external debugger
  *  
  *    The System Bus -- This services requests from the cores. Generally
  *                      this interface should only be active at the request
  *                      of the debugger, but the Debug Module may also 
  *                      provide the default MTVEC since it is mapped
  *                      to address 0x0.
  *  
  *  DebugModule is responsible for control registers and RAM, and
  *  Debug ROM. It runs partially off of the dmiClk (e.g. TCK) and
  *  the TL clock. Therefore, it is divided into "Outer" portion (running
  *  of off dmiClock and dmiReset) and "Inner" (running off tlClock and tlReset).
  *  This allows DMCONTROL.haltreq, hartsel, hasel, hawindowsel, hawindow, dmactive,
  *  and ndreset to be modified even while the Core is in reset or not being clocked.
  *  Not all reads from the Debugger to the Debug Module will actually complete
  *  in these scenarios either, they will just block until tlClock and tlReset
  *  allow them to complete. This is not strictly necessary for 
  *  proper debugger functionality.
  */

// Local reg mapper function : Notify when written, but give the value as well.  
object WNotifyWire {
  def apply(n: Int, value: UInt, set: Bool, name: String, desc: String) : RegField = {
    RegField(n, UInt(0), RegWriteFn((valid, data) => {
      set := valid
      value := data
      Bool(true)
    }), Some(RegFieldDesc(name = name, desc = desc,
      access = RegFieldAccessType.W)))
  }
}

// Local reg mapper function : Notify when accessed either as read or write.
object RWNotify {
    def apply (n: Int, rVal: UInt, wVal: UInt, rNotify: Bool, wNotify: Bool, desc: Option[RegFieldDesc] = None): RegField = {
      RegField(n,
        RegReadFn ((ready)       => {rNotify := ready ; (Bool(true), rVal)}),
        RegWriteFn((valid, data) => {
          wNotify := valid
          when (valid) {wVal := data}
          Bool(true)
        }
        ), desc)
    }
}

// Local reg mapper function : Notify with value when written, take read input as presented.
//   This allows checking or correcting the write value before storing it in the register field.
object WNotifyVal {
  def apply(n: Int, rVal: UInt, wVal: UInt, wNotify: Bool, desc: RegFieldDesc): RegField = {
    RegField(n, rVal, RegWriteFn((valid, data) => {
      wNotify := valid
      wVal := data
      true.B
    }
    ), desc)
  }
}

class TLDebugModuleOuter(device: Device)(implicit p: Parameters) extends LazyModule {

  // For Shorter Register Names
  import DMI_RegAddrs._

  val cfg = p(DebugModuleKey).get

  val intnode = IntNexusNode(
    sourceFn       = { _ => IntSourcePortParameters(Seq(IntSourceParameters(1, Seq(Resource(device, "int"))))) },
    sinkFn         = { _ => IntSinkPortParameters(Seq(IntSinkParameters())) },
    outputRequiresInput = false)

  val dmiNode = TLRegisterNode (
    address = AddressSet.misaligned(DMI_DMCONTROL   << 2, 4) ++
              AddressSet.misaligned(DMI_HAWINDOWSEL << 2, 4) ++
              AddressSet.misaligned(DMI_HAWINDOW    << 2, 4),
    device = device,
    beatBytes = 4,
    executable = false
  )

  lazy val module = new LazyModuleImp(this) {
    require (intnode.edges.in.size == 0, "Debug Module does not accept interrupts")

    val nComponents = intnode.out.size
    def getNComponents = () => nComponents

    val supportHartArray = cfg.supportHartArray && (nComponents > 1)    // no hart array if only one hart

    val io = IO(new Bundle {
      val ctrl = (new DebugCtrlBundle(nComponents))
      val innerCtrl = new DecoupledIO(new DebugInternalBundle(nComponents))
      val hgDebugInt = Vec(nComponents, Bool()).asInput
      val hartResetReq = cfg.hasHartResets.option(Output(Vec(nComponents, Bool())))
      val dmAuthenticated = cfg.hasAuthentication.option(Input(Bool()))
    })

    val dmAuthenticated = io.dmAuthenticated.map( dma =>
      AsyncResetSynchronizerShiftReg(dma, 3, false.B, Some("dmAuthenticated_sync"))).getOrElse(true.B)

    //----DMCONTROL (The whole point of 'Outer' is to maintain this register on dmiClock (e.g. TCK) domain, so that it
    //               can be written even if 'Inner' is not being clocked or is in reset. This allows halting
    //               harts while the rest of the system is in reset. It doesn't really allow any other
    //               register accesses, which will keep returning 'busy' to the debugger interface.

    val DMCONTROLReset = Wire(init = (new DMCONTROLFields().fromBits(0.U)))
    val DMCONTROLNxt = Wire(init = new DMCONTROLFields().fromBits(0.U))

    val DMCONTROLReg = Wire(init = new DMCONTROLFields().fromBits(AsyncResetReg(updateData = DMCONTROLNxt.asUInt,
      resetData = BigInt(0),
      enable = true.B,
      name = "DMCONTROL"
    )))

    val hartsel_mask = if (nComponents > 1) ((1 << p(MaxHartIdBits)) - 1).U else 0.U
    val DMCONTROLWrData = Wire(init = new DMCONTROLFields().fromBits(0.U))
    val dmactiveWrEn        = Wire(init = false.B)
    val ndmresetWrEn        = Wire(init = false.B)
    val clrresethaltreqWrEn = Wire(init = false.B)
    val setresethaltreqWrEn = Wire(init = false.B)
    val hartselloWrEn       = Wire(init = false.B)
    val haselWrEn           = Wire(init = false.B)
    val ackhaveresetWrEn    = Wire(init = false.B)
    val hartresetWrEn       = Wire(init = false.B)
    val resumereqWrEn       = Wire(init = false.B)
    val haltreqWrEn         = Wire(init = false.B)

    val dmactive = DMCONTROLReg.dmactive

    DMCONTROLNxt := DMCONTROLReg
    when (~dmactive) {
      DMCONTROLNxt := DMCONTROLReset
    } .otherwise {
      when (dmAuthenticated && ndmresetWrEn)  { DMCONTROLNxt.ndmreset     := DMCONTROLWrData.ndmreset }
      when (dmAuthenticated && hartselloWrEn) { DMCONTROLNxt.hartsello    := DMCONTROLWrData.hartsello & hartsel_mask}
      when (dmAuthenticated && haselWrEn)     { DMCONTROLNxt.hasel        := DMCONTROLWrData.hasel }
      when (dmAuthenticated && hartresetWrEn) { DMCONTROLNxt.hartreset    := DMCONTROLWrData.hartreset }
      when (dmAuthenticated && haltreqWrEn)   { DMCONTROLNxt.haltreq      := DMCONTROLWrData.haltreq }
    }

    // Put this last to override its own effects.
    when (dmactiveWrEn) {
      DMCONTROLNxt.dmactive := DMCONTROLWrData.dmactive
    }

    //--------------------------------------------------------------
    // Hart array mask and window
    //  HAMASK is 1 bit per component
    //  HAWINDOWSEL selects a 32-bit slice of HAMASK to be visible for read/write in HAWINDOW
    //--------------------------------------------------------------
    val hamask = Wire(init = Vec.fill(nComponents){false.B})
    def haWindowSize = 32

      // The following need to be declared even if supportHartArray is false due to reference
      // at compile time by dmiNode.regmap
    val HAWINDOWSELWrData = Wire(init = (new HAWINDOWSELFields()).fromBits(0.U))
    val HAWINDOWSELWrEn   = Wire(init = false.B)

    val HAWINDOWRdData = Wire(init = (new HAWINDOWFields()).fromBits(0.U))
    val HAWINDOWWrData = Wire(init = (new HAWINDOWFields()).fromBits(0.U))
    val HAWINDOWWrEn   = Wire(init = false.B)

    def hartSelected(hart: Int): Bool = {
      ((io.innerCtrl.bits.hartsel === hart.U) ||
        (if (supportHartArray) io.innerCtrl.bits.hasel && io.innerCtrl.bits.hamask(hart) else false.B))
    }

    val HAWINDOWSELNxt = Wire(init = (new HAWINDOWSELFields().fromBits(0.U)))
    val HAWINDOWSELReg = Wire(init = new HAWINDOWSELFields().fromBits(AsyncResetReg(updateData = HAWINDOWSELNxt.asUInt,
      resetData = 0,
      enable = true.B,
      name = "HAWINDOWSELReg"
    )))

    if (supportHartArray) {
      val HAWINDOWSELReset = Wire(init = (new HAWINDOWSELFields().fromBits(0.U)))

      HAWINDOWSELNxt := HAWINDOWSELReg
      when (~dmactive || ~dmAuthenticated) {
        HAWINDOWSELNxt := HAWINDOWSELReset
      } .otherwise {
        when (HAWINDOWSELWrEn) {
            // Unneeded upper bits of HAWINDOWSEL are tied to 0.  Entire register is 0 if all harts fit in one window
          if (nComponents > haWindowSize) {
            HAWINDOWSELNxt.hawindowsel := HAWINDOWSELWrData.hawindowsel & ((1 << (log2Up(nComponents) - 5)) - 1).U
          } else {
            HAWINDOWSELNxt.hawindowsel := 0.U
          }
        }
      }

      val numHAMASKSlices = ((nComponents - 1)/haWindowSize)+1
      HAWINDOWRdData.maskdata := 0.U     // default, overridden below
      for (ii <- 0 until numHAMASKSlices) {
        val sliceMask = if (nComponents > ((ii*haWindowSize) + haWindowSize-1)) 0xFFFFFFFF  // All harts in this slice exist
                        else (1<<(nComponents - (ii*haWindowSize))) - 1         // Partial last slice
        val HAMASKRst = Wire(init = (new HAWINDOWFields().fromBits(0.U)))
        val HAMASKNxt = Wire(init = (new HAWINDOWFields().fromBits(0.U)))
        val HAMASKReg = Wire(init = Vec(AsyncResetReg(updateData = HAMASKNxt.asUInt,
          resetData = 0,
          enable = true.B,
          name = s"HAMASKReg${ii}")))

        when (ii.U === HAWINDOWSELReg.hawindowsel) {
          HAWINDOWRdData.maskdata := HAMASKReg.asUInt & sliceMask.U
        }

        HAMASKNxt.maskdata := HAMASKReg.asUInt
        when (~dmactive || ~dmAuthenticated) {
          HAMASKNxt := HAMASKRst
        }.otherwise {
          when (HAWINDOWWrEn && (ii.U === HAWINDOWSELReg.hawindowsel)) {
            HAMASKNxt.maskdata := HAWINDOWWrData.maskdata
          }
        }

         // drive each slice of hamask with stored HAMASKReg or with new value being written
        for (jj <- 0 until haWindowSize) {
          if (((ii*haWindowSize) + jj) < nComponents) {
            val tempWrData = HAWINDOWWrData.maskdata.asBools
            val tempMaskReg = HAMASKReg.asUInt.asBools
            when (dmAuthenticated && HAWINDOWWrEn && (ii.U === HAWINDOWSELReg.hawindowsel)) {
              hamask(ii*haWindowSize + jj) := tempWrData(jj)
            }.otherwise {
              hamask(ii*haWindowSize + jj) := tempMaskReg(jj)
            }
          }
        }
      }
    }

    //--------------------------------------------------------------
    // Halt-on-reset
    //  hrmaskReg is current set of harts that should halt-on-reset
    //    Reset state (dmactive=0) is all zeroes
    //    Bits are set by writing 1 to DMCONTROL.setresethaltreq
    //    Bits are cleared by writing 1 to DMCONTROL.clrresethaltreq
    //    Spec says if both are 1, then clrresethaltreq is executed
    //--------------------------------------------------------------
    val hrmask    = Wire(Vec(nComponents, Bool()))
    val hrmaskNxt = Wire(Vec(nComponents, Bool()))
    val hrmaskReg = Wire(init = Vec(AsyncResetReg(updateData = hrmaskNxt.asUInt,
      resetData = 0,
      enable = true.B,
      name = "hrmaskReg").asBools))

    hrmaskNxt := hrmaskReg
    for (component <- 0 until nComponents) {
      when (~dmAuthenticated) {
        hrmaskNxt(component) := false.B
      }.elsewhen (clrresethaltreqWrEn && DMCONTROLWrData.clrresethaltreq && hartSelected(component)) {
        hrmaskNxt(component) := false.B
      }.elsewhen (setresethaltreqWrEn && DMCONTROLWrData.setresethaltreq && hartSelected(component)) {
        hrmaskNxt(component) := true.B
      }
    }
    hrmask := Mux(clrresethaltreqWrEn | setresethaltreqWrEn, hrmaskNxt, hrmaskReg)


    val dmControlRegFields = RegFieldGroup("dmcontrol", Some("debug module control register"), Seq(
      WNotifyVal(1, DMCONTROLReg.dmactive,    DMCONTROLWrData.dmactive, dmactiveWrEn,
        RegFieldDesc("dmactive", "debug module active", reset=Some(0))),
      WNotifyVal(1, DMCONTROLReg.ndmreset,    DMCONTROLWrData.ndmreset, ndmresetWrEn,
        RegFieldDesc("ndmreset", "debug module reset output", reset=Some(0))),
      WNotifyVal(1, 0.U,                      DMCONTROLWrData.clrresethaltreq, clrresethaltreqWrEn,
        RegFieldDesc("clrresethaltreq", "clear reset halt request", reset=Some(0), access=RegFieldAccessType.W)),
      WNotifyVal(1, 0.U,                      DMCONTROLWrData.setresethaltreq, setresethaltreqWrEn,
        RegFieldDesc("setresethaltreq", "set reset halt request",   reset=Some(0), access=RegFieldAccessType.W)),
      RegField(12),
      if (nComponents > 1) WNotifyVal(p(MaxHartIdBits),
                      DMCONTROLReg.hartsello, DMCONTROLWrData.hartsello, hartselloWrEn,
        RegFieldDesc("hartsello",       "hart select low", reset=Some(0)))
      else RegField(1),
      if (nComponents > 1) RegField(10-p(MaxHartIdBits))
      else RegField(9),
      if (supportHartArray)
        WNotifyVal(1, DMCONTROLReg.hasel,     DMCONTROLWrData.hasel, haselWrEn,
        RegFieldDesc("hasel",           "hart array select", reset=Some(0)))
      else RegField(1),
      RegField(1),
      WNotifyVal(1, 0.U,                      DMCONTROLWrData.ackhavereset, ackhaveresetWrEn,
        RegFieldDesc("ackhavereset",    "acknowledge reset", reset=Some(0),  access=RegFieldAccessType.W)),
      if (cfg.hasHartResets)
        WNotifyVal(1, DMCONTROLReg.hartreset, DMCONTROLWrData.hartreset, hartresetWrEn,
        RegFieldDesc("hartreset",       "hart reset request", reset=Some(0)))
      else RegField(1),
      WNotifyVal(1, 0.U,                      DMCONTROLWrData.resumereq, resumereqWrEn,
        RegFieldDesc("resumereq",       "resume request", reset=Some(0), access=RegFieldAccessType.W)),
      WNotifyVal(1, DMCONTROLReg.haltreq,     DMCONTROLWrData.haltreq, haltreqWrEn,     // Spec says W, but maintaining previous behavior
        RegFieldDesc("haltreq",         "halt request", reset=Some(0)))
    ))


    //--------------------------------------------------------------
    // DMI register decoder for Outer
    //--------------------------------------------------------------
      // regmap addresses are byte offsets from lowest address
    def DMI_DMCONTROL_OFFSET   = 0
    def DMI_HAWINDOWSEL_OFFSET = ((DMI_HAWINDOWSEL - DMI_DMCONTROL) << 2)
    def DMI_HAWINDOW_OFFSET    = ((DMI_HAWINDOW - DMI_DMCONTROL) << 2)

    val omRegMap = dmiNode.regmap(
      DMI_DMCONTROL_OFFSET   -> dmControlRegFields,
      DMI_HAWINDOWSEL_OFFSET -> (if (supportHartArray && (nComponents > 32)) Seq(
        WNotifyVal(log2Up(nComponents)-5, HAWINDOWSELReg.hawindowsel, HAWINDOWSELWrData.hawindowsel, HAWINDOWSELWrEn,
        RegFieldDesc("hawindowsel", "hart array window select", reset=Some(0)))) else Nil),
      DMI_HAWINDOW_OFFSET    -> (if (supportHartArray) Seq(
        WNotifyVal(if (nComponents > 31) 32 else nComponents, HAWINDOWRdData.maskdata, HAWINDOWWrData.maskdata, HAWINDOWWrEn,
        RegFieldDesc("hawindow", "hart array window", reset=Some(0), volatile=(nComponents > 32)))) else Nil)
    )

    //--------------------------------------------------------------
    // Interrupt Registers
    //--------------------------------------------------------------

    val debugIntNxt = Wire(init = Vec.fill(nComponents){false.B})
    val debugIntRegs = Wire(init = Vec(AsyncResetReg(updateData = debugIntNxt.asUInt,
      resetData = 0,
      enable = true.B,
      name = "debugInterrupts").asBools))

    debugIntNxt := debugIntRegs

    val (intnode_out, _) = intnode.out.unzip
    for (component <- 0 until nComponents) {
      intnode_out(component)(0) := debugIntRegs(component) | io.hgDebugInt(component)
    }

    // Halt request registers are set & cleared by writes to DMCONTROL.haltreq
    // resumereq also causes the core to execute a 'dret',
    // so resumereq is passed through to Inner.
    // hartsel/hasel/hamask must also be used by the DebugModule state machine,
    // so it is passed to Inner.
    // It is true that there is no backpressure -- writes
    // which occur 'too fast' will be dropped.

    for (component <- 0 until nComponents) {
      when (~dmactive || ~dmAuthenticated) {
        debugIntNxt(component) := false.B
      }. otherwise {
        when (haltreqWrEn && ((DMCONTROLWrData.hartsello === component.U)
          || (if (supportHartArray) DMCONTROLWrData.hasel && hamask(component) else false.B))) {
          debugIntNxt(component) := DMCONTROLWrData.haltreq
        }
      }
    }

    io.innerCtrl.valid := dmAuthenticated && (hartselloWrEn | resumereqWrEn | ackhaveresetWrEn | setresethaltreqWrEn | clrresethaltreqWrEn | haselWrEn |
       (HAWINDOWWrEn & supportHartArray.B))
    io.innerCtrl.bits.hartsel      := Mux(hartselloWrEn, DMCONTROLWrData.hartsello, DMCONTROLReg.hartsello)
    io.innerCtrl.bits.resumereq    := resumereqWrEn & DMCONTROLWrData.resumereq    // This bit is W1
    io.innerCtrl.bits.ackhavereset := ackhaveresetWrEn & DMCONTROLWrData.ackhavereset
    io.innerCtrl.bits.hrmask       := hrmask
    if (supportHartArray) {
      io.innerCtrl.bits.hasel      := Mux(haselWrEn, DMCONTROLWrData.hasel, DMCONTROLReg.hasel)
      io.innerCtrl.bits.hamask     := hamask
    }

    io.ctrl.ndreset := DMCONTROLReg.ndmreset
    io.ctrl.dmactive := DMCONTROLReg.dmactive

    if (cfg.hasHartResets) {
      val hartResetNxt = Wire(Vec(nComponents, Bool()))
      val hartResetReg = Wire(init = Vec(AsyncResetReg(updateData = hartResetNxt.asUInt,
        resetData = 0,
        enable = true.B,
        name = "hartResetReg").asBools))

      for (component <- 0 until nComponents) {
        hartResetNxt(component) := DMCONTROLReg.hartreset & hartSelected(component)
        io.hartResetReq.get(component) := hartResetReg(component)
      }
    }
  }
}

class TLDebugModuleOuterAsync(device: Device)(implicit p: Parameters) extends LazyModule {

  val dmiXbar = LazyModule (new TLXbar())

  val dmi2tlOpt = (!p(ExportDebug).apb).option({
    val dmi2tl = LazyModule(new DMIToTL())
    dmiXbar.node := dmi2tl.node
    dmi2tl
  })

  val apbNodeOpt = p(ExportDebug).apb.option({
    val apb2tl = LazyModule(new APBToTL())
    val apb2tlBuffer = LazyModule(new TLBuffer(BufferParams.pipe))
    val apbXbar = LazyModule(new APBFanout())
    val apbRegs = LazyModule(new APBDebugRegisters())

    apbRegs.node := apbXbar.node
    apb2tl.node  := apbXbar.node
    apb2tlBuffer.node := apb2tl.node
    dmiXbar.node := apb2tlBuffer.node

    apbXbar.node
  })

  val dmOuter = LazyModule( new TLDebugModuleOuter(device))
  val intnode = IntSyncCrossingSource(alreadyRegistered = true) :*= dmOuter.intnode

  val dmiInnerNode = TLAsyncCrossingSource() := dmiXbar.node
  dmOuter.dmiNode := dmiXbar.node
  
  lazy val module = new LazyModuleImp(this) {

    val nComponents = dmOuter.intnode.edges.out.size

    val io = IO(new Bundle {
      val dmi   = (!p(ExportDebug).apb).option(new DMIIO()(p).flip())
      // Optional APB Interface is fully diplomatic so is not listed here.
      val ctrl = new DebugCtrlBundle(nComponents)
      val innerCtrl = new AsyncBundle(new DebugInternalBundle(nComponents), AsyncQueueParams.singleton())
      val hgDebugInt = Vec(nComponents, Bool()).asInput
      val hartResetReq = p(DebugModuleKey).get.hasHartResets.option(Output(Vec(nComponents, Bool())))
      val dmAuthenticated = p(DebugModuleKey).get.hasAuthentication.option(Input(Bool()))
    })

    dmi2tlOpt.foreach { _.module.io.dmi <> io.dmi.get }

    io.ctrl <> dmOuter.module.io.ctrl
    io.innerCtrl := ToAsyncBundle(dmOuter.module.io.innerCtrl, AsyncQueueParams.singleton())
    dmOuter.module.io.hgDebugInt := io.hgDebugInt
    io.hartResetReq.foreach { x => dmOuter.module.io.hartResetReq.foreach {y => x := y}}
    io.dmAuthenticated.foreach { x => dmOuter.module.io.dmAuthenticated.foreach { y => y := x}}
  }
}

class TLDebugModuleInner(device: Device, getNComponents: () => Int, beatBytes: Int)(implicit p: Parameters) extends LazyModule
{

  // For Shorter Register Names
  import DMI_RegAddrs._

  val cfg = p(DebugModuleKey).get
  def getCfg = () => cfg
  val hartSelFuncs = p(DebugModuleHartSelKey)

  val dmiNode = TLRegisterNode(
       // Address is range 0 to 0x1FF except DMCONTROL, HAWINDOWSEL, HAWINDOW which are handled by Outer
    address = AddressSet.misaligned(0, DMI_DMCONTROL << 2) ++
              AddressSet.misaligned((DMI_DMCONTROL + 1) << 2, ((DMI_HAWINDOWSEL << 2) - ((DMI_DMCONTROL + 1) << 2))) ++
              AddressSet.misaligned((DMI_HAWINDOW + 1) << 2, (0x200 - ((DMI_HAWINDOW + 1) << 2))),
    device = device,
    beatBytes = 4,
    executable = false
  )

  val tlNode = TLRegisterNode(
    address=Seq(cfg.address),
    device=device,
    beatBytes=beatBytes,
    executable=true
  )

  val sb2tlOpt = cfg.hasBusMaster.option(LazyModule(new SBToTL()))

  // If we want to support custom registers read through Abstract Commands,
  // provide a place to bring them into the debug module. What this connects
  // to is up to the implementation.
  val customNode = new DebugCustomSink()

  lazy val module = new LazyModuleImp(this){
    val nComponents = getNComponents()
    Annotated.params(this, cfg)
    val supportHartArray = cfg.supportHartArray & (nComponents > 1)
    val nExtTriggers = cfg.nExtTriggers
    val nHaltGroups = if ((nComponents > 1) | (nExtTriggers > 0)) cfg.nHaltGroups
      else 0  // no halt groups possible if single hart with no external triggers

    val io = IO(new Bundle {
      val dmactive = Bool(INPUT)
      val innerCtrl = (new DecoupledIO(new DebugInternalBundle(nComponents))).flip
      val debugUnavail = Vec(nComponents, Bool()).asInput
      val hgDebugInt = Vec(nComponents, Bool()).asOutput
      val extTrigger = (nExtTriggers > 0).option(new DebugExtTriggerIO())
      val hartReset  = cfg.hasHartResets.option(Input(Vec(nComponents, Bool())))
      val auth = cfg.hasAuthentication.option(new DebugAuthenticationIO())
    })


    //--------------------------------------------------------------
    // Import constants for shorter variable names
    //--------------------------------------------------------------

    import DMI_RegAddrs._
    import DsbRegAddrs._
    import DsbBusConsts._
    import DMIConsts._

    //--------------------------------------------------------------
    // Sanity Check Configuration For this implementation.
    //--------------------------------------------------------------

    require (cfg.supportQuickAccess == false, "No Quick Access support yet")
    require ((nHaltGroups > 0) || (nExtTriggers == 0), "External triggers require at least 1 halt group")

    //--------------------------------------------------------------
    // Register & Wire Declarations (which need to be pre-declared)
    //--------------------------------------------------------------

    val haltedBitRegs    = Reg(Vec(nComponents, Bool()))
    val resumeReqRegs    = Reg(Vec(nComponents, Bool()))
    val haveResetBitRegs = Reg(Vec(nComponents, Bool()))

    // --- regmapper outputs

    val hartHaltedWrEn       = Wire(Bool())
    val hartHaltedId         = Wire(UInt(sbIdWidth.W))
    val hartGoingWrEn        = Wire(Bool())
    val hartGoingId          = Wire(UInt(sbIdWidth.W))
    val hartResumingWrEn     = Wire(Bool())
    val hartResumingId       = Wire(UInt(sbIdWidth.W))
    val hartExceptionWrEn    = Wire(Bool())
    val hartExceptionId      = Wire(UInt(sbIdWidth.W))

    val dmiProgramBufferRdEn = Wire(init = Vec.fill(cfg.nProgramBufferWords * 4){false.B})
    val dmiProgramBufferAccessLegal = Wire(init = false.B)
    val dmiProgramBufferWrEnMaybe = Wire(init = Vec.fill(cfg.nProgramBufferWords * 4){false.B})

    val dmiAbstractDataRdEn = Wire(init = Vec.fill(cfg.nAbstractDataWords * 4){false.B})
    val dmiAbstractDataAccessLegal = Wire (init = false.B)
    val dmiAbstractDataWrEnMaybe = Wire(init = Vec.fill(cfg.nAbstractDataWords * 4){false.B})

    //--------------------------------------------------------------
    // Registers coming from 'CONTROL' in Outer
    //--------------------------------------------------------------

    val dmAuthenticated = io.auth.map(a => a.dmAuthenticated).getOrElse(true.B)

    val selectedHartReg = Reg(UInt(p(MaxHartIdBits).W))
      // hamaskFull is a vector of all selected harts including hartsel, whether or not supportHartArray is true
    val hamaskFull = Wire(init = Vec.fill(nComponents){false.B})

    if (nComponents > 1) {
      when (~io.dmactive) {
        selectedHartReg := 0.U
      }.elsewhen (io.innerCtrl.fire()){
        selectedHartReg := io.innerCtrl.bits.hartsel
      }
    }

    if (supportHartArray) withReset (~io.dmactive || ~dmAuthenticated) {
      val hamaskZero = Wire(init = Vec.fill(nComponents){false.B})
      val hamaskReg = RegInit(Vec.fill(nComponents){false.B})
      when (io.innerCtrl.fire()){
        hamaskReg := Mux(io.innerCtrl.bits.hasel, io.innerCtrl.bits.hamask, hamaskZero)
      }
      hamaskFull := hamaskReg
    }

    when (selectedHartReg < nComponents.U) {
      hamaskFull(selectedHartReg) := true.B
    }

    io.innerCtrl.ready := true.B

     // Construct a Vec from io.innerCtrl fields indicating whether each hart is being selected in this write
     // A hart may be selected by hartsel field or by hart array
    val hamaskWrSel = Wire(init = Vec.fill(nComponents){false.B})
    for (component <- 0 until nComponents ) {
      hamaskWrSel(component) := ((io.innerCtrl.bits.hartsel === component.U) ||
        (if (supportHartArray) io.innerCtrl.bits.hasel && io.innerCtrl.bits.hamask(component) else false.B))
    }

    //-------------------------------------
    // Halt-on-reset logic
    //  hrmask is set in dmOuter and passed in
    //  hartResets is the Vec of hart resets if that configuration is selected, otherwise it is 'reset'
    //  Debug interrupt is generated when a reset occurs whose corresponding hrmask bit is set
    //  Debug interrupt is maintained until the hart enters halted state
    //-------------------------------------
    val hrReset    = Wire(Vec.fill(nComponents) { false.B })
    val hrDebugInt = Reg(Vec(nComponents, Bool()))
    val hrmaskReg  = Reg(Vec(nComponents, Bool()))
    val hartResets = Wire(Vec(nComponents, Bool()))

    for (component <- 0 until nComponents) {
      hartResets(component) := (if (cfg.hasHartResets) SynchronizerShiftReg(io.hartReset.get(component), 3, Some(s"debug_hartReset_$component"))
        else reset)
    }

    when (~io.dmactive || ~dmAuthenticated) {
      hrDebugInt := hrReset
      hrmaskReg := hrReset
    }.otherwise {
      when (io.innerCtrl.fire()){
        hrmaskReg := io.innerCtrl.bits.hrmask
      }
      hrDebugInt := hrmaskReg &
        (hartResets |                      // set debugInt during reset
        (hrDebugInt & ~haltedBitRegs))     // maintain until core halts
    }

    //--------------------------------------------------------------
    // DMI Registers
    //--------------------------------------------------------------

    //----DMSTATUS

    val DMSTATUSRdData = Wire(init = (new DMSTATUSFields()).fromBits(0.U))
    DMSTATUSRdData.authenticated := dmAuthenticated
    DMSTATUSRdData.version       := 2.U    // Version 0.13
    io.auth.map(a => DMSTATUSRdData.authbusy := a.dmAuthBusy)

    val resumereq = io.innerCtrl.fire() && io.innerCtrl.bits.resumereq

    when (dmAuthenticated) {
      DMSTATUSRdData.hasresethaltreq := true.B

      DMSTATUSRdData.anynonexistent := (selectedHartReg >= nComponents.U)   // only hartsel can be nonexistent
         // all harts nonexistent if hartsel is out of range and there are no harts selected in the hart array
      DMSTATUSRdData.allnonexistent := (selectedHartReg >= nComponents.U) & (~hamaskFull.reduce(_ | _))

      when (~DMSTATUSRdData.allnonexistent) {  // if no existent harts selected, all other status is false
        DMSTATUSRdData.anyunavail   := (io.debugUnavail &  hamaskFull).reduce(_ | _)
        DMSTATUSRdData.anyhalted    := ((~io.debugUnavail &  haltedBitRegs) &  hamaskFull).reduce(_ | _)
        DMSTATUSRdData.anyrunning   := ((~io.debugUnavail & ~haltedBitRegs) &  hamaskFull).reduce(_ | _)
        DMSTATUSRdData.anyhavereset := (haveResetBitRegs &  hamaskFull).reduce(_ | _)
        DMSTATUSRdData.anyresumeack := (~resumeReqRegs &  hamaskFull).reduce(_ | _)
        when (~DMSTATUSRdData.anynonexistent) {  // if one hart is nonexistent, no 'all' status is set
          DMSTATUSRdData.allunavail   := (io.debugUnavail | ~hamaskFull).reduce(_ & _)
          DMSTATUSRdData.allhalted    := ((~io.debugUnavail &  haltedBitRegs) | ~hamaskFull).reduce(_ & _)
          DMSTATUSRdData.allrunning   := ((~io.debugUnavail & ~haltedBitRegs) | ~hamaskFull).reduce(_ & _)
          DMSTATUSRdData.allhavereset := (haveResetBitRegs | ~hamaskFull).reduce(_ & _)
          DMSTATUSRdData.allresumeack := (~resumeReqRegs | ~hamaskFull).reduce(_ & _)
        }
      }

      //TODO
      DMSTATUSRdData.confstrptrvalid := false.B
      DMSTATUSRdData.impebreak := (cfg.hasImplicitEbreak).B
    }

    for (component <- 0 until nComponents ) {
      when (~io.dmactive || ~dmAuthenticated) {
        haveResetBitRegs(component) := false.B
      }.elsewhen (hartResets(component)) {
        haveResetBitRegs(component) := true.B
      }.elsewhen (io.innerCtrl.fire() && io.innerCtrl.bits.ackhavereset && hamaskWrSel(component)) {
        haveResetBitRegs(component) := false.B
      }
    }

    //----DMCS2 (Halt Groups)

    val DMCS2RdData    = Wire(init = (new DMCS2Fields()).fromBits(0.U))
    val DMCS2WrData    = Wire(init = (new DMCS2Fields()).fromBits(0.U))
    val hgselectWrEn   = Wire(init = false.B)
    val hgwriteWrEn    = Wire(init = false.B)
    val haltgroupWrEn  = Wire(init = false.B)
    val exttriggerWrEn = Wire(init = false.B)
    val hgDebugInt     = Wire(Vec.fill(nComponents){false.B})

    if (nHaltGroups > 0) withReset(~io.dmactive) {
      val hgBits = log2Up(nHaltGroups)
       // hgParticipate: Each entry indicates which hg that entity belongs to (1 to nHartGroups). 0 means no hg assigned.
      val hgParticipateHart = RegInit(Vec(Seq.fill(nComponents)(0.U(hgBits.W))))
      val hgParticipateTrig = if (nExtTriggers > 0) RegInit(Vec(Seq.fill(nExtTriggers)(0.U(hgBits.W)))) else Nil

      for (component <- 0 until nComponents) {
        when (~io.dmactive || ~dmAuthenticated) {
          hgParticipateHart(component) := 0.U
        }.otherwise {
          when (haltgroupWrEn & DMCS2WrData.hgwrite & ~DMCS2WrData.hgselect &
              hamaskFull(component) & (DMCS2WrData.haltgroup <= nHaltGroups.U)) {
            hgParticipateHart(component) := DMCS2WrData.haltgroup
          }
        }
      }
      DMCS2RdData.haltgroup := hgParticipateHart(selectedHartReg)

      if (nExtTriggers > 0) {
        val hgSelect = RegInit(false.B)

        when (~io.dmactive || ~dmAuthenticated) {
          hgSelect := false.B
        }.otherwise {
           when (hgselectWrEn) {
             hgSelect := DMCS2WrData.hgselect
           }
        }

        for (trigger <- 0 until nExtTriggers) {
          when (~io.dmactive || ~dmAuthenticated) {
            hgParticipateTrig(trigger) := 0.U
          }.otherwise {
            when (haltgroupWrEn & DMCS2WrData.hgwrite & DMCS2WrData.hgselect &
                (DMCS2WrData.exttrigger === trigger.U) & (DMCS2WrData.haltgroup <= nHaltGroups.U)) {
              hgParticipateTrig(trigger) := DMCS2WrData.haltgroup
            }
          }
        }

        DMCS2RdData.hgselect := hgSelect
        when (hgSelect) {
          DMCS2RdData.haltgroup := hgParticipateTrig(0)
        }

        // If there is only 1 ext trigger, then the exttrigger field is fixed at 0
        // Otherwise, instantiate a register with only the number of bits required

        if (nExtTriggers > 1) {
          val trigBits = log2Up(nExtTriggers-1)
          val hgExtTrigger = RegInit(0.U(trigBits.W))
          when (~io.dmactive || ~dmAuthenticated) {
            hgExtTrigger := 0.U
          }.otherwise {
            when (exttriggerWrEn & (DMCS2WrData.exttrigger < nExtTriggers.U)) {
               hgExtTrigger := DMCS2WrData.exttrigger
            }
          }

          DMCS2RdData.exttrigger := hgExtTrigger
          when (hgSelect) {
            DMCS2RdData.haltgroup := hgParticipateTrig(hgExtTrigger)
          }
        }
      }

      // Halt group state machine
      //  IDLE:  Go to FIRED when any hart in this hg writes to HALTED while its HaltedBitRegs=0
      //                     or when any trigin assigned to this hg occurs
      //  FIRED: Back to IDLE when all harts in this hg have set their haltedBitRegs
      //                     and all trig out in this hg have been acknowledged

      val hgFired = Reg(Vec(nHaltGroups+1, Bool()))
      val hgHartFiring     = Wire(init = Vec.fill(nHaltGroups+1){false.B})     // which hg's are firing due to hart halting
      val hgTrigFiring     = Wire(init = Vec.fill(nHaltGroups+1){false.B})     // which hg's are firing due to trig in
      val hgHartsAllHalted = Wire(init = Vec.fill(nHaltGroups+1){false.B})     // in which hg's have all harts halted
      val hgTrigsAllAcked  = Wire(init = Vec.fill(nHaltGroups+1){ true.B})     // in which hg's have all trigouts been acked

      io.extTrigger.foreach {extTrigger =>
        val extTriggerInReq = Wire(Vec(nExtTriggers, Bool()))
        val extTriggerOutAck = Wire(Vec(nExtTriggers, Bool()))
        extTriggerInReq := extTrigger.in.req.asBools
        extTriggerOutAck := extTrigger.out.ack.asBools
        val trigInReq  = SynchronizerShiftReg(extTriggerInReq,  3, Some("dm_extTriggerInReqSync"))
        val trigOutAck = SynchronizerShiftReg(extTriggerOutAck, 3, Some("dm_extTriggerOutAckSync"))
        for (hg <- 1 to nHaltGroups) {
          hgTrigFiring(hg) := (trigInReq & ~RegNext(trigInReq) & hgParticipateTrig.map(_ === hg.U)).reduce(_ | _)
          hgTrigsAllAcked(hg) := (trigOutAck | hgParticipateTrig.map(_ =/= hg.U)).reduce(_ & _)
        }
        extTrigger.in.ack := trigInReq.asUInt()
      }

      for (hg <- 1 to nHaltGroups) {
        hgHartFiring(hg) := hartHaltedWrEn & ~haltedBitRegs(hartHaltedId) & (hgParticipateHart(hartSelFuncs.hartIdToHartSel(hartHaltedId)) === hg.U)
        hgHartsAllHalted(hg) := (haltedBitRegs | hgParticipateHart.map(_ =/= hg.U)).reduce(_ & _)

        when (~io.dmactive || ~dmAuthenticated) {
          hgFired(hg) := false.B
        }.elsewhen (~hgFired(hg) & (hgHartFiring(hg) | hgTrigFiring(hg))) {
          hgFired(hg) := true.B
        }.elsewhen ( hgFired(hg) & hgHartsAllHalted(hg) & hgTrigsAllAcked(hg)) {
          hgFired(hg) := false.B
        }
      }

      // For each hg that has fired, assert debug interrupt to each hart in that hg
      for (component <- 0 until nComponents) {
        hgDebugInt(component) := hgFired(hgParticipateHart(component))
      }

      // For each hg that has fired, assert trigger out for all external triggers in that hg
      io.extTrigger.foreach {extTrigger =>
        val extTriggerOutReq = RegInit(Vec.fill(cfg.nExtTriggers){false.B})
        for (trig <- 0 until nExtTriggers) {
          extTriggerOutReq(trig) := hgFired(hgParticipateTrig(trig))
        }
        extTrigger.out.req := extTriggerOutReq.asUInt()
      }
    }
    io.hgDebugInt := hgDebugInt | hrDebugInt


    //----HARTINFO

    val HARTINFORdData = Wire (init = (new HARTINFOFields()).fromBits(0.U))
    when (dmAuthenticated) {
      HARTINFORdData.dataaccess  := true.B
      HARTINFORdData.datasize    := cfg.nAbstractDataWords.U
      HARTINFORdData.dataaddr    := DsbRegAddrs.DATA.U
      HARTINFORdData.nscratch    := cfg.nScratch.U
    }

    //----HALTSUM*
    val numHaltedStatus = ((nComponents - 1) / 32) + 1
    val haltedStatus   = Wire(Vec(numHaltedStatus, Bits(width = 32)))

    for (ii <- 0 until numHaltedStatus) {
      when (dmAuthenticated) {
        haltedStatus(ii) := Cat(haltedBitRegs.slice(ii * 32, (ii + 1) * 32).reverse)
      }.otherwise {
        haltedStatus(ii) := 0.U
      }
    }

    val haltedSummary = Cat(haltedStatus.map(_.orR).reverse)
    val HALTSUM1RdData = (new HALTSUM1Fields()).fromBits(haltedSummary)

    val selectedHaltedStatus = Mux((selectedHartReg >> 5) > numHaltedStatus.U, 0.U, haltedStatus(selectedHartReg >> 5))
    val HALTSUM0RdData = (new HALTSUM0Fields()).fromBits(selectedHaltedStatus)

    // Since we only support 1024 harts, we don't implement HALTSUM2 or HALTSUM3

    //----ABSTRACTCS

    val ABSTRACTCSReset = Wire(init = (new ABSTRACTCSFields()).fromBits(0.U))
    ABSTRACTCSReset.datacount   := cfg.nAbstractDataWords.U
    ABSTRACTCSReset.progbufsize := cfg.nProgramBufferWords.U

    val ABSTRACTCSReg       = Reg(new ABSTRACTCSFields())
    val ABSTRACTCSWrData    = Wire(init = (new ABSTRACTCSFields()).fromBits(0.U))
    val ABSTRACTCSRdData    = Wire(init = ABSTRACTCSReg)

    val ABSTRACTCSRdEn = Wire(init = false.B)
    val ABSTRACTCSWrEnMaybe = Wire(init = false.B)

    val ABSTRACTCSWrEnLegal = Wire(init = false.B)
    val ABSTRACTCSWrEn      = ABSTRACTCSWrEnMaybe && ABSTRACTCSWrEnLegal

    val errorBusy        = Wire(init = false.B)
    val errorException   = Wire(init = false.B)
    val errorUnsupported = Wire(init = false.B)
    val errorHaltResume  = Wire(init = false.B)

    when (~io.dmactive || ~dmAuthenticated) {
      ABSTRACTCSReg := ABSTRACTCSReset
    }.otherwise {
      when (errorBusy){
        ABSTRACTCSReg.cmderr := DebugAbstractCommandError.ErrBusy.id.U
      }.elsewhen (errorException) {
        ABSTRACTCSReg.cmderr := DebugAbstractCommandError.ErrException.id.U
      }.elsewhen (errorUnsupported) {
        ABSTRACTCSReg.cmderr := DebugAbstractCommandError.ErrNotSupported.id.U
      }.elsewhen (errorHaltResume) {
        ABSTRACTCSReg.cmderr := DebugAbstractCommandError.ErrHaltResume.id.U
      }.otherwise {
        when (ABSTRACTCSWrEn){
          ABSTRACTCSReg.cmderr := ABSTRACTCSReg.cmderr & ~(ABSTRACTCSWrData.cmderr);
        }
      }
    }

    // For busy, see below state machine.
    val abstractCommandBusy = Wire(init = true.B)
    ABSTRACTCSRdData.busy := abstractCommandBusy
    when (~dmAuthenticated) {   // read value must be 0 when not authenticated
      ABSTRACTCSRdData.datacount := 0.U
      ABSTRACTCSRdData.progbufsize := 0.U
    }

    //---- ABSTRACTAUTO

    val ABSTRACTAUTOReset     = Wire(init = (new ABSTRACTAUTOFields()).fromBits(0.U))
    val ABSTRACTAUTOReg       = Reg(new ABSTRACTAUTOFields())
    val ABSTRACTAUTOWrData    = Wire(init = (new ABSTRACTAUTOFields()).fromBits(0.U))
    val ABSTRACTAUTORdData    = Wire(init = ABSTRACTAUTOReg)

    val ABSTRACTAUTORdEn = Wire(init = false.B)
    val autoexecdataWrEnMaybe = Wire(init = false.B)
    val autoexecprogbufWrEnMaybe = Wire(init = false.B)

    val ABSTRACTAUTOWrEnLegal = Wire(init = false.B)

    when (~io.dmactive || ~dmAuthenticated) {
      ABSTRACTAUTOReg := ABSTRACTAUTOReset
    }.otherwise {
      when (autoexecprogbufWrEnMaybe && ABSTRACTAUTOWrEnLegal) {
        ABSTRACTAUTOReg.autoexecprogbuf := ABSTRACTAUTOWrData.autoexecprogbuf & ( (1 << cfg.nProgramBufferWords) - 1).U
      }
      when (autoexecdataWrEnMaybe && ABSTRACTAUTOWrEnLegal) {
        ABSTRACTAUTOReg.autoexecdata := ABSTRACTAUTOWrData.autoexecdata & ( (1 << cfg.nAbstractDataWords) - 1).U
      }
    }

    val dmiAbstractDataAccessVec  = Wire(init = Vec.fill(cfg.nAbstractDataWords * 4){false.B})
    dmiAbstractDataAccessVec := (dmiAbstractDataWrEnMaybe zip dmiAbstractDataRdEn).map{ case (r,w) => r | w}

    val dmiProgramBufferAccessVec  = Wire(init = Vec.fill(cfg.nProgramBufferWords * 4){false.B})
    dmiProgramBufferAccessVec := (dmiProgramBufferWrEnMaybe zip dmiProgramBufferRdEn).map{ case (r,w) => r | w}

    val dmiAbstractDataAccess  = dmiAbstractDataAccessVec.reduce(_ || _ )
    val dmiProgramBufferAccess = dmiProgramBufferAccessVec.reduce(_ || _)

    // This will take the shorter of the lists, which is what we want.
    val autoexecData  = Wire(init = Vec.fill(cfg.nAbstractDataWords){false.B})
    val autoexecProg  = Wire(init = Vec.fill(cfg.nProgramBufferWords){false.B})
      (autoexecData zip ABSTRACTAUTOReg.autoexecdata.asBools).zipWithIndex.foreach {case (t, i) => t._1 := dmiAbstractDataAccessVec(i * 4) && t._2 }
      (autoexecProg zip ABSTRACTAUTOReg.autoexecprogbuf.asBools).zipWithIndex.foreach {case (t, i) => t._1 := dmiProgramBufferAccessVec(i * 4) && t._2}

    val autoexec = autoexecData.reduce(_ || _) || autoexecProg.reduce(_ || _)

    //---- COMMAND

    val COMMANDReset = Wire(init = (new COMMANDFields()).fromBits(0.U))
    val COMMANDReg = Reg(new COMMANDFields())

    val COMMANDWrDataVal    = Wire(init = 0.U(32.W))
    val COMMANDWrData       = Wire(init = (new COMMANDFields()).fromBits(COMMANDWrDataVal))
    val COMMANDWrEnMaybe    = Wire(init = false.B)
    val COMMANDWrEnLegal    = Wire(init = false.B)
    val COMMANDRdEn  = Wire(init = false.B)

    val COMMANDWrEn = COMMANDWrEnMaybe && COMMANDWrEnLegal
    val COMMANDRdData = COMMANDReg

    when (~io.dmactive || ~dmAuthenticated) {
      COMMANDReg := COMMANDReset
    }.otherwise {
      when (COMMANDWrEn) {
        COMMANDReg := COMMANDWrData
      }
    }

    // --- Abstract Data

    // These are byte addressible, s.t. the Processor can use
    // byte-addressible instructions to store to them.
    val abstractDataMem       = Reg(Vec(cfg.nAbstractDataWords*4, UInt(8.W)))
    val abstractDataNxt       = Wire(init = abstractDataMem)

    // --- Program Buffer
    val programBufferMem    = Reg(Vec(cfg.nProgramBufferWords*4, UInt(8.W)))
    val programBufferNxt    = Wire(init = programBufferMem)

    //--------------------------------------------------------------
    // These bits are implementation-specific bits set
    // by harts executing code.
    //--------------------------------------------------------------

    for (component <- 0 until nComponents) {
      when (~io.dmactive || ~dmAuthenticated) {
        haltedBitRegs(component) := false.B
        resumeReqRegs(component) := false.B
      }.otherwise {
        // Hart Halt Notification Logic
        when (hartResets(component)) {
          haltedBitRegs(component) := false.B
          resumeReqRegs(component) := false.B
        }.elsewhen (hartHaltedWrEn) {
          when (hartSelFuncs.hartIdToHartSel(hartHaltedId) === component.U) {
            haltedBitRegs(component) := true.B
          }
        }.elsewhen (hartResumingWrEn) {
          when (hartSelFuncs.hartIdToHartSel(hartResumingId) === component.U) {
            haltedBitRegs(component) := false.B
          }
        }

        // Hart Resume Req Logic
        // If you request a hart to resume at the same moment
        // it actually does resume, then the request wins.
        // So don't try to write resumereq more than once
        when (hartResumingWrEn) {
          when (hartSelFuncs.hartIdToHartSel(hartResumingId) === component.U) {
            resumeReqRegs(component) := false.B
          }
        }
        when (resumereq && hamaskWrSel(component)) {
          resumeReqRegs(component) := true.B
        }
      }
    }

    //---- AUTHDATA
    val authRdEnMaybe = Wire(Bool())
    val authWrEnMaybe = Wire(Bool())
    io.auth.map { a =>
      a.dmactive    := io.dmactive
      a.dmAuthRead  := authRdEnMaybe & ~a.dmAuthBusy
      a.dmAuthWrite := authWrEnMaybe & ~a.dmAuthBusy
    }

    val dmstatusRegFields = RegFieldGroup("dmi_dmstatus", Some("debug module status register"), Seq(
      RegField.r(4, DMSTATUSRdData.version,         RegFieldDesc("version",         "version",         reset=Some(2))),
      RegField.r(1, DMSTATUSRdData.confstrptrvalid, RegFieldDesc("confstrptrvalid", "confstrptrvalid", reset=Some(0))),
      RegField.r(1, DMSTATUSRdData.hasresethaltreq, RegFieldDesc("hasresethaltreq", "hasresethaltreq", reset=Some(1))),
      RegField.r(1, DMSTATUSRdData.authbusy,        RegFieldDesc("authbusy",        "authbusy",        reset=Some(0))),
      RegField.r(1, DMSTATUSRdData.authenticated,   RegFieldDesc("authenticated",   "authenticated",   reset=Some(1))),
      RegField.r(1, DMSTATUSRdData.anyhalted,       RegFieldDesc("anyhalted",       "anyhalted",       reset=Some(0))),
      RegField.r(1, DMSTATUSRdData.allhalted,       RegFieldDesc("allhalted",       "allhalted",       reset=Some(0))),
      RegField.r(1, DMSTATUSRdData.anyrunning,      RegFieldDesc("anyrunning",      "anyrunning",      reset=Some(1))),
      RegField.r(1, DMSTATUSRdData.allrunning,      RegFieldDesc("allrunning",      "allrunning",      reset=Some(1))),
      RegField.r(1, DMSTATUSRdData.anyunavail,      RegFieldDesc("anyunavail",      "anyunavail",      reset=Some(0))),
      RegField.r(1, DMSTATUSRdData.allunavail,      RegFieldDesc("allunavail",      "allunavail",      reset=Some(0))),
      RegField.r(1, DMSTATUSRdData.anynonexistent,  RegFieldDesc("anynonexistent",  "anynonexistent",  reset=Some(0))),
      RegField.r(1, DMSTATUSRdData.allnonexistent,  RegFieldDesc("allnonexistent",  "allnonexistent",  reset=Some(0))),
      RegField.r(1, DMSTATUSRdData.anyresumeack,    RegFieldDesc("anyresumeack",    "anyresumeack",    reset=Some(1))),
      RegField.r(1, DMSTATUSRdData.allresumeack,    RegFieldDesc("allresumeack",    "allresumeack",    reset=Some(1))),
      RegField.r(1, DMSTATUSRdData.anyhavereset,    RegFieldDesc("anyhavereset",    "anyhavereset",    reset=Some(0))),
      RegField.r(1, DMSTATUSRdData.allhavereset,    RegFieldDesc("allhavereset",    "allhavereset",    reset=Some(0))),
      RegField(2),
      RegField.r(1, DMSTATUSRdData.impebreak,       RegFieldDesc("impebreak",       "impebreak",       reset=Some(if (cfg.hasImplicitEbreak) 1 else 0)))
    ))

    val dmcs2RegFields = RegFieldGroup("dmi_dmcs2", Some("debug module control/status register 2"), Seq(
      WNotifyVal(1, DMCS2RdData.hgselect,  DMCS2WrData.hgselect, hgselectWrEn,
        RegFieldDesc("hgselect", "select halt groups or external triggers", reset=Some(0), volatile=true)),
      WNotifyVal(1, 0.U,                   DMCS2WrData.hgwrite,  hgwriteWrEn,
        RegFieldDesc("hgwrite",  "write 1 to change halt groups", reset=None, access=RegFieldAccessType.W)),
      WNotifyVal(5, DMCS2RdData.haltgroup, DMCS2WrData.haltgroup, haltgroupWrEn,
        RegFieldDesc("haltgroup", "halt group", reset=Some(0), volatile=true)),
      if (nExtTriggers > 1)
        WNotifyVal(4, DMCS2RdData.exttrigger, DMCS2WrData.exttrigger, exttriggerWrEn,
        RegFieldDesc("exttrigger", "external trigger select", reset=Some(0), volatile=true))
      else RegField(4)
    ))

    val hartinfoRegFields = RegFieldGroup("dmi_hartinfo", Some("hart information"), Seq(
      RegField.r(12, HARTINFORdData.dataaddr,   RegFieldDesc("dataaddr",   "data address",                reset=Some(DsbRegAddrs.DATA))),
      RegField.r(4,  HARTINFORdData.datasize,   RegFieldDesc("datasize",   "number of DATA registers",    reset=Some(cfg.nAbstractDataWords))),
      RegField.r(1,  HARTINFORdData.dataaccess, RegFieldDesc("dataaccess", "data access type",            reset=Some(1))),
      RegField(3),
      RegField.r(4,  HARTINFORdData.nscratch,   RegFieldDesc("nscratch",   "number of scratch registers", reset=Some(cfg.nScratch)))
    ))

    val abstractcsRegFields = RegFieldGroup("dmi_abstractcs", Some("abstract command control/status"), Seq(
      RegField.r(4, ABSTRACTCSRdData.datacount, RegFieldDesc("datacount", "number of DATA regsiters", reset=Some(cfg.nAbstractDataWords))),
      RegField(4),
      WNotifyVal(3, ABSTRACTCSRdData.cmderr, ABSTRACTCSWrData.cmderr, ABSTRACTCSWrEnMaybe,
        RegFieldDesc("cmderr", "command error", reset=Some(0), wrType=Some(RegFieldWrType.ONE_TO_CLEAR))),
      RegField(1),
      RegField.r(1, ABSTRACTCSRdData.busy, RegFieldDesc("busy", "busy", reset=Some(0))),
      RegField(11),
      RegField.r(5, ABSTRACTCSRdData.progbufsize, RegFieldDesc("progbufsize", "number of PROGBUF regsiters", reset=Some(cfg.nProgramBufferWords)))
    ))

    val (sbcsFields, sbAddrFields, sbDataFields):
    (Seq[RegField], Seq[Seq[RegField]], Seq[Seq[RegField]]) = sb2tlOpt.map{ sb2tl =>
      SystemBusAccessModule(sb2tl, io.dmactive, dmAuthenticated)(p)
    }.getOrElse((Seq.empty[RegField], Seq.fill[Seq[RegField]](4)(Seq.empty[RegField]), Seq.fill[Seq[RegField]](4)(Seq.empty[RegField])))

    //--------------------------------------------------------------
    // Program Buffer Access (DMI ... System Bus can override)
    //--------------------------------------------------------------
    val omRegMap = dmiNode.regmap(
      (DMI_DMSTATUS    << 2) -> dmstatusRegFields,
      //TODO (DMI_CFGSTRADDR0 << 2) -> cfgStrAddrFields,
      (DMI_DMCS2       << 2) -> (if (nHaltGroups > 0) dmcs2RegFields else Nil),
      (DMI_HARTINFO    << 2) -> hartinfoRegFields,
      (DMI_HALTSUM0    << 2) -> RegFieldGroup("dmi_haltsum0", Some("Halt Summary 0"),
         Seq(RegField.r(32, HALTSUM0RdData.asUInt(), RegFieldDesc("dmi_haltsum0", "halt summary 0")))),
      (DMI_HALTSUM1    << 2) -> RegFieldGroup("dmi_haltsum1", Some("Halt Summary 1"),
         Seq(RegField.r(32, HALTSUM1RdData.asUInt(), RegFieldDesc("dmi_haltsum1", "halt summary 1")))),
      (DMI_ABSTRACTCS  << 2) -> abstractcsRegFields,
      (DMI_ABSTRACTAUTO<< 2) -> RegFieldGroup("dmi_abstractauto", Some("abstract command autoexec"), Seq(
        WNotifyVal(cfg.nAbstractDataWords, ABSTRACTAUTORdData.autoexecdata, ABSTRACTAUTOWrData.autoexecdata, autoexecdataWrEnMaybe,
          RegFieldDesc("autoexecdata", "abstract command data autoexec", reset=Some(0))),
        RegField(16-cfg.nAbstractDataWords),
        WNotifyVal(cfg.nProgramBufferWords, ABSTRACTAUTORdData.autoexecprogbuf, ABSTRACTAUTOWrData.autoexecprogbuf, autoexecprogbufWrEnMaybe,
          RegFieldDesc("autoexecprogbuf", "abstract command progbuf autoexec", reset=Some(0))))),
      (DMI_COMMAND     << 2) -> RegFieldGroup("dmi_command", Some("Abstract Command Register"),
        Seq(RWNotify(32, COMMANDRdData.asUInt(), COMMANDWrDataVal, COMMANDRdEn, COMMANDWrEnMaybe,
        Some(RegFieldDesc("dmi_command", "abstract command register", reset=Some(0), volatile=true))))),
      (DMI_DATA0       << 2) -> RegFieldGroup("dmi_data", None, abstractDataMem.zipWithIndex.map{case (x, i) => RWNotify(8,
        Mux(dmAuthenticated, x, 0.U), abstractDataNxt(i),
        dmiAbstractDataRdEn(i),
        dmiAbstractDataWrEnMaybe(i),
        Some(RegFieldDesc(s"dmi_data_$i", s"abstract command data register $i", reset = Some(0), volatile=true)))}),
      (DMI_PROGBUF0    << 2) -> RegFieldGroup("dmi_progbuf", None, programBufferMem.zipWithIndex.map{case (x, i) => RWNotify(8,
        Mux(dmAuthenticated, x, 0.U), programBufferNxt(i),
        dmiProgramBufferRdEn(i),
        dmiProgramBufferWrEnMaybe(i),
        Some(RegFieldDesc(s"dmi_progbuf_$i", s"abstract command progbuf register $i", reset = Some(0))))}),
      (DMI_AUTHDATA   << 2) -> (if (cfg.hasAuthentication) RegFieldGroup("dmi_authdata", Some("authentication data exchange register"),
        Seq(RWNotify(32, io.auth.get.dmAuthRdata, io.auth.get.dmAuthWdata, authRdEnMaybe, authWrEnMaybe,
        Some(RegFieldDesc("authdata", "authentication data exchange", volatile=true))))) else Nil),
      (DMI_SBCS       << 2) -> sbcsFields,
      (DMI_SBDATA0    << 2) -> sbDataFields(0),
      (DMI_SBDATA1    << 2) -> sbDataFields(1),
      (DMI_SBDATA2    << 2) -> sbDataFields(2),
      (DMI_SBDATA3    << 2) -> sbDataFields(3),
      (DMI_SBADDRESS0 << 2) -> sbAddrFields(0),
      (DMI_SBADDRESS1 << 2) -> sbAddrFields(1),
      (DMI_SBADDRESS2 << 2) -> sbAddrFields(2),
      (DMI_SBADDRESS3 << 2) -> sbAddrFields(3) 
    )

    // Abstract data mem is written by both the tile link interface and DMI...
    abstractDataMem.zipWithIndex.foreach { case (x, i) =>
      when (dmAuthenticated && dmiAbstractDataWrEnMaybe(i) && dmiAbstractDataAccessLegal) {
        x := abstractDataNxt(i)
      }
    }
    // ... and also by custom register read (if implemented)
    val (customs, customParams) = customNode.in.unzip
    val needCustom = (customs.size > 0) && (customParams.head.addrs.size > 0)
    def getNeedCustom = () => needCustom

    if (needCustom) {
      val (custom, customP) = customNode.in.head
      require(customP.width % 8 == 0, s"Debug Custom width must be divisible by 8, not ${customP.width}")
      val custom_data = custom.data.asBools
      val custom_bytes =  Seq.tabulate(customP.width/8){i => custom_data.slice(i*8, (i+1)*8).asUInt}
      when (custom.ready && custom.valid) {
        (abstractDataMem zip custom_bytes).zipWithIndex.foreach {case ((a, b), i) =>
          a := b
        }
      }
    }

    programBufferMem.zipWithIndex.foreach { case (x, i) =>
      when (dmAuthenticated && dmiProgramBufferWrEnMaybe(i) && dmiProgramBufferAccessLegal) {
        x := programBufferNxt(i)
      }
    }

    //--------------------------------------------------------------
    // "Variable" ROM Generation
    //--------------------------------------------------------------

    val goReg        = Reg(Bool())
    val goAbstract   = Wire(init = false.B)
    val goCustom     = Wire(init = false.B)
    val jalAbstract  = Wire(init = (new GeneratedUJ()).fromBits(Instructions.JAL.value.U))
    jalAbstract.setImm(ABSTRACT(cfg) - WHERETO)

    when (~io.dmactive){
      goReg := false.B
    }.otherwise {
      when (goAbstract) {
        goReg := true.B
      }.elsewhen (hartGoingWrEn){
        assert(hartGoingId === 0.U, "Unexpected 'GOING' hart.")//Chisel3 #540 %x, expected %x", hartGoingId, 0.U)
        goReg := false.B
      }
    }

    class flagBundle extends Bundle {
      val reserved = UInt(6.W)
      val resume = Bool()
      val go = Bool()
    }

    val flags = Wire(init = Vec.fill(1 << selectedHartReg.getWidth){new flagBundle().fromBits(0.U)})
    assert ((hartSelFuncs.hartSelToHartId(selectedHartReg) < flags.size.U),
      s"HartSel to HartId Mapping is illegal for this Debug Implementation, because HartID must be < ${flags.size} for it to work.")
    flags(hartSelFuncs.hartSelToHartId(selectedHartReg)).go := goReg

    for (component <- 0 until nComponents) {
      val componentSel = Wire(init = component.U)
      flags(hartSelFuncs.hartSelToHartId(componentSel)).resume := resumeReqRegs(component)
    }

    //----------------------------
    // Abstract Command Decoding & Generation
    //----------------------------

    val accessRegisterCommandWr  = Wire(init = (new ACCESS_REGISTERFields()).fromBits(COMMANDWrData.asUInt()))
    val accessRegisterCommandReg = Wire(init = (new ACCESS_REGISTERFields()).fromBits(COMMANDReg.asUInt()))

    // TODO: Quick Access

    class GeneratedI extends Bundle {
      val imm    = UInt(12.W)
      val rs1    = UInt(5.W)
      val funct3 = UInt(3.W)
      val rd     = UInt(5.W)
      val opcode = UInt(7.W)
    }

    class GeneratedS extends Bundle {
      val immhi  = UInt(7.W)
      val rs2    = UInt(5.W)
      val rs1    = UInt(5.W)
      val funct3 = UInt(3.W)
      val immlo  = UInt(5.W)
      val opcode = UInt(7.W)
    }

    class GeneratedUJ extends Bundle {
      val imm3    = UInt(1.W)
      val imm0    = UInt(10.W)
      val imm1    = UInt(1.W)
      val imm2    = UInt(8.W)
      val rd      = UInt(5.W)
      val opcode  = UInt(7.W)

      def setImm(imm: Int) : Unit = {
        // TODO: Check bounds of imm.

        require(imm % 2 == 0, "Immediate must be even for UJ encoding.")
        val immWire = Wire(init = imm.S(21.W))
        val immBits = Wire(init = Vec(immWire.asBools))

        imm0 := immBits.slice(1,  1  + 10).asUInt()
        imm1 := immBits.slice(11, 11 + 11).asUInt()
        imm2 := immBits.slice(12, 12 + 8).asUInt()
        imm3 := immBits.slice(20, 20 + 1).asUInt()
      }
    }

    val abstractGeneratedMem = Reg(Vec(2, (UInt(32.W))))
    val abstractGeneratedI = Wire(new GeneratedI())
    val abstractGeneratedS = Wire(new GeneratedS())
    val nop = Wire(new GeneratedI())

    abstractGeneratedI.opcode := ((new GeneratedI()).fromBits(Instructions.LW.value.U)).opcode
    abstractGeneratedI.rd     := (accessRegisterCommandReg.regno & 0x1F.U)
    abstractGeneratedI.funct3 := accessRegisterCommandReg.size
    abstractGeneratedI.rs1    := 0.U
    abstractGeneratedI.imm    := DATA.U

    abstractGeneratedS.opcode := ((new GeneratedS()).fromBits(Instructions.SW.value.U)).opcode
    abstractGeneratedS.immlo  := (DATA & 0x1F).U
    abstractGeneratedS.funct3 := accessRegisterCommandReg.size
    abstractGeneratedS.rs1    := 0.U
    abstractGeneratedS.rs2    := (accessRegisterCommandReg.regno & 0x1F.U)
    abstractGeneratedS.immhi  := (DATA >> 5).U

    nop := ((new GeneratedI()).fromBits(Instructions.ADDI.value.U))
    nop.rd   := 0.U
    nop.rs1  := 0.U
    nop.imm  := 0.U

    when (goAbstract) {
      abstractGeneratedMem(0) := Mux(accessRegisterCommandReg.transfer,
        Mux(accessRegisterCommandReg.write,
          // To write a register, we need to do LW.
          abstractGeneratedI.asUInt(),
          // To read a register, we need to do SW.
          abstractGeneratedS.asUInt()),
        nop.asUInt()
      )
      abstractGeneratedMem(1) := Mux(accessRegisterCommandReg.postexec,
        nop.asUInt(),
        Instructions.EBREAK.value.U)
    }

    //--------------------------------------------------------------
    // Drive Custom Access
    //--------------------------------------------------------------
    if (needCustom) {
      val (custom, customP) = customNode.in.head
      custom.addr  := accessRegisterCommandReg.regno
      custom.valid := goCustom
    }
    //--------------------------------------------------------------
    // Hart Bus Access
    //--------------------------------------------------------------

    tlNode.regmap(
      // This memory is writable.
      HALTED      -> Seq(WNotifyWire(sbIdWidth, hartHaltedId, hartHaltedWrEn,
        "debug_hart_halted", "Debug ROM Causes hart to write its hartID here when it is in Debug Mode.")),
      GOING       -> Seq(WNotifyWire(sbIdWidth, hartGoingId,  hartGoingWrEn,
        "debug_hart_going", "Debug ROM causes hart to write 0 here when it begins executing Debug Mode instructions.")),
      RESUMING    -> Seq(WNotifyWire(sbIdWidth, hartResumingId,  hartResumingWrEn,
        "debug_hart_resuming", "Debug ROM causes hart to write its hartID here when it leaves Debug Mode.")),
      EXCEPTION   -> Seq(WNotifyWire(sbIdWidth, hartExceptionId,  hartExceptionWrEn,
        "debug_hart_exception", "Debug ROM causes hart to write 0 here if it gets an exception in Debug Mode.")),
      DATA        -> RegFieldGroup("debug_data", Some("Data used to communicate with Debug Module"),
        abstractDataMem.zipWithIndex.map {case (x, i) => RegField(8, x, RegFieldDesc(s"debug_data_$i", ""))}),
      PROGBUF(cfg)-> RegFieldGroup("debug_progbuf", Some("Program buffer used to communicate with Debug Module"),
        programBufferMem.zipWithIndex.map {case (x, i) => RegField(8, x, RegFieldDesc(s"debug_progbuf_$i", ""))}),

      // These sections are read-only.
      IMPEBREAK(cfg)-> {if (cfg.hasImplicitEbreak) Seq(RegField.r(32,  Instructions.EBREAK.value.U,
        RegFieldDesc("debug_impebreak", "Debug Implicit EBREAK", reset=Some(Instructions.EBREAK.value)))) else Nil},
      WHERETO       -> Seq(RegField.r(32, jalAbstract.asUInt, RegFieldDesc("debug_whereto", "Instruction filled in by Debug Module to control hart in Debug Mode", volatile = true))),
      ABSTRACT(cfg) -> RegFieldGroup("debug_abstract", Some("Instructions generated by Debug Module"),
        abstractGeneratedMem.zipWithIndex.map{ case (x,i) => RegField.r(32, x, RegFieldDesc(s"debug_abstract_$i", "", volatile=true))}),
      FLAGS         -> RegFieldGroup("debug_flags", Some("Memory region used to control hart going/resuming in Debug Mode"),
        flags.zipWithIndex.map{case(x, i) => RegField.r(8, x.asUInt(), RegFieldDesc(s"debug_flags_$i", "", volatile=true))}),
      ROMBASE       -> RegFieldGroup("debug_rom", Some("Debug ROM"),
        DebugRomContents().zipWithIndex.map{case (x, i) => RegField.r(8, (x & 0xFF).U(8.W), RegFieldDesc(s"debug_rom_$i", "", reset=Some(x)))})
    )

    // Override System Bus accesses with dmactive reset.
    when (~io.dmactive){
      abstractDataMem.foreach  {x => x := 0.U}
      programBufferMem.foreach {x => x := 0.U}
    }

    //--------------------------------------------------------------
    // Abstract Command State Machine
    //--------------------------------------------------------------

    object CtrlState extends scala.Enumeration {
      type CtrlState = Value
      val Waiting, CheckGenerate, Exec, Custom = Value

      def apply( t : Value) : UInt = {
        t.id.U(log2Up(values.size).W)
      }
    }
    import CtrlState._

    // This is not an initialization!
    val ctrlStateReg = Reg(CtrlState(Waiting))

    val hartHalted   = haltedBitRegs(selectedHartReg)
    val ctrlStateNxt = Wire(init = ctrlStateReg)

    //------------------------
    // DMI Register Control and Status

    abstractCommandBusy := (ctrlStateReg =/= CtrlState(Waiting))

    ABSTRACTCSWrEnLegal   := (ctrlStateReg === CtrlState(Waiting))
    COMMANDWrEnLegal      := (ctrlStateReg === CtrlState(Waiting))
    ABSTRACTAUTOWrEnLegal := (ctrlStateReg === CtrlState(Waiting))
    dmiAbstractDataAccessLegal  := (ctrlStateReg === CtrlState(Waiting))
    dmiProgramBufferAccessLegal := (ctrlStateReg === CtrlState(Waiting))

    errorBusy := (ABSTRACTCSWrEnMaybe    && ~ABSTRACTCSWrEnLegal)        ||
                 (autoexecdataWrEnMaybe  && ~ABSTRACTAUTOWrEnLegal)      ||
                 (autoexecprogbufWrEnMaybe && ~ABSTRACTAUTOWrEnLegal)    ||
                 (COMMANDWrEnMaybe       && ~COMMANDWrEnLegal)           ||
                 (dmiAbstractDataAccess  && ~dmiAbstractDataAccessLegal) ||
                 (dmiProgramBufferAccess && ~dmiProgramBufferAccessLegal)

    // TODO: Maybe Quick Access
    val commandWrIsAccessRegister = (COMMANDWrData.cmdtype === DebugAbstractCommandType.AccessRegister.id.U)
    val commandRegIsAccessRegister = (COMMANDReg.cmdtype === DebugAbstractCommandType.AccessRegister.id.U)

    val commandWrIsUnsupported = COMMANDWrEn && !commandWrIsAccessRegister;

    val commandRegIsUnsupported = Wire(init = true.B)
    val commandRegBadHaltResume = Wire(init = false.B)

    // We only support abstract commands for GPRs and any custom registers, if specified.
    val accessRegIsGPR = (accessRegisterCommandReg.regno >= 0x1000.U && accessRegisterCommandReg.regno <= 0x101F.U)
    val accessRegIsCustom = if (needCustom) {
      val (custom, customP) = customNode.in.head
      customP.addrs.foldLeft(false.B){
        (result, current) => result || (current.U === accessRegisterCommandReg.regno)}
    } else false.B

    when (commandRegIsAccessRegister) {
      when (accessRegIsCustom && accessRegisterCommandReg.transfer && accessRegisterCommandReg.write === false.B) {
        commandRegIsUnsupported := false.B
      }.elsewhen (!accessRegisterCommandReg.transfer || accessRegIsGPR) {
        commandRegIsUnsupported := false.B
        commandRegBadHaltResume := ~hartHalted
      }
    }

    val wrAccessRegisterCommand  = COMMANDWrEn && commandWrIsAccessRegister  && (ABSTRACTCSReg.cmderr === 0.U)
    val regAccessRegisterCommand = autoexec    && commandRegIsAccessRegister && (ABSTRACTCSReg.cmderr === 0.U)

    //------------------------
    // Variable ROM STATE MACHINE
    // -----------------------

    when (ctrlStateReg === CtrlState(Waiting)){
      when (wrAccessRegisterCommand || regAccessRegisterCommand) {
        ctrlStateNxt := CtrlState(CheckGenerate)
      }.elsewhen (commandWrIsUnsupported) { // These checks are really on the command type.
        errorUnsupported := true.B
      }.elsewhen (autoexec && commandRegIsUnsupported) {
        errorUnsupported := true.B
      }
    }.elsewhen (ctrlStateReg === CtrlState(CheckGenerate)){

      // We use this state to ensure that the COMMAND has been
      // registered by the time that we need to use it, to avoid
      // generating it directly from the COMMANDWrData.
      // This 'commandRegIsUnsupported' is really just checking the
      // AccessRegisterCommand parameters (regno)
      when (commandRegIsUnsupported) {
        errorUnsupported := true.B
        ctrlStateNxt := CtrlState(Waiting)
      }.elsewhen (commandRegBadHaltResume){
        errorHaltResume := true.B
        ctrlStateNxt := CtrlState(Waiting)
      }.otherwise {
        when(accessRegIsCustom) {
          ctrlStateNxt := CtrlState(Custom)
        }.otherwise {
          ctrlStateNxt := CtrlState(Exec)
          goAbstract := true.B
        }
      }
    }.elsewhen (ctrlStateReg === CtrlState(Exec)) {

      // We can't just look at 'hartHalted' here, because
      // hartHaltedWrEn is overloaded to mean 'got an ebreak'
      // which may have happened when we were already halted.
      when(goReg === false.B && hartHaltedWrEn && (hartSelFuncs.hartIdToHartSel(hartHaltedId) === selectedHartReg)){
        ctrlStateNxt := CtrlState(Waiting)
      }
      when(hartExceptionWrEn) {
        assert(hartExceptionId === 0.U, "Unexpected 'EXCEPTION' hart")//Chisel3 #540, %x, expected %x", hartExceptionId, 0.U)
          ctrlStateNxt := CtrlState(Waiting)
        errorException := true.B
      }
    }.elsewhen (ctrlStateReg === CtrlState(Custom)) {
      assert(needCustom.B, "Should not be in custom state unless we need it.")
      goCustom := true.B
      val (custom, customP) = customNode.in.head
      when (custom.ready && custom.valid) {
        ctrlStateNxt := CtrlState(Waiting)
      }
    }

    when (~io.dmactive || ~dmAuthenticated) {
      ctrlStateReg := CtrlState(Waiting)
    }.otherwise {
      ctrlStateReg := ctrlStateNxt
    }
    assert ((!io.dmactive || !hartExceptionWrEn || ctrlStateReg === CtrlState(Exec)),
      "Unexpected EXCEPTION write: should only get it in Debug Module EXEC state")
  }
}

// Wrapper around TL Debug Module Inner and an Async DMI Sink interface.
// Handles the synchronization of dmactive, which is used as a synchronous reset
// inside the Inner block.
// Also is the Sink side of hartsel & resumereq fields of DMCONTROL.
class TLDebugModuleInnerAsync(device: Device, getNComponents: () => Int, beatBytes: Int)(implicit p: Parameters) extends LazyModule{

  val dmInner = LazyModule(new TLDebugModuleInner(device, getNComponents, beatBytes))
  val dmiXing = LazyModule(new TLAsyncCrossingSink(AsyncQueueParams.singleton()))
  val dmiNode = dmiXing.node
  val tlNode = dmInner.tlNode

  dmInner.dmiNode := dmiXing.node

  // Require that there are no registers in TL interface, so that spurious
  // processor accesses to the DM don't need to enable the clock.  We don't
  // require this property of the SBA, because the debugger is responsible for
  // raising dmactive (hence enabling the clock) during these transactions.
  require(dmInner.tlNode.concurrency == 0)

  lazy val module = new LazyModuleImp(this) {

    val io = IO(new Bundle {
      // These are all asynchronous and come from Outer
      val dmactive = Bool(INPUT)
      val innerCtrl = new AsyncBundle(new DebugInternalBundle(getNComponents()), AsyncQueueParams.singleton()).flip
      // This comes from tlClk domain.
      val debugUnavail    = Vec(getNComponents(), Bool()).asInput
      val hgDebugInt      = Vec(getNComponents(), Bool()).asOutput
      val extTrigger = (p(DebugModuleKey).get.nExtTriggers > 0).option(new DebugExtTriggerIO())
      val hartReset  = p(DebugModuleKey).get.hasHartResets.option(Input(Vec(getNComponents(), Bool())))
      val auth = p(DebugModuleKey).get.hasAuthentication.option(new DebugAuthenticationIO())
      val psd = new PSDTestMode().asInput
    })

    val dmactive_synced = ~ResetCatchAndSync(clock, ~io.dmactive, "dmactiveSync", io.psd)
    // Need to clock DM during reset because of synchronous reset.  The unit
    // should also be reset when dmactive_synced is low, so keep the clock
    // alive for one cycle after dmactive_synced falls to action this behavior.
    val clock_en = RegNext(dmactive_synced || reset)
    val gated_clock =
      if (!p(DebugModuleKey).get.clockGate) clock
      else ClockGate(clock, clock_en, "debug_clock_gate")

    // Keep the async-crossing sink in the gated-clock domain, both to save
    // power and also for the sake of the ready-valid handshake with dmInner
    withClock (gated_clock) {
      dmInner.module.clock := gated_clock
      dmInner.module.io.dmactive := dmactive_synced
      withReset (~dmactive_synced) {
        dmInner.module.io.innerCtrl := FromAsyncBundle(io.innerCtrl)
      }
      dmInner.module.io.debugUnavail := io.debugUnavail
      io.hgDebugInt := dmInner.module.io.hgDebugInt
      io.extTrigger.foreach { x => dmInner.module.io.extTrigger.foreach {y => x <> y}}
      io.hartReset.foreach { x => dmInner.module.io.hartReset.foreach {y => y := x}}
      io.auth.foreach { x => dmInner.module.io.auth.foreach {y => x <> y}}
      dmiXing.module.reset := false.B  // Safe AsyncQueue is reset from DMI side only
    }
  }
}

/** Create a version of the TLDebugModule which includes a synchronization interface
  * internally for the DMI. This is no longer optional outside of this module
  *  because the Clock must run when tlClock isn't running or tlReset is asserted.
  */

class TLDebugModule(beatBytes: Int)(implicit p: Parameters) extends LazyModule {

  val device = new SimpleDevice("debug-controller", Seq("sifive,debug-013","riscv,debug-013")){
    override val alwaysExtended = true
  }

  val dmOuter : TLDebugModuleOuterAsync = LazyModule(new TLDebugModuleOuterAsync(device)(p))
  val dmInner : TLDebugModuleInnerAsync = LazyModule(new TLDebugModuleInnerAsync(device, () => {dmOuter.dmOuter.intnode.edges.out.size}, beatBytes)(p))

  val node = dmInner.tlNode
  val intnode = dmOuter.intnode
  val apbNodeOpt = dmOuter.apbNodeOpt

  dmInner.dmiNode := dmOuter.dmiInnerNode

  lazy val module = new LazyModuleImp(this) {
    val nComponents = dmOuter.dmOuter.intnode.edges.out.size

    val io = IO(new Bundle {
      val ctrl = new DebugCtrlBundle(nComponents)
      val dmi = (!p(ExportDebug).apb).option(new ClockedDMIIO().flip)
      val apb_clock = p(ExportDebug).apb.option(Clock(INPUT))
      val apb_reset = p(ExportDebug).apb.option(Bool(INPUT))
      val extTrigger = (p(DebugModuleKey).get.nExtTriggers > 0).option(new DebugExtTriggerIO())
      val hartReset    = p(DebugModuleKey).get.hasHartResets.option(Input(Vec(nComponents, Bool())))
      val hartResetReq = p(DebugModuleKey).get.hasHartResets.option(Output(Vec(nComponents, Bool())))
      val auth = p(DebugModuleKey).get.hasAuthentication.option(new DebugAuthenticationIO())
      val psd = new PSDTestMode().asInput
    })

    dmOuter.module.io.dmi.foreach { dmOuterDMI =>
      dmOuterDMI <> io.dmi.get.dmi
      dmOuter.module.reset := io.dmi.get.dmiReset
      dmOuter.module.clock := io.dmi.get.dmiClock
    }

    (io.apb_clock zip io.apb_reset)  foreach { case (c, r) =>
      dmOuter.module.reset := r
      dmOuter.module.clock := c
    }

    dmInner.module.io.innerCtrl    := dmOuter.module.io.innerCtrl
    dmInner.module.io.dmactive     := dmOuter.module.io.ctrl.dmactive
    dmInner.module.io.debugUnavail := io.ctrl.debugUnavail
    dmOuter.module.io.hgDebugInt   := dmInner.module.io.hgDebugInt

    dmInner.module.io.psd <> io.psd

    io.ctrl <> dmOuter.module.io.ctrl
    io.extTrigger.foreach { x => dmInner.module.io.extTrigger.foreach {y => x <> y}}
    io.hartReset.foreach { x => dmInner.module.io.hartReset.foreach {y => y := x}}
    io.hartResetReq.foreach { x => dmOuter.module.io.hartResetReq.foreach {y => x := y}}
    io.auth.foreach { x => dmOuter.module.io.dmAuthenticated.get := x.dmAuthenticated }
    io.auth.foreach { x => dmInner.module.io.auth.foreach {y => x <> y}}
  }

  val logicalTreeNode = new DebugLogicalTreeNode(
    device,
    () => dmOuter,
    () => dmInner
  )
}
