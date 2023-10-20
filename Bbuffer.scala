package boom.acc

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import boom.exu.ygjk._


//ping-pong B buffer
class PEBbuffer extends Module with HWParameters{
  val io = IO(new Bundle{
    val full = Output(Bool())
    val datafSP = Flipped(Valid(Vec(ALineDNum, Vec(PEWidth, UInt(ALineDWidth.W)))))
    val data2PE = DecoupledIO(Vec(PEWidth,UInt(ALineDWidth.W)))
    val icb = Flipped(Valid(UInt(icWidth.W)))
  })
  
//  printf(p"PEBbuffer datafSP ${io.datafSP.valid} data2PE ${io.data2PE.valid} ${io.data2PE.ready} io.full ${io.full}\n")

  val buffer = RegInit(VecInit.tabulate(2, ALineDNum, PEWidth){(x,y,z) => 0.U(ALineDWidth.W)})
  val buffer_v = RegInit(VecInit(Seq.fill(2)(false.B)))
  val PE_use = RegInit(0.U(1.W))
  val SP_use = RegInit(0.U(1.W))
  val PEhead = RegInit(0.U(icWidth.W))


//  printf(p"buffer_v $buffer_v\n PE_use $PE_use SP_use $SP_use\n PEhead $PEhead full ${io.full}\n")
//  printf(p"----------------------------\n")


  val length = RegInit(0.U(icWidth.W))
  when(io.icb.valid){
    length := io.icb.bits
  }
//  printf(p"Bbuffer icb $length\n")

  io.full := buffer_v.reduce(_&_)
  when(io.datafSP.valid){
    buffer_v(SP_use) := true.B
    buffer(SP_use) := io.datafSP.bits
    SP_use := SP_use ^ 1.U
  }
  
  io.data2PE.valid := buffer_v(PE_use)
  io.data2PE.bits := buffer(PE_use)(PEhead)
  when(io.data2PE.fire()){
    when(PEhead+1.U===length){
      PEhead := 0.U
      PE_use := PE_use ^ 1.U
      buffer_v(PE_use) := false.B
    }.otherwise{
      PEhead := PEhead + 1.U
    }
  }
}

