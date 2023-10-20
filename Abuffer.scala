package boom.acc

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import boom.exu.ygjk._

//ping-pong A buffer
class PEAbuffer extends Module with HWParameters{
  val io = IO(new Bundle{
    val full = Output(Bool())
    val datafSP = Flipped(Vec(PEHigh,Valid(Vec(ALineDNum, UInt(ALineDWidth.W)))))
    val data2PE = DecoupledIO(Vec(PEHigh,UInt(ALineDWidth.W)))
    val icb = Flipped(Valid(UInt(icWidth.W)))
  })
  
  val buffer = RegInit(VecInit.tabulate(PEHigh, 2, ALineDNum){(x,y,z) => 0.U(ALineDWidth.W)})
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

//    printf(p"Abuffer icb $length\n")  

  io.full := buffer_v.reduce(_&_)

  when(io.datafSP.map(_.valid).reduce(_|_)){
    buffer_v(SP_use) := true.B
    for(i <- 0 until PEHigh){
      when(io.datafSP(i).valid === false.B){
        for(j <- 0 until ALineDNum){
          buffer(i)(SP_use)(j) := 0.U
        }
      }.otherwise{
        buffer(i)(SP_use) := io.datafSP(i).bits
      }
    }
    SP_use := SP_use ^ 1.U
  }
  
  io.data2PE.valid := buffer_v(PE_use)
  for(i <- 0 until PEHigh){
    io.data2PE.bits(i) := buffer(i)(PE_use)(PEhead)
  }
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


