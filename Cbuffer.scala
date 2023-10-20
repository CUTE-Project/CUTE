package boom.acc

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import boom.exu.ygjk._

//PEArray结果累加C矩阵，每个PE独立拥有
class Cbuffer extends Module with HWParameters{
    val io = IO(new Bundle{
      val kw = Flipped(Valid(UInt(kwWidth.W)))

      val PEres = Flipped(DecoupledIO(Vec(PEHigh,Vec(PEWidth,UInt(ALineDWidth.W))))) //PE送来的累加值，需要再和读来的数在本模块中相加

      val datafC = Flipped(DecoupledIO(new Bundle{
        val data = Vec(PEHigh*PEWidth, UInt(ALineDWidth.W))
//        val khi = UInt(khWidth.W)
        val ohbi = UInt(ohWidth.W)
        val lastn = UInt(ohbWidth.W)
      }))

      val data2C = Valid(new Bundle{
        val data = Vec(PEHigh*PEWidth, UInt(ALineDWidth.W))
//        val khi = UInt(khWidth.W)
        val ohbi = UInt(ohWidth.W)
        val lastn = UInt(ohbWidth.W)
      })
    })
    
    val buffer = RegInit(VecInit.tabulate(2, PEHigh*PEWidth){(x,y) => 0.U(ALineDWidth.W)})
//    val khi = RegInit(VecInit(Seq.fill(2)(0.U(khWidth.W))))  //记录当前数据的ohbi和kwi，在累加完写回时使用
    val ohbi = RegInit(VecInit(Seq.fill(2)(0.U(ohWidth.W))))
    val lastn = RegInit(VecInit(Seq.fill(2)(0.U(ohbWidth.W))))
    val writeMem = RegInit(VecInit(Seq.fill(2)(false.B)))  //是否需要写回内存
    val buffer_v = RegInit(VecInit(Seq.fill(2)(false.B)))  //可以用于累加
    val PE_use = RegInit(0.U(1.W))
    val SP_r_use = RegInit(0.U(1.W))
    val SP_w_use = RegInit(0.U(1.W))

    val kw = RegInit(0.U(kwWidth.W))  //计算需要与PE累加结果握手kw次
    val kwi = RegInit(0.U(kwWidth.W))
//    printf(p"PEres valid${io.PEres.valid} ready${io.PEres.ready}\n")
//    printf(p"datafC valid${io.datafC.valid} ready${io.datafC.ready}\n")
//    printf(p"data2C valid${io.data2C.valid}\n")
//    printf(p"writeMem ${writeMem} buffer_v ${buffer_v} PE_use ${PE_use} SP_r_use ${SP_r_use} SP_w_use ${SP_w_use} kwi $kwi kw $kw\n")
//    printf(p"-------------------------\n")

    when(io.kw.valid){
      kw := io.kw.bits
    }
//    printf(p"Cbuffer kw $kw\n")

    io.datafC.ready := buffer_v(SP_r_use) === false.B
    when(io.datafC.fire()){
      buffer(SP_r_use) := io.datafC.bits.data
      buffer_v(SP_r_use) := true.B
      ohbi(SP_r_use) := io.datafC.bits.ohbi
      lastn(SP_r_use) := io.datafC.bits.lastn
//      khi(SP_r_use) := io.datafC.bits.khi
      SP_r_use := SP_r_use ^ 1.U
    }

    io.PEres.ready := buffer_v(PE_use)
    when(io.PEres.fire()){
      for{
        i <- 0 until PEHigh
        j <- 0 until PEWidth
      } yield {
        buffer(PE_use)(i*PEWidth+j) := buffer(PE_use)(i*PEWidth+j) + io.PEres.bits(i)(j)
      }
      when(kwi + 1.U === kw){
        kwi := 0.U
        PE_use := PE_use ^ 1.U
        writeMem(PE_use) := true.B
      }.otherwise{
        kwi := kwi + 1.U
      }
    }

    io.data2C.valid := writeMem(SP_w_use)
    io.data2C.bits.ohbi := ohbi(SP_w_use)
    io.data2C.bits.lastn := lastn(SP_w_use)
//    io.data2C.bits.khi := khi(SP_w_use)
    for{
        i <- 0 until PEHigh
        j <- 0 until PEWidth
      } yield {
        io.data2C.bits.data(i*PEWidth+j) := buffer(SP_w_use)(i*PEWidth+j)
    }
    when(io.data2C.valid){
      SP_w_use := SP_w_use ^ 1.U
      buffer_v(SP_w_use) := false.B
      writeMem(SP_w_use) := false.B
    }
/*
    when(io.data2C.valid){
      printf(p"C ")
      for{
          i <- 0 until PEHigh
          j <- 0 until PEWidth
        } yield {
          printf(p"${io.data2C.bits.data(i*PEWidth+j)} ")
      }
      printf(p"\n")
    }
    */
}
