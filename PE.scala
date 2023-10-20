package boom.acc

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import boom.exu.ygjk._

//MAC32,一个32位数乘累加器，一个乘法延迟为MACLatency，icb次累加
class MAC32 extends Module with HWParameters{
    val io = IO(new Bundle{
        val Ain = Flipped(DecoupledIO(UInt(ALineDWidth.W)))
        val Bin = Flipped(DecoupledIO(UInt(ALineDWidth.W)))
        val Cout = DecoupledIO(UInt(ALineDWidth.W))
        val icb = Flipped(Valid(UInt(icWidth.W)))
    })

    val icb = RegInit(0.U(icWidth.W))
    val icbi = RegInit(0.U(icWidth.W))
    val res = RegInit(VecInit(Seq.fill(2)(0.U(ALineDWidth.W))))
    val res_v = RegInit(VecInit(Seq.fill(2)(false.B)))
    val resfout = RegInit(0.U(1.W))
    val resfmac = RegInit(0.U(1.W))

    when(io.icb.valid){
        icb := io.icb.bits
    }
//    printf(p"PE icbi $icbi\n")

    io.Ain.ready := (res_v(0)^res_v(1) && icb - icbi > MACLatency.U) || !res_v.reduce(_&_) //在PE中保证valid信号同时来到
    io.Bin.ready := (res_v(0)^res_v(1) && icb - icbi > MACLatency.U) || !res_v.reduce(_&_)

    val addIn = Pipe(io.Ain.fire()&&io.Bin.fire(), io.Ain.bits*io.Bin.bits, MACLatency)
    when(addIn.valid){
      when(icbi+1.U===icb){
        icbi := 0.U
        res_v(resfmac) := true.B
        resfmac := resfmac ^ 1.U
      }.otherwise{
        icbi := icbi + 1.U
      }
      res(resfmac) := res(resfmac) + addIn.bits
    }

    io.Cout.valid := res_v(resfout)
    io.Cout.bits := res(resfout)
    when(io.Cout.fire()){
      res_v(resfout) := false.B
      res(resfout) := 0.U
      resfout := resfout ^ 1.U
    }
}

//16位MAC，配合矩阵外积做A中同行连续两个16位和B中同列连续两个16位的数分别相乘再加(数据拼接由PE模块完成)，结果是一个32位数，icb和32位保持统一，实际是icb的2倍, 16位数乘1拍加1拍完成
class MAC16 extends Module with HWParameters{
    val io = IO(new Bundle{
        val Ain = Flipped(DecoupledIO(UInt(ALineDWidth.W)))
        val Bin = Flipped(DecoupledIO(UInt(ALineDWidth.W)))
        val Cout = DecoupledIO(UInt(ALineDWidth.W))
        val icb = Flipped(Valid(UInt(icWidth.W)))
    })

    val icb = RegInit(0.U(icWidth.W))
    val icbi = RegInit(0.U(icWidth.W))
    val res = RegInit(VecInit(Seq.fill(2)(0.U(ALineDWidth.W))))
    val res_v = RegInit(VecInit(Seq.fill(2)(false.B)))
    val resfout = RegInit(0.U(1.W))
    val resfmac = RegInit(0.U(1.W))
    val mulRes1 = RegInit(0.U(ALineDWidth.W))
    val mulRes2 = RegInit(0.U(ALineDWidth.W))
    val mulResV = RegInit(false.B)

    when(io.icb.valid){
        icb := io.icb.bits
    }
//    printf(p"PE icbi $icbi\n")

    io.Ain.ready := (res_v(0)^res_v(1) && icb - icbi > MACLatency.U) || !res_v.reduce(_&_) //在PE中保证valid信号同时来到
    io.Bin.ready := (res_v(0)^res_v(1) && icb - icbi > MACLatency.U) || !res_v.reduce(_&_)

    mulRes1 := io.Ain.bits(ALineDWidth/2-1, 0) * io.Bin.bits(ALineDWidth/2-1, 0)
    mulRes2 := io.Ain.bits(ALineDWidth-1, ALineDWidth/2) * io.Bin.bits(ALineDWidth-1, ALineDWidth/2)
    mulResV := io.Ain.fire() && io.Bin.fire()
    when(mulResV){
      when(icbi+1.U===icb){
        icbi := 0.U
        res_v(resfmac) := true.B
        resfmac := resfmac ^ 1.U
      }.otherwise{
        icbi := icbi + 1.U
      }
      res(resfmac) := res(resfmac) + mulRes1 + mulRes2
    }

    io.Cout.valid := res_v(resfout)
    io.Cout.bits := res(resfout)
    when(io.Cout.fire()){
      res_v(resfout) := false.B
      res(resfout) := 0.U
      resfout := resfout ^ 1.U
    }
}

//8位MAC，原理和16位一样，是4个数乘累加
class MAC8 extends Module with HWParameters{
    val io = IO(new Bundle{
        val Ain = Flipped(DecoupledIO(UInt(ALineDWidth.W)))
        val Bin = Flipped(DecoupledIO(UInt(ALineDWidth.W)))
        val Cout = DecoupledIO(UInt(ALineDWidth.W))
        val icb = Flipped(Valid(UInt(icWidth.W)))
    })

    val icb = RegInit(0.U(icWidth.W))
    val icbi = RegInit(0.U(icWidth.W))
    val res = RegInit(VecInit(Seq.fill(2)(0.U(ALineDWidth.W))))
    val res_v = RegInit(VecInit(Seq.fill(2)(false.B)))
    val resfout = RegInit(0.U(1.W))
    val resfmac = RegInit(0.U(1.W))
    val mulRes1 = RegInit(0.U(ALineDWidth.W))
    val mulRes2 = RegInit(0.U(ALineDWidth.W))
    val mulRes3 = RegInit(0.U(ALineDWidth.W))
    val mulRes4 = RegInit(0.U(ALineDWidth.W))
    val mulResV = RegInit(false.B)

    when(io.icb.valid){
        icb := io.icb.bits
    }
//    printf(p"PE icbi $icbi\n")

    io.Ain.ready := (res_v(0)^res_v(1) && icb - icbi > MACLatency.U) || !res_v.reduce(_&_) //在PE中保证valid信号同时来到
    io.Bin.ready := (res_v(0)^res_v(1) && icb - icbi > MACLatency.U) || !res_v.reduce(_&_)

    mulRes1 := io.Ain.bits(ALineDWidth/4-1, 0) * io.Bin.bits(ALineDWidth/4-1, 0)
    mulRes2 := io.Ain.bits(ALineDWidth/2-1, ALineDWidth/4) * io.Bin.bits(ALineDWidth/2-1, ALineDWidth/4)
    mulRes3 := io.Ain.bits(ALineDWidth/4*3-1, ALineDWidth/2) * io.Bin.bits(ALineDWidth/4*3-1, ALineDWidth/2)
    mulRes4 := io.Ain.bits(ALineDWidth-1, ALineDWidth/4*3) * io.Bin.bits(ALineDWidth-1, ALineDWidth/4*3)
    mulResV := io.Ain.fire() && io.Bin.fire()
    when(mulResV){
      when(icbi+1.U===icb){
        icbi := 0.U
        res_v(resfmac) := true.B
        resfmac := resfmac ^ 1.U
      }.otherwise{
        icbi := icbi + 1.U
      }
      res(resfmac) := res(resfmac) + mulRes1 + mulRes2 + mulRes3 + mulRes4
    }

    io.Cout.valid := res_v(resfout)
    io.Cout.bits := res(resfout)
    when(io.Cout.fire()){
      res_v(resfout) := false.B
      res(resfout) := 0.U
      resfout := resfout ^ 1.U
    }
}

//单个PE, 计算矩阵乘,计算A*B，+C在C供数模块处理
class PE extends Module with HWParameters{
    val io = IO(new Bundle{
      val icb = Flipped(Valid(UInt(icWidth.W)))
      val datatype = Flipped(Valid(UInt(3.W)))
      val A2PE = Flipped(DecoupledIO(Vec(PEHigh,UInt(ALineDWidth.W))))
      val B2PE = Flipped(DecoupledIO(Vec(PEWidth,UInt(ALineDWidth.W))))
      val PE2C = DecoupledIO(Vec(PEHigh,Vec(PEWidth,UInt(ALineDWidth.W))))
    })

//    printf(p"PE A2PE ${io.A2PE.valid} ${io.A2PE.ready} Bin ${io.B2PE.valid} ${io.B2PE.ready} Cout ${io.PE2C.ready} ${io.PE2C.valid}\n")

    val dataType = RegInit(0.U(3.W))
    when(io.datatype.valid){
      dataType := io.datatype.bits
    }

    val matrix32 = VecInit.tabulate(PEHigh, PEWidth){(x,y) => Module(new MAC32).io}
    val matrix16 = VecInit.tabulate(PEHigh, PEWidth){(x,y) => Module(new MAC16).io}
    val matrix8 = VecInit.tabulate(PEHigh, PEWidth){(x,y) => Module(new MAC8).io}

    val resq = RegInit(VecInit.tabulate(MACresq, PEHigh*PEWidth){(a,b) => 0.U(ALineDWidth.W)})
    val resq_vn = RegInit(0.U(log2Ceil(MACresq).W))
    val reshead = RegInit(0.U(log2Ceil(MACresq).W))
    val restail = RegInit(0.U(log2Ceil(MACresq).W))
    val resqFull = reshead===(restail+1.U)(log2Ceil(MACresq)-1, 0)
    
    //要保证AB数据同时握手
    io.A2PE.ready := Mux(dataType===1.U,matrix32(0)(0).Ain.ready && resq_vn < (MACresq-MACLatency).U && io.B2PE.valid, 
                     Mux(dataType===2.U,matrix16(0)(0).Ain.ready && resq_vn < (MACresq-1).U &&  io.B2PE.valid, 
                                        matrix8(0)(0).Ain.ready && resq_vn < (MACresq-1).U &&  io.B2PE.valid))   
    io.B2PE.ready := Mux(dataType===1.U,matrix32(0)(0).Bin.ready && resq_vn < (MACresq-MACLatency).U && io.A2PE.valid, 
                     Mux(dataType===2.U,matrix16(0)(0).Bin.ready && resq_vn < (MACresq-1).U &&  io.A2PE.valid, 
                                        matrix8(0)(0).Bin.ready && resq_vn < (MACresq-1).U &&  io.A2PE.valid))  
    for{
      i <- 0 until PEHigh
      j <- 0 until PEWidth
    } yield {
      matrix32(i)(j).icb := io.icb
      matrix32(i)(j).Ain.valid := io.A2PE.fire() && dataType===1.U
      matrix32(i)(j).Ain.bits := io.A2PE.bits(i)
      matrix32(i)(j).Bin.valid := io.B2PE.fire() && dataType===1.U
      matrix32(i)(j).Bin.bits := io.B2PE.bits(j)
      matrix32(i)(j).Cout.ready := !resqFull  && dataType===1.U

      matrix16(i)(j).icb := io.icb
      matrix16(i)(j).Ain.valid := io.A2PE.fire() && dataType===2.U
      matrix16(i)(j).Ain.bits := io.A2PE.bits(i)
      matrix16(i)(j).Bin.valid := io.B2PE.fire() && dataType===2.U
      matrix16(i)(j).Bin.bits := Cat(io.B2PE.bits(j/2)((j&1)*16+15,(j&1)*16), io.B2PE.bits(j/2+2)((j&1)*16+15,(j&1)*16))
      matrix16(i)(j).Cout.ready := !resqFull  && dataType===2.U

      matrix8(i)(j).icb := io.icb
      matrix8(i)(j).Ain.valid := io.A2PE.fire() && dataType===4.U
      matrix8(i)(j).Ain.bits := io.A2PE.bits(i)
      matrix8(i)(j).Bin.valid := io.B2PE.fire() && dataType===4.U
      matrix8(i)(j).Bin.bits := Cat(Cat(io.B2PE.bits(0)(j*8+7, j*8), io.B2PE.bits(1)(j*8+7, j*8)), 
                                    Cat(io.B2PE.bits(2)(j*8+7, j*8), io.B2PE.bits(3)(j*8+7, j*8)))
      matrix8(i)(j).Cout.ready := !resqFull  && dataType===4.U
    }

    when(matrix32(0)(0).Cout.fire() || matrix16(0)(0).Cout.fire() || matrix8(0)(0).Cout.fire()){
      for{
        i <- 0 until PEHigh
        j <- 0 until PEWidth
      } yield {
        resq(restail)(i*PEWidth+j) := Mux(dataType===1.U, matrix32(i)(j).Cout.bits,
                                      Mux(dataType===2.U, matrix16(i)(j).Cout.bits, matrix8(i)(j).Cout.bits)) 
      }
      when(restail+1.U===MACresq.U){
        restail := 0.U
      }.otherwise{
        restail := restail + 1.U
      }
    }

    io.PE2C.valid := restail =/= reshead
    for{
        i <- 0 until PEHigh
        j <- 0 until PEWidth
    } yield {
        io.PE2C.bits(i)(j) := resq(reshead)(i*PEWidth+j) 
    }
    when(io.PE2C.fire()){
      when(reshead+1.U===MACresq.U){
        reshead := 0.U
      }.otherwise{
        reshead := reshead + 1.U
      }
    }

    when((matrix32(0)(0).Cout.fire() || matrix16(0)(0).Cout.fire() || matrix8(0)(0).Cout.fire()) && !io.PE2C.fire()){
      resq_vn := resq_vn + 1.U
    }.elsewhen(!(matrix32(0)(0).Cout.fire() || matrix16(0)(0).Cout.fire() || matrix8(0)(0).Cout.fire()) && io.PE2C.fire()){
      resq_vn := resq_vn - 1.U
    }

}

