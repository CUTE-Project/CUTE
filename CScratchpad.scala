package boom.acc

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import boom.exu.ygjk._


//为C矩阵提供数据缓存，一个类似队列的结构（读写请求以队列形式发出，对SP的的读写可以访问任意一项）
class CScratchpad extends Module with HWParameters with YGJKParameters{
  val io = IO(new Bundle{	
    val config = new ConfigPEIO
    val idle = Output(Bool())
    val ocn = Flipped(Valid(UInt(ocWidth.W)))

    val bufferRead = new Bundle{
      val v = Input(Bool())   //读请求有效
//      val oh = Input(UInt(khWidth.W))
//      val ow = Input(UInt(kwWidth.W))
      val offset = Input(UInt(ohbWidth.W))
      val data = Valid(Vec(PEHigh*PEWidth, UInt(ALineDWidth.W)))   //当拍返回数据，数据是否有效（在缓存中）
      val lastn = Input(UInt(ohbWidth.W))   //用来更新read_base
    }
      
    val bufferWrite = new Bundle{
      val v = Input(Bool())   //写请求有效
//      val oh = Input(UInt(khWidth.W))
//      val ow = Input(UInt(kwWidth.W))
      val offset = Input(UInt(ohbWidth.W))
      val data = Input(Vec(PEHigh*PEWidth, UInt(ALineDWidth.W)))   //数据写入SP中，SP缓存保证数据在
//      val last = Input(Bool())   //是否可以出SP，写入内存
      val lastn = Input(UInt(ohbWidth.W))  //需要写回内存的行数
    }

    val readMem = DecoupledIO(new Bundle{
      val vaddr = UInt(addrWidth.W)
      val req_state = UInt(log2Ceil(CSPQEntry).W)
    })
    val req_id = Input(UInt((log2Ceil(sourceNum)+1).W))

    val readData0 = Flipped(Valid(new Bundle{
      val data = Vec(dataNum,UInt(dataWidth.W))
      val id = UInt((log2Ceil(sourceNum)+1).W)
    }))

    val readData1 = Flipped(Valid(new Bundle{
      val data = Vec(dataNum,UInt(dataWidth.W))
      val id = UInt((log2Ceil(sourceNum)+1).W)
    }))

    val writeMem = DecoupledIO(new Bundle{
      val vaddr = UInt(addrWidth.W)
      val data = Vec(dataNum,UInt(dataWidth.W))    //按照TL访存接口的宽度传数据，一般情况想设置成和内部存储行一样大小
      val req_state = UInt(log2Ceil(CSPQEntry).W)
    })
  })

  val data = RegInit(VecInit.tabulate(CSPQEntry, PEHigh*PEWidth){(x,y) => 0.U(dataWidth.W)})
  val data_v = RegInit(VecInit(Seq.fill(CSPQEntry)(false.B)))
  val req_inflight = RegInit(VecInit(Seq.fill(CSPQEntry)(false.B))) 
  val req_id = RegInit(VecInit(Seq.fill(CSPQEntry)(0.U((log2Ceil(sourceNum)+1).W))))
  val vaddr = RegInit(VecInit(Seq.fill(CSPQEntry)(0.U(addrWidth.W))))
  val pad = RegInit(VecInit(Seq.fill(CSPQEntry)(false.B)))
  val lastWriteOut = RegInit(VecInit(Seq.fill(CSPQEntry)(false.B)))

  val firstAddr = RegInit(0.U(addrWidth.W))   //C矩阵的首地址
  val writep = RegInit(0.U(log2Ceil(CSPQEntry).W))   //队列头，标记将要写回内存的指针
  val addp = RegInit(0.U(log2Ceil(CSPQEntry).W))   //队列中将要接收PE累加结果的位置
  val reqp = RegInit(0.U(log2Ceil(CSPQEntry).W))   //队列尾，标记将要发送读请求的位置
  val read_base = RegInit(0.U(log2Ceil(CSPQEntry).W))   //buffer读的基下标
  val writen = RegInit(0.U(ohbWidth.W))  //C2PE模块发来的需要写回的行数

  val writeOut = Module(new PrecisionWrite).io

//  printf(p"data_v $data_v\n")
//  printf(p"CScratchpad writep $writep addp $addp reqp $reqp read_base $read_base\n")

    val oc = RegInit(0.U(ocWidth.W))
    val ic = RegInit(0.U(icWidth.W))
    val icb = RegInit(0.U(icWidth.W))
    val ih = RegInit(0.U(ohWidth.W))
    val oh = RegInit(0.U(ohWidth.W))
    val ow = RegInit(0.U(owWidth.W))
    val ohb = RegInit(0.U(ohbWidth.W))
    val strideH = RegInit(0.U(strideWidth.W))

//    printf(p"CSCratchpad oc $oc ic $ic ohb $ohb icb $icb oh $oh ow $ow\n")

    val oci = RegInit(0.U(ocWidth.W))
    val ici = RegInit(0.U(icWidth.W))
    val ohi = RegInit(0.U(ohWidth.W))  //每次加ohb
    val owi = RegInit(0.U(owWidth.W))  //每次加PEHigh(OW_BLOCK)
    val ohbi = RegInit(0.U(ohbWidth.W))

    val running = RegInit(false.B)


//  TODO读数宽度和计算结果数据量不匹配的情况未处理
//    val lineQeqNum = icb*PEWidth.U/dataNum.U    //一个循环计数需要的访存请求数量
//    val reqInLine = RegInit(0.U(icWidth.W))     //当前请求出于循环计数访存的第几个

    when(io.config.start){
      ici := 0.U
      ohi := 0.U
      owi := 0.U
      ohbi := 0.U
      running := true.B
//      printf(p"**************** start ****************\n")
    }

//    io.config.idle := !running && (writep===reqp)
      io.idle := !running && (writep===reqp)
//    printf(p"CScratchpad run ${!io.config.idle}\n")
    
    when(io.ocn.valid){
      oci := io.ocn.bits
    }
    when(io.config.oc.valid){
      oc := io.config.oc.bits
    }
    when(io.config.ic.valid){
      ic := io.config.ic.bits
    }
    when(io.config.ih.valid){
      ih := io.config.ih.bits
    }
    when(io.config.oh.valid){
      oh := io.config.oh.bits
    }
    when(io.config.ow.valid){
      ow := io.config.ow.bits
    }
    when(io.config.ohb.valid){
      ohb := io.config.ohb.bits
    }
    when(io.config.icb.valid){
      icb := io.config.icb.bits
    }
    when(io.config.Caddr.valid){
      firstAddr := io.config.Caddr.bits
    }
    when(io.config.strideH.valid){
      strideH := io.config.strideH.bits
    }

    //循环计数
//    printf(p"CSP2MEM oci $oci ici $ici ohi $ohi owi $owi ohbi $ohbi\n")
    when(running && reqp + 1.U =/= writep && (!(writep===0.U&&reqp===CSPQEntry.U-1.U)) && (io.readMem.fire() || (strideH===2.U && (ohbi+ohi)>(ih>>1.U)))){
      when((ohbi + 1.U === ohb || ohbi + ohi + 1.U === oh)){
        ohbi := 0.U
      }.otherwise{
        ohbi := ohbi + 1.U
      }

      when(ohbi + 1.U === ohb || ohbi + ohi + 1.U === oh){
        when(owi + PEHigh.U === ow){
          owi := 0.U
        }.otherwise{
          owi := owi + PEHigh.U
        }
//        printf(p"CScratchpad owi $owi\n")
      }

      when((ohbi + 1.U === ohb || ohbi + ohi + 1.U === oh)&& owi + PEHigh.U === ow){
        when(ohi + ohb >= oh){
          ohi := 0.U
        }.otherwise{
          ohi := ohi + ohb
        }
//        printf(p"CScratchpad ohi $ohi\n")
      }

      when((ohbi + 1.U === ohb || ohbi + ohi + 1.U === oh) && owi + PEHigh.U === ow && ohi + ohb >= oh){
        when(ici + 1.U === ic){
          ici := 0.U
        }.otherwise{
          ici := ici + 1.U
        }
//        printf(p"CScratchpad ici $ici\n")
      }

      when((ohbi + 1.U === ohb || ohbi + ohi + 1.U === oh) && owi + PEHigh.U === ow && ohi + ohb >= oh && ici + 1.U ===ic){
        when(oci+ocPENum.U<oc){
          oci := oci + ocPENum.U
        }.otherwise{
          running := false.B
          printf(p"CScratchpad complete\n")
        }
        printf(p"CScratchpad oci $oci\n")
      }

    }

    //内存读写
    val curAddr = firstAddr+((oci*oh*ow*PEWidth.U+(ohi+ohbi)*ow*PEWidth.U+owi*PEWidth.U)<<2.U)
    io.readMem.valid := running && reqp + 1.U =/= writep && (!(writep===0.U&&reqp===CSPQEntry.U-1.U)) && Mux(strideH===1.U, true.B, ohbi+ohb <= (ih>>1.U))
    io.readMem.bits.vaddr := curAddr
    val buffer_req_critical = Wire(Bool())
    io.readMem.bits.req_state := Mux(running, Mux(buffer_req_critical, 0.U, Mux(reqp>=writep, reqp-writep, reqp+CSPQEntry.U-writep)), CSPQEntry.U)
    
    when(io.readMem.fire()){
      req_id(reqp) := io.req_id
      req_inflight(reqp) := true.B
      vaddr(reqp) := curAddr
      lastWriteOut(reqp) := ici + 1.U === ic
      when(reqp + 1.U === CSPQEntry.U){
        reqp := 0.U
      }.otherwise{
        reqp := reqp + 1.U
      }
    }.elsewhen(strideH===2.U && ohbi+ohi>(ih>>1.U) && reqp + 1.U =/= writep && (!(writep===0.U&&reqp===CSPQEntry.U-1.U))){
      when(reqp + 1.U === CSPQEntry.U){
        reqp := 0.U
      }.otherwise{
        reqp := reqp + 1.U
      }
      req_inflight(reqp) := false.B
      data_v(reqp) := true.B
      pad(reqp) := true.B
    }

    when(io.readData0.valid){
      for(i <- 0 until CSPQEntry){
        when(req_inflight(i) && req_id(i) === io.readData0.bits.id){
          data(i) := io.readData0.bits.data
          req_inflight(i) := false.B
          data_v(i) := true.B
        }
      }
    }

    when(io.readData1.valid){
      for(i <- 0 until CSPQEntry){
        when(req_inflight(i) && req_id(i) === io.readData1.bits.id){
          data(i) := io.readData1.bits.data
          req_inflight(i) := false.B
          data_v(i) := true.B
        }
      }
    }

    writeOut.dataType := io.config.dataType
    writeOut.ohb := io.config.ohb
    writeOut.in.valid := writep =/= addp && lastWriteOut(writep)
    writeOut.in.bits.vaddr := vaddr(writep) - firstAddr
    writeOut.in.bits.data := data(writep)
    writeOut.in.bits.pad := pad(writep)

    when(writeOut.in.fire() || (io.writeMem.fire() && !writeOut.out.valid)){
      data_v(writep) := false.B
      pad(writep) := false.B
      lastWriteOut(writep) := false.B
      when(writep + 1.U === CSPQEntry.U){
        writep := 0.U
      }.otherwise{
        writep := writep + 1.U
      }
    }

    io.writeMem.bits.vaddr := Mux(writeOut.out.valid, writeOut.out.bits.vaddr + firstAddr, vaddr(writep)) 
    io.writeMem.bits.data := Mux(writeOut.out.valid, writeOut.out.bits.data, data(writep))
    io.writeMem.valid := writeOut.out.valid || (!lastWriteOut(writep) && writep=/=addp && !pad(writep))
    writeOut.out.ready := io.writeMem.ready
//    io.writeMem.bits.req_state := Mux(addp>writep, CSPQEntry.U-(addp-writep), writep-addp)
    io.writeMem.bits.req_state := Mux(writeOut.out.valid, writeOut.writeNum, Mux(addp>writep, CSPQEntry.U-(addp-writep), writep-addp))
    

    //buffer读 
    val read_index = Mux(CSPQEntry.U-io.bufferRead.offset<read_base, read_base+io.bufferRead.offset-CSPQEntry.U, read_base+io.bufferRead.offset)
    io.bufferRead.data.bits := data(read_index)
    io.bufferRead.data.valid := data_v(read_index) && io.bufferRead.v
    buffer_req_critical := io.bufferRead.v && !(data_v(read_index) || req_inflight(read_index))
    when(io.bufferRead.lastn =/= 0.U && io.bufferRead.v && io.bufferRead.data.valid){
      when(read_base+io.bufferRead.lastn>=CSPQEntry.U){
        read_base := read_base + io.bufferRead.lastn - CSPQEntry.U
      }.otherwise{
        read_base := read_base + io.bufferRead.lastn
      }
    }

    //buffer写
    when(io.bufferWrite.v){
      when(io.bufferWrite.lastn =/= 0.U){
        data(addp) := io.bufferWrite.data
        when(addp + io.bufferWrite.lastn >= CSPQEntry.U){
          addp := addp + io.bufferWrite.lastn - CSPQEntry.U
        }.otherwise{
          addp := addp + io.bufferWrite.lastn
        }
      }.otherwise{
        data(addp+io.bufferWrite.offset) := io.bufferWrite.data
      }
    }
//    printf(p"-----------------------\n")
}

class PrecisionWrite extends Module with HWParameters with YGJKParameters{
  val io  = IO(new Bundle{
    val dataType = Flipped(Valid(UInt(3.W)))
    val ohb = Flipped(Valid(UInt(ohbWidth.W)))
    val writeNum = Output(UInt(log2Ceil(PrecWriteEntry).W))
    val in = Flipped(DecoupledIO(new Bundle{
      val vaddr = UInt(addrWidth.W)
      val data = Vec(dataNum,UInt(dataWidth.W))
      val pad = Bool()
    }))
    val out = DecoupledIO(new Bundle{
      val vaddr = UInt(addrWidth.W)
      val data = Vec(dataNum,UInt(dataWidth.W))
    })
  })
  val in_index = RegInit(0.U(log2Ceil(PrecWriteEntry).W))
  val out_index = RegInit(0.U(log2Ceil(PrecWriteEntry).W))
  val line_index = RegInit(0.U(3.W))
  val addr = RegInit(VecInit(Seq.fill(PrecWriteEntry)(0.U(addrWidth.W))))
  val data = RegInit(VecInit.tabulate(PrecWriteEntry, dataNum){(x,y) => 0.U(dataWidth.W)})
  val data_v = RegInit(VecInit(Seq.fill(PrecWriteEntry)(false.B)))
  val pad = RegInit(VecInit(Seq.fill(PrecWriteEntry)(false.B)))

  io.writeNum := PopCount(data_v)
  
  val dataType = RegInit(0.U(3.W))
  val ohb = RegInit(0.U(ohbWidth.W))
  when(io.dataType.valid){
    dataType := io.dataType.bits
  }
  when(io.ohb.valid){
    ohb := io.ohb.bits
  }

  io.out.bits.vaddr := addr(out_index)
  io.out.bits.data := data(out_index)
  io.out.valid := data_v(out_index) && !pad(out_index)
  when(pad(out_index) === true.B){
    pad(out_index) := false.B
    out_index := 0.U
  }
  when(io.out.fire()){
    data_v(out_index) := false.B
    when(out_index + 1.U === ohb){
      out_index := 0.U
    }.otherwise{
      out_index := out_index + 1.U
    }
  }

  io.in.ready := !data_v.reduce(_&_) || line_index + 1.U =/= dataType
  when(line_index===0.U && io.in.fire()){
    addr(in_index) := Mux(dataType===1.U, io.in.bits.vaddr, 
                        Mux(dataType===2.U, 
                          Cat((io.in.bits.vaddr>>1.U)(addrWidth-1, log2Ceil(ygjk_memWidth)), 0.U(log2Ceil(ygjk_memWidth).W)),
                          Cat((io.in.bits.vaddr>>2.U)(addrWidth-1, log2Ceil(ygjk_memWidth)), 0.U(log2Ceil(ygjk_memWidth).W))))
  }

  when(io.in.fire()){
    when(io.in.bits.pad){
      pad(in_index) := true.B
    }
    when(dataType===1.U){
      data(in_index) := io.in.bits.data
    }.elsewhen(dataType===2.U){
      for(i <- 0 until dataNum/2){
        data(in_index)((line_index<<3.U)+i.U) := Cat(io.in.bits.data(i<<1)(15,0), io.in.bits.data((i<<1)+1)(15,0))
      }
    }.otherwise{
      for(i <- 0 until dataNum/4){
        data(in_index)((line_index<<2.U)+i.U) := Cat(Cat(io.in.bits.data(i<<2)(7,0), io.in.bits.data((i<<2)+1)(7,0)),
                                           Cat(io.in.bits.data((i<<2)+2)(7,0), io.in.bits.data((i<<2)+3)(7,0)))
      }
    }
    when(line_index + 1.U === dataType){
      data_v(in_index) := true.B
    }

    when(line_index + 1.U === dataType && in_index + 1.U === ohb){
      line_index := 0.U
    }.elsewhen(in_index + 1.U === ohb){
      line_index := line_index + 1.U
    }

    when(in_index + 1.U === ohb){
      in_index := 0.U
    }.otherwise{
      in_index := in_index + 1.U
    }
  }

}
