package boom.acc

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import boom.exu.ygjk._

//PEArray访问B矩阵的转发广播部件
class B2PEbroadcast extends Module with HWParameters{
    val io = IO(new Bundle{
      val config = new ConfigPEIO
      val ocn = Flipped(Valid(UInt(ocWidth.W)))

      val PEsBbufferFull = Input(Vec(ohPENum, Bool()))
      val Data2PEs = Valid(Vec(ALineDNum, Vec(PEWidth, UInt(ALineDWidth.W))))
      
      val readB = new Bundle{
        val v = Output(Bool())
        val kh = Output(UInt(khWidth.W))
        val kw = Output(UInt(kwWidth.W))
        val data = Input(Valid(Vec(ALineDNum, Vec(PEWidth, UInt(ALineDWidth.W)))))  //需要读的数是否都在缓存中
        val lastohb = Output(Bool())  //B模块使用时，ohb维处于最后一个下标
        val lastow = Output(Bool())   //
        val lastoh = Output(Bool())   //
      }
    })
//    printf(p"B2PEbroadcast Data2PEs ${io.Data2PEs.valid} readB.v ${io.readB.v} readB.data.valid ${io.readB.data.valid} PEsBufferFull ${io.PEsBbufferFull}\n")
    val oc = RegInit(0.U(ocWidth.W))
    val ic = RegInit(0.U(icWidth.W))
    val ih = RegInit(0.U(ohWidth.W))
    val iw = RegInit(0.U(owWidth.W))
    val ohb = RegInit(0.U(ohbWidth.W))
    val icb = RegInit(0.U(icWidth.W))
    val kh = RegInit(0.U(khWidth.W))
    val kw = RegInit(0.U(kwWidth.W))
    val paddingH = RegInit(0.U(paddingWidth.W))
    val paddingW = RegInit(0.U(paddingWidth.W))
    val strideH = RegInit(0.U(strideWidth.W))
    val strideW = RegInit(0.U(strideWidth.W))
//    val oh = (ih+2.U*paddingH-kh)/strideH + 1.U
//    val ow = (iw+2.U*paddingW-kw)/strideW + 1.U
    val oh = RegInit(0.U(ohWidth.W))
    val ow = RegInit(0.U(owWidth.W))

//    printf(p"B2PEbroadcast oc $oc ic $ic ih $ih iw $iw ohb $ohb icb $icb kh $kh kw $kw strideH $strideH strideW $strideW paddingH $paddingH paddingW $paddingW\n")    
    
    val oci = RegInit(0.U(ocWidth.W))
    val ici = RegInit(0.U(icWidth.W))
    val ohi = RegInit(0.U(ohWidth.W))  //每次加ohb
    val owi = RegInit(0.U(owWidth.W))  //每次加PEHigh(OW_BLOCK)
    val ohbi = RegInit(0.U(ohbWidth.W))
    val khi = RegInit(0.U(khWidth.W))
    val kwi = RegInit(0.U(kwWidth.W))

    val running = RegInit(false.B)

    when(io.config.start){
      ici := 0.U
      ohi := 0.U
      owi := 0.U
      ohbi := Mux(strideH===1.U || kh===1.U, paddingH, Mux((ohb-1.U)*2.U<paddingH, paddingH-ohb, (paddingH>>1.U)+1.U))
      khi := Mux((ohb-1.U)*2.U<paddingH, 1.U, 0.U)
      kwi := 0.U
      running := true.B
    }

//    io.config.idle := !running
    
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
    when(io.config.iw.valid){
      iw := io.config.iw.bits
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
    when(io.config.kh.valid){
      kh := io.config.kh.bits
    }
    when(io.config.kw.valid){
      kw := io.config.kw.bits
    }
    when(io.config.paddingH.valid){
      paddingH := io.config.paddingH.bits
    }
    when(io.config.paddingW.valid){
      paddingW := io.config.paddingW.bits
    }
    when(io.config.strideH.valid){
      strideH := io.config.strideH.bits
    }
    when(io.config.strideW.valid){
      strideW := io.config.strideW.bits
    }

    when(!io.PEsBbufferFull.reduce(_|_) && running){
        io.readB.v := true.B
        io.readB.kh := khi
        io.readB.kw := kwi
    }.otherwise{
      io.readB.v := false.B
      io.readB.kh := 0.U
      io.readB.kw := 0.U
    }

    io.Data2PEs.bits := io.readB.data.bits
    io.Data2PEs.valid := io.readB.v && io.readB.data.valid
    
    //循环计数
    //运行状态发出读请求且读到数时+1
    val ihi = (ohi+ohbi)*strideH+khi-paddingH
//    printf(p"oci $oci ici $ici ohi $ohi owi $owi khi $khi ohbi $ohbi kwi $kwi\n")
//    printf(p"lastoh ${io.readB.lastoh} io.readB.lastow ${io.readB.lastow} io.readB.lastohb${io.readB.lastohb}\n")
//    printf(p"-----------------------\n")
    when(running && io.readB.v && io.readB.data.valid){
      when(kwi+1.U===kw){
        kwi := 0.U
      }.otherwise{
        kwi := kwi + 1.U
      }

      //需要处理padding，只有ohbi会受到padding影响，进而表现为同khi中kwi循环的次数变少
      when(kwi+1.U===kw){
        when(ohbi+1.U===ohb || ohbi+ohi+1.U===oh || (ohi+ohbi+1.U)*strideH+khi>=ih+paddingH){
          when(khi + 1.U ===kh){
            when((owi+PEHigh.U===ow&&ohi+ohb>=oh)||(owi+PEHigh.U=/=ow&&ohi===0.U)){
              ohbi := Mux(strideH===1.U || kh===1.U, paddingH, Mux((ohb-1.U)*2.U<paddingH, paddingH-ohb, (paddingH>>1.U)+1.U))
            }.otherwise{
              ohbi := 0.U
            }
          }.otherwise{
            when(ohi*strideH+khi+1.U<paddingH){
              ohbi := Mux(strideH===1.U, paddingH - khi - 1.U, (( paddingH - khi - 1.U)>>1.U)+(paddingH-khi-1.U)(0))
            }.otherwise{
              ohbi := 0.U
            }
          }
        }.otherwise{
          ohbi := ohbi + 1.U
        }
      }

      when(kwi+1.U===kw && (ohbi+1.U===ohb || ohbi+ohi+1.U===oh || ihi + strideH >= ih)){
        when(khi+1.U===kh){
          when(((owi+PEHigh.U===ow&&ohi+ohb>=oh)||(owi+PEHigh.U=/=ow&&ohi===0.U)) && strideH=/=1.U && (ohb-1.U)*2.U<paddingH){
            khi := 1.U
          }.otherwise{
            khi := 0.U
          }          
        }.otherwise{
          khi := khi + 1.U
        }
      }

      when(kwi+1.U===kw && khi+1.U===kh && (ohbi+1.U===ohb || ohbi+ohi+1.U===oh || ihi + strideH >= ih)){
        when(owi+PEHigh.U===ow){
          owi := 0.U
        }.otherwise{
          owi := owi + PEHigh.U
        }
//        printf(p"B2PEbroadcast owi $owi\n")
      }

      when(kwi+1.U===kw && khi+1.U===kh && (ohbi+1.U===ohb || ohbi+ohi+1.U===oh || ihi + strideH >= ih) && owi+PEHigh.U===ow){
        when(ohi+ohb>=oh){
          ohi := 0.U
        }.otherwise{
          ohi := ohi + ohb
        }
//        printf(p"B2PEbroadcast ohi $ohi\n")
      }

      when(kwi+1.U===kw && khi+1.U===kh && (ohbi+1.U===ohb || ohbi+ohi+1.U===oh || ihi + strideH >= ih) && owi+PEHigh.U===ow && ohi+ohb>=oh){
        when(ici+1.U===ic){
          ici := 0.U
        }.otherwise{
          ici := ici+1.U
        }
//        printf(p"B2PEbroadcast ici $ici\n")
      }

      when(kwi+1.U===kw && khi+1.U===kh && (ohbi+1.U===ohb || ohbi+ohi+1.U===oh || ihi + strideH >= ih) && owi+PEHigh.U===ow && ohi+ohb>=oh && ici+1.U===ic){
        when(oci+ocPENum.U<oc){
          oci := oci + ocPENum.U
        }.otherwise{
          running := false.B
          printf(p"B2PEbroadcast complete\n")
        }
        printf(p"B2PEbroadcast oci $oci\n")
      }
    }
    io.readB.lastohb := ohbi+1.U===ohb || ohbi+ohi+1.U===oh || (ohi+ohbi+1.U)*strideH+khi>=ih+paddingH
    io.readB.lastoh := ohi+ohb>=oh
    io.readB.lastow := owi+PEHigh.U===ow
}
