package boom.acc

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import boom.exu.ygjk._

//PEArray结果累加C矩阵，每个PE独立拥有
class C2PE extends Module with HWParameters{
    val io = IO(new Bundle{
      val config = new ConfigPEIO
      val ocn = Flipped(Valid(UInt(ocWidth.W)))

      val data2PE = DecoupledIO(new Bundle{
        val data = Vec(PEHigh*PEWidth, UInt(ALineDWidth.W))
 //       val khi = UInt(khWidth.W)
        val ohbi = UInt(ohWidth.W)
        val lastn = UInt(ohbWidth.W)
      })  
      
      val datafPE = Flipped(Valid(new Bundle{
        val data = Vec(PEHigh*PEWidth, UInt(ALineDWidth.W))
//        val khi = UInt(khWidth.W)
        val ohbi = UInt(ohWidth.W)
        val lastn = UInt(ohbWidth.W)
      }))

      val readC = new Bundle{
        val v = Output(Bool())   //读请求有效
        val offset = Output(UInt(ohbWidth.W))
        val data = Flipped(Valid(Vec(PEHigh*PEWidth, UInt(ALineDWidth.W))))   //当拍拿到的数据，数据是否有效（在缓存中）
        val lastn = Output(UInt(ohbWidth.W))
      }
      
      val writeC = new Bundle{
        val v = Output(Bool())   //写请求有效
        val offset = Output(UInt(ohbWidth.W))
        val data = Output(Vec(PEHigh*PEWidth, UInt(ALineDWidth.W)))   //数据写入缓存中，缓存保证数据在
//        val last = Output(Bool())   //是否可以写入内存
        val lastn = Output(UInt(ohbWidth.W))    //写回时边界最后一次因为padding不会被读到，但是需要写回
      }
    })

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
 //   val oh = (ih+2.U*paddingH-kh)/strideH + 1.U
 //   val ow = (iw+2.U*paddingW-kw)/strideW + 1.U
    val oh = RegInit(0.U(ohWidth.W))
    val ow = RegInit(0.U(owWidth.W))

//    printf(p"C2PE oc $oc ic $ic ih $ih iw $iw ohb $ohb icb $icb kh $kh kw $kw strideH $strideH strideW $strideW paddingH $paddingH paddingW $paddingW\n")

    val oci = RegInit(0.U(ocWidth.W))
    val ici = RegInit(0.U(icWidth.W))
    val ohi = RegInit(0.U(ohWidth.W))  //每次加ohb
    val owi = RegInit(0.U(owWidth.W))  //每次加PEHigh(OW_BLOCK)
    val ohbi = RegInit(0.U(ohbWidth.W))
    val khi = RegInit(0.U(khWidth.W))
    val kwi = RegInit(0.U(kwWidth.W))
    val ihi = (ohi+ohbi)*strideH+khi-paddingH

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

    io.readC.v := io.data2PE.ready && running
    io.readC.offset := ohbi
    io.readC.lastn := Mux(khi+1.U===kh && (ohbi+1.U===ohb || ohbi+ohi+1.U===oh || ihi+1.U===ih),Mux(ohb<oh-ohi, ohb, oh-ohi),0.U)

    io.data2PE.valid := io.readC.data.valid
    io.data2PE.bits.data := io.readC.data.bits
//    io.data2PE.bits.khi := khi
    io.data2PE.bits.ohbi := ohbi
    io.data2PE.bits.lastn := Mux(khi+1.U=/=kh, 0.U, Mux(ihi+1.U===ih, Mux(ohb-ohbi<oh-ohi-ohbi,ohb-ohbi,oh-ohi-ohbi), 1.U))

    io.writeC.v := io.datafPE.valid    //要写的数据一定在CScratchpad中，所以不用握手
    io.writeC.offset := io.datafPE.bits.ohbi
    io.writeC.data := io.datafPE.bits.data
//    io.writeC.last := io.datafPE.bits.khi + 1.U === kh
    io.writeC.lastn := io.datafPE.bits.lastn

    //循环计数
    //运行状态发出读请求且读到数时+1

//    printf(p"oci $oci ici $ici ohi $ohi owi $owi khi $khi ohbi $ohbi\n")
//    printf(p"readC io.readC.offset ${io.readC.offset} io.readC.last ${io.readC.last}\n")
//    printf(p"writeC io.writeC.offset ${io.writeC.offset} io.writeC.last ${io.writeC.last}\n")
//    printf(p"-----------------------\n")
    when(running && io.readC.v && io.readC.data.valid){
      //需要处理padding，只有ohbi会受到padding影响
      when(ohbi+1.U===ohb || ohbi+ohi+1.U===oh || (ohi+ohbi+1.U)*strideH+khi>=ih+paddingH){
        when(khi + 1.U ===kh){
          when((owi+PEHigh.U===ow&&ohi+ohb>=oh)||(owi+PEHigh.U=/=ow&&ohi===0.U)){
            ohbi := Mux(strideH===1.U||kh===1.U, paddingH, Mux((ohb-1.U)*2.U<paddingH, paddingH-ohb, (paddingH>>1.U)+1.U))
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

      when((ohbi+1.U===ohb || ohbi+ohi+1.U===oh || ihi + strideH >= ih)){
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

      when(khi+1.U===kh && (ohbi+1.U===ohb || ohbi+ohi+1.U===oh || ihi + strideH >= ih)){
        when(owi+PEHigh.U===ow){
          owi := 0.U
        }.otherwise{
          owi := owi + PEHigh.U
        }
//        printf(p"C2PE owi $owi\n")
      }

      when(khi+1.U===kh && (ohbi+1.U===ohb || ohbi+ohi+1.U===oh || ihi + strideH >= ih) && owi+PEHigh.U===ow){
        when(ohi+ohb>=oh){
          ohi := 0.U
        }.otherwise{
          ohi := ohi + ohb
        }
//        printf(p"C2PE ohi $ohi\n")
      }

      when(khi+1.U===kh && (ohbi+1.U===ohb || ohbi+ohi+1.U===oh || ihi + strideH >= ih) && owi+PEHigh.U===ow && ohi+ohb>=oh){
        when(ici+1.U===ic){
          ici := 0.U
        }.otherwise{
          ici := ici+1.U
        }
//        printf(p"C2PE ici $ici\n")
      }

      when(khi+1.U===kh && (ohbi+1.U===ohb || ohbi+ohi+1.U===oh || ihi + strideH >= ih) && owi+PEHigh.U===ow && ohi+ohb>=oh && ici+1.U===ic){
        when(oci+ocPENum.U<oc){
          oci := oci + ocPENum.U
        }.otherwise{
          running := false.B
          printf(p"C2PE complete\n")
        }
        printf(p"C2PE oci $oci\n")
      }
    }
}
