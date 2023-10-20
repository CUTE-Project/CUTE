package boom.acc

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import boom.exu.ygjk._


//PEArray访问A矩阵的转发广播部件
class A2PEbroadcast extends Module with HWParameters{
    val io = IO(new Bundle{
      val config = new ConfigPEIO

      val PEsAbufferFull = Input(Vec(ocPENum, Bool()))
      val Data2PEs = Vec(PEHigh,Valid(Vec(ALineDNum, UInt(ALineDWidth.W))))
      
      val readA = Vec(PEHigh, new Bundle{
        val v = Output(Bool())
        val ih = Output(UInt(ohWidth.W))
        val iw = Output(UInt(owWidth.W))
        val data = Input(Valid(Vec(ALineDNum, UInt(ALineDWidth.W))))
        val lasth = Output(Bool())
        val lastw = Output(Bool())
      })
    })
    //配置参数
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

//    printf(p"A2PEbroadcast oc $oc ic $ic ih $ih iw $iw ohb $ohb icb $icb kh $kh kw $kw strideH $strideH strideW $strideW paddingH $paddingH paddingW $paddingW\n")    
    
    //循环变量
    val oci = RegInit(0.U(ocWidth.W))
    val ici = RegInit(0.U(icWidth.W))
    val ohi = RegInit(0.U(ohWidth.W))  //每次加ohb
    val owi = RegInit(0.U(owWidth.W))  //每次加PEHigh(OW_BLOCK)
    val ohbi = RegInit(0.U(ohbWidth.W))
    val khi = RegInit(0.U(khWidth.W))
    val kwi = RegInit(0.U(kwWidth.W))
    val ihi = WireInit(0.U(ohWidth.W))
    val iwi = WireInit(0.U(owWidth.W))

    val running = RegInit(false.B)

    when(io.config.start){
      oci := 0.U
      ici := 0.U
      ohi := 0.U
      owi := 0.U
      ohbi := Mux(strideH===1.U || kh===1.U, paddingH, Mux((ohb-1.U)*2.U<paddingH, paddingH-ohb, (paddingH>>1.U)+1.U))
      khi := Mux((ohb-1.U)*2.U<paddingH, 1.U, 0.U)
      kwi := 0.U
      running := true.B
    }

//    io.config.idle := !running
//    printf(p"A2PEbroadcast run ${!io.config.idle}\n")

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

    //读数请求
    when(!io.PEsAbufferFull.zipWithIndex.map(p=>p._1 && oci+p._2.U<oc).reduce(_|_) && running) {
      for(i <- 0 until PEHigh){
//        when((owi===0.U&&kwi+PEHigh.U-i.U-1.U<paddingW) || (iwi+i.U>=iw)){
        when((owi===0.U&&kwi+i.U*strideW<paddingW) || (iwi+i.U*strideW>=iw)){
          io.readA(i).v := false.B
        }.otherwise{
          io.readA(i).v := true.B
        }
      }
    }.otherwise{
      io.readA.map{_.v := false.B}
    }

    io.readA.map{_.ih := ihi}
    io.readA.zipWithIndex.foreach {
      case(read, i) => 
        read.iw := Mux((owi+i.U)*strideW+kwi>paddingW, (owi+i.U)*strideW+kwi-paddingW, 0.U)
    }

    for(i <- 0 until PEHigh){
//      printf(p"ih${io.readA(i).ih} iw${io.readA(i).iw} v${io.readA(i).v} lasth${io.readA(i).lasth} lastw${io.readA(i).lastw}\n")
    }
    
    //最后一次访问的控制
//    io.readA.map(_.lasth := khi===0.U || ohbi+1.U===ohb)
    io.readA.map(_.lasth := ohbi===0.U || khi+strideH>=kh)
    io.readA.zipWithIndex.foreach{
      case(r, i) =>
/*
        r.lastw := (khi===0.U || ohbi+1.U===ohb) &&    //lasth，在最后一次访问的ih维才能标记最后访问的iwi
                   (iwi+i.U<owi+PEHigh.U-paddingW || owi+PEHigh.U===ow) &&  //iw维会有重叠，保证不标记下一个区域的iw维坐标，最后一个域中数据不会被下一个域重叠
                   (owi>1.U||kwi>=paddingW) &&           //iw维最开头的元素由于padding存在所以从iwi=0开始读数，但此时i==0还会被使用
                   (i.U===0.U||kwi+1.U===kw)             //在上述条件下，一般情况中，i==0不会被再次用到，该区域要结束访问时所有与下个区域不重叠的行
*/
        r.lastw := (ohbi===0.U || khi+strideH>=kh) &&    //lasth，在最后一次访问的ih维才能标记最后访问的iwi
                   (iwi+i.U*strideW<(owi+PEHigh.U)*strideW-paddingW || owi+PEHigh.U>=ow) &&  //iw维会有重叠，保证不标记下一个区域的iw维坐标，最后一个域中数据不会被下一个域重叠
                   (owi>1.U||kwi>=paddingW) &&           //iw维最开头的元素由于padding存在所以从iwi=0开始读数，但此时i==0还会被使用
                   (i.U===0.U||kwi+strideW>=kw)             //在上述条件下，一般情况中，i==0不会被再次用到，该区域要结束访问时所有与下个区域不重叠的行
    }

    val readenough = ((io.readA zip io.readA).map{case(v,data)=> (v.v===false.B ||v.v===data.data.valid)}.reduce(_&_)) && io.readA.map(_.v).reduce(_|_)
    for(i <- 0 until PEHigh){
        io.Data2PEs(i).bits := io.readA(i).data.bits
        io.Data2PEs(i).valid := readenough && io.readA(i).v 
    }
    

    //循环计数
    //运行状态且读到足够的数时 +1
    iwi := Mux(owi*strideW+kwi>paddingW, owi*strideW+kwi-paddingW, 0.U)
    ihi := (ohi+ohbi)*strideH+khi-paddingH
//    printf(p"ohi$ohi ohbi$ohbi khi$khi\n")
//    printf(p"owi$owi kwi$kwi\n")
    
    when(running && readenough){
      when(kwi+1.U===kw){
        kwi := 0.U
      }.otherwise{
        kwi := kwi + 1.U
      }

/*
 **  循环中ohbi在外层，khi在内层
      when(kwi+1.U===kw){    //kwi到达kw时khi变化
        when(khi + 1.U === kh || ihi + 1.U === ih){
          when(ohbi + 1.U === ohb){
            when((owi+PEHigh.U===ow&&ohi+ohb===oh)||(owi+PEHigh.U=/=ow&&ohi===0.U)){
              khi := paddingH
            }.otherwise{
              khi := 0.U
            }
          }.otherwise{
            when((ohi+ohbi+1.U)*strideH>paddingH){
              khi := 0.U
            }.otherwise{
              khi := paddingH - (ohi+ohbi+1.U)*strideH
            }
          }
        }.otherwise{
          khi := khi + 1.U
        }
      }

      when(kwi+1.U===kw && khi+1.U===kh){
        when(ohbi+1.U===ohb){
          ohbi := 0.U
        }.otherwise{
          ohbi := ohbi + 1.U
        }
      }
*/

      //循环中ohbi在内层，khi在外层
      when(kwi+1.U===kw){    //kwi到达kw时ohbi变化
        when(ohbi + 1.U === ohb || ohbi+ohi+1.U===oh || ihi + strideH >= ih){
          when(khi + 1.U === kh){
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
//        printf(p"A2PEbroadcast owi $owi\n")
      }

      when(kwi+1.U===kw && khi+1.U===kh && (ohbi+1.U===ohb || ohbi+ohi+1.U===oh || ihi + strideH >= ih) && owi+PEHigh.U===ow){
        when(ohi+ohb>=oh){
          ohi := 0.U
        }.otherwise{
          ohi := ohi + ohb
        }
//        printf(p"A2PEbroadcast ohi $ohi\n")
      }

      when(kwi+1.U===kw && khi+1.U===kh && (ohbi+1.U===ohb || ohbi+ohi+1.U===oh || ihi + strideH >= ih) && owi+PEHigh.U===ow && ohi+ohb>=oh){
        when(ici+1.U===ic){
          ici := 0.U
        }.otherwise{
          ici := ici+1.U
        }
//        printf(p"A2PEbroadcast ici $ici\n")
      }

      when(kwi+1.U===kw && khi+1.U===kh && (ohbi+1.U===ohb || ohbi+ohi+1.U===oh || ihi + strideH >= ih) && owi+PEHigh.U===ow && ohi+ohb>=oh && ici+1.U===ic){
        oci := oci + ocPENum.U
        when(oci+ocPENum.U>=oc){
          running := false.B
          printf(p"A2PEbroadcast complete\n")
        }
        printf(p"A2PEbroadcast oci $oci\n")
      }

    }
//    printf(p"----------------------------------\n")
}
