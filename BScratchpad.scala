package boom.acc

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import boom.exu.ygjk._

class BScratchpad extends Module with YGJKParameters with HWParameters{
	val io = IO(new Bundle{
		val config = new ConfigPEIO
		val ocn = Flipped(Valid(UInt(ocWidth.W)))

		val bufferRead = new Bundle{
      val v = Input(Bool())
      val kh = Input(UInt(khWidth.W))
      val kw = Input(UInt(kwWidth.W))
      val data = Valid(Vec(ALineDNum, Vec(PEWidth, UInt(ALineDWidth.W))))
      val lastohb = Input(Bool())  //B模块使用时，ohb维处于最后一个下标
      val lastow = Input(Bool())   //
      val lastoh = Input(Bool())   //
    }
      
    val readMem = DecoupledIO(new Bundle{
    	val vaddr = UInt(addrWidth.W)
      val req_state = UInt(log2Ceil(BSPQEntry).W)
    })
    val req_id = Input(UInt(6.W))

	  val readData0 = Flipped(Valid(new Bundle{
    	val data = Vec(dataNum,UInt(dataWidth.W))
      val id = UInt((log2Ceil(sourceNum)+1).W)
    }))

	  val readData1 = Flipped(Valid(new Bundle{
    	val data = Vec(dataNum,UInt(dataWidth.W))
      val id = UInt((log2Ceil(sourceNum)+1).W)
    }))    
	})

	val data = RegInit(VecInit.tabulate(BSPQEntry, BICBNum, PEWidth){(x,y,z) => 0.U(dataWidth.W)})
	val data_in_queue = RegInit(VecInit(Seq.fill(BSPQEntry)(false.B)))
	val req_id = RegInit(VecInit.tabulate(BSPQEntry, BICBNum*PEWidth/dataNum){(x,y) => 0.U((log2Ceil(sourceNum+1)+1).W)})
	val req_inflight = RegInit(VecInit.tabulate(BSPQEntry, BICBNum*PEWidth/dataNum){(x,y) => false.B})
  val req_w2spi = RegInit(VecInit.tabulate(BSPQEntry, BICBNum*PEWidth/dataNum){(x,y) => 0.U(log2Ceil(dataNum).W)})
	val khkw = RegInit(VecInit.tabulate(BSPQEntry,2){(x,y) => 0.U(khWidth.W)})

	val firstAddr = RegInit(0.U(addrWidth.W))   //B矩阵的首地址
	val head = RegInit(0.U(log2Ceil(BSPQEntry).W)) 
  val tail = RegInit(0.U(log2Ceil(BSPQEntry).W))   
  val data_in_queue_num = Mux(tail>head, tail-head, 
                              Mux(tail<head, tail+BSPQEntry.U-head,
                                  Mux(data_in_queue(head), BSPQEntry.U, 0.U)))
  val id_tmp = RegInit(0.U((log2Ceil(sourceNum)+1).W))
  val w2spi_tmp = RegInit(0.U(log2Ceil(dataNum).W))

  	val oc = RegInit(0.U(ocWidth.W))
    val ic = RegInit(0.U(icWidth.W))
    val ohb = RegInit(0.U(ohbWidth.W))
    val icb = RegInit(0.U(icWidth.W))
    val kh = RegInit(0.U(khWidth.W))
    val kw = RegInit(0.U(kwWidth.W))
    val ih = RegInit(0.U(ohWidth.W))
    val iw = RegInit(0.U(owWidth.W))
    val strideH = RegInit(0.U(strideWidth.W))
    val strideW = RegInit(0.U(strideWidth.W))
    val paddingH = RegInit(0.U(paddingWidth.W))
    val paddingW = RegInit(0.U(paddingWidth.W))
//    val oh = (ih+2.U*paddingH-kh)/strideH + 1.U
//    val ow = (iw+2.U*paddingW-kw)/strideW + 1.U
    val oh = RegInit(0.U(ohWidth.W))
    val ow = RegInit(0.U(owWidth.W))


//    printf(p"BSCratchpad oc $oc ic $ic ih $ih iw $iw ohb $ohb icb $icb kh $kh kw $kw strideH $strideH strideW $strideW paddingH $paddingH paddingW $paddingW\n")
    
    val oci = RegInit(0.U(ocWidth.W))
    val ici = RegInit(0.U(icWidth.W))
    val ohi = RegInit(0.U(ohWidth.W))  //每次加ohb
    val owi = RegInit(0.U(owWidth.W))  //每次加PEHigh(OW_BLOCK)
    val ohbi = RegInit(0.U(ohbWidth.W))
    val khi = RegInit(0.U(khWidth.W))
    val kwi = RegInit(0.U(kwWidth.W))

    val running = RegInit(false.B)
    val sizeenough = kh*kw <= BSPQEntry.U   // 判断缓存大小是够能放下kh*kw个块

    when(io.config.start){
      ici := 0.U
      ohi := 0.U
      owi := 0.U
      ohbi := Mux(strideH===1.U || kh===1.U, paddingH, Mux((ohb-1.U)*2.U<paddingH, paddingH-ohb, (paddingH>>1.U)+1.U))
      khi := Mux((ohb-1.U)*2.U<paddingH, 1.U, 0.U)
      kwi := 0.U
      running := true.B
//      printf(p"*************** start ***************\n")
    }

//    io.config.idle := !running
//    printf(p"BScratchpad run ${!io.config.idle}\n")
    
    when(io.ocn.valid){
      oci := io.ocn.bits
    }
    when(io.config.oc.valid){
      oc := io.config.oc.bits
    }
    when(io.config.ic.valid){
      ic := io.config.ic.bits
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
    when(io.config.strideW.valid){
      strideW := io.config.strideW.bits
    }
    when(io.config.strideH.valid){
      strideH := io.config.strideH.bits
    }
    when(io.config.paddingH.valid){
      paddingH := io.config.paddingH.bits
    }
    when(io.config.paddingW.valid){
      paddingW := io.config.paddingW.bits
    }
    when(io.config.Baddr.valid){
      firstAddr := io.config.Baddr.bits
    }

    //访存
    val lineReqNum = icb*PEWidth.U/dataNum.U    //一个循环计数需要的访存请求数量，等于0时表示一次访存可以填充多个计算域
    val reqInLine = RegInit(0.U(icWidth.W))     //lineReqNum>=1时，表示当前请求处于循环计数访存的第几个；lineReqNum>=1时，表示循环计数慢于访存的次数
    val curAddr = firstAddr + ((oci*ic*kh*kw*PEWidth.U*icb + ici*kh*kw*icb*PEWidth.U + khi*kw*icb*PEWidth.U + kwi*icb*PEWidth.U) << 2.U)
    val reqLineLast = (reqInLine + 1.U === lineReqNum) || lineReqNum === 0.U

    io.readMem.valid := running && data_in_queue_num < kh*kw && 
                        ((lineReqNum > 0.U && (data_in_queue_num + 1.U =/= BSPQEntry.U || reqInLine=/=0.U)) ||
                        (lineReqNum === 0.U && reqInLine===0.U && data_in_queue_num +reqInLine+ dataNum.U/PEWidth.U <= BSPQEntry.U))

    io.readMem.bits.vaddr := Mux(lineReqNum>1.U, curAddr+((reqInLine*dataNum.U)<<2.U), 
                                 Cat(curAddr(addrWidth-1, log2Ceil(ygjk_memWidth)), 0.U(log2Ceil(ygjk_memWidth).W)))   //发出对齐的地址
    val buffer_req_critical = Wire(Bool())
    io.readMem.bits.req_state := Mux(running, Mux(buffer_req_critical, 0.U, Mux(tail>=head, tail-head, tail+BSPQEntry.U-head)), BSPQEntry.U)

    //队列维护
    val data_pre = RegInit(0.U(icWidth.W)) 
    val index_pre = RegInit(0.U(log2Ceil(BSPQEntry).W)) 
    when(lineReqNum >= 1.U){
      when(io.readMem.fire()){
        when(reqLineLast){
          reqInLine := 0.U
          khkw(tail)(0) := khi
    		  khkw(tail)(1) := kwi
          when(tail + 1.U === BSPQEntry.U){
            tail := 0.U
          }.otherwise{   //sizeenough和tail没有到缓存最后一个区域的时候都是+1
            tail := tail + 1.U  
          }
          data_in_queue(tail) := true.B
        }.otherwise{
          reqInLine := reqInLine + 1.U
        }
        req_id(tail)(reqInLine) := io.req_id
      	req_inflight(tail)(reqInLine) := true.B
      }
    }.otherwise{
      when((io.readMem.fire() || data_pre =/= 0.U)){
        when(io.readMem.fire()){
          data_pre := Mux(icb===2.U,
                            (dataNum/PEWidth/2).U - 1.U - curAddr(log2Ceil(ygjk_memWidth)-1, log2Ceil((ADWidth*PEWidth)>>3)),
                            (dataNum/PEWidth).U - 1.U - curAddr(log2Ceil(ygjk_memWidth)-1, log2Ceil((ADWidth*PEWidth)>>3)))
          req_id(tail)(0) := io.req_id
          req_inflight(tail)(0) := true.B
          req_w2spi(tail)(0) := curAddr(log2Ceil(ygjk_memWidth)-1, log2Ceil(ADWidth>>3))
          id_tmp := io.req_id
          w2spi_tmp := curAddr(log2Ceil(ygjk_memWidth)-1, log2Ceil(ADWidth>>3)) + dataNum.U/PEWidth.U
          when(tail+1.U===BSPQEntry.U){
            index_pre := 0.U
          }.otherwise{
            index_pre := tail + 1.U
          }
        }.otherwise{
          data_pre := data_pre - 1.U
          req_id(index_pre)(0) := id_tmp
          req_w2spi(index_pre)(0) := w2spi_tmp
          w2spi_tmp := w2spi_tmp + dataNum.U/PEWidth.U
          req_inflight(index_pre)(0) := true.B
          when(index_pre+1.U===BSPQEntry.U){
            index_pre := 0.U
          }.otherwise{
            index_pre := index_pre + 1.U
          }
        }
      }
      when((io.readMem.fire() || reqInLine =/= 0.U)&&data_in_queue_num<kh*kw){
        when(io.readMem.fire()){
          reqInLine := Mux(icb===2.U,
                            (dataNum/PEWidth/2).U - 1.U - curAddr(log2Ceil(ygjk_memWidth)-1, log2Ceil((ADWidth*PEWidth)>>3)),
                            (dataNum/PEWidth).U - 1.U - curAddr(log2Ceil(ygjk_memWidth)-1, log2Ceil((ADWidth*PEWidth)>>3)))
        }.otherwise{
          reqInLine := reqInLine - 1.U
        }
        when(tail + 1.U === BSPQEntry.U){
          tail := 0.U
        }.otherwise{
          tail := tail + 1.U
        }
        khkw(tail)(0) := khi
    	  khkw(tail)(1) := kwi
        data_in_queue(tail) := true.B
      }
    }
    

    when(io.readData0.valid){
      when(lineReqNum >= 1.U){
        for{
    		  i <- 0 until BSPQEntry
      		j <- 0 until BICBNum*PEWidth/dataNum
      	} yield {
      		when(req_inflight(i)(j) && req_id(i)(j)===io.readData0.bits.id){
    	  		for{
    		  		p <- 0 until dataNum/PEWidth
    			  	q <- 0 until PEWidth
      			} yield {
      				data(i)(j*dataNum/PEWidth+p)(q) := io.readData0.bits.data(p*PEWidth+q)
      			}
    	  		req_inflight(i)(j) := false.B
    		  }
    	  }
      }.otherwise{
        for(i <- 0 until BSPQEntry){
          when(req_inflight(i)(0) && req_id(i)(0)===io.readData0.bits.id){
            req_inflight(i)(0) := false.B
            for(k <- 0 until PEWidth) {
              data(i)(0)(k) := io.readData0.bits.data(req_w2spi(i)(0)+k.U)
            }
            when(icb === 2.U){
              for(k <- 0 until PEWidth) {
                data(i)(1)(k) := io.readData0.bits.data(req_w2spi(i)(0)+PEWidth.U+k.U)
              }
            }
          }
        }
      }
    }

    when(io.readData1.valid){
      when(lineReqNum >= 1.U){
        for{
    		  i <- 0 until BSPQEntry
      		j <- 0 until BICBNum*PEWidth/dataNum
      	} yield {
      		when(req_inflight(i)(j) && req_id(i)(j)===io.readData1.bits.id){
    	  		for{
    		  		p <- 0 until dataNum/PEWidth
    			  	q <- 0 until PEWidth
      			} yield {
      				data(i)(j*dataNum/PEWidth+p)(q) := io.readData1.bits.data(p*PEWidth+q)
      			}
    	  		req_inflight(i)(j) := false.B
    		  }
    	  }
      }.otherwise{
        for(i <- 0 until BSPQEntry){
          when(req_inflight(i)(0) && req_id(i)(0)===io.readData1.bits.id){
            req_inflight(i)(0) := false.B
            for(k <- 0 until PEWidth) {
              data(i)(0)(k) := io.readData1.bits.data(req_w2spi(i)(0)+k.U)
            }
            when(icb === 2.U){
              for(k <- 0 until PEWidth) {
                data(i)(1)(k) := io.readData1.bits.data(req_w2spi(i)(0)+PEWidth.U+k.U)
              }
            }
          }
        }
      }
    }
 	  
    io.bufferRead.data.bits := data(0)
    io.bufferRead.data.valid := false.B
    buffer_req_critical := io.bufferRead.v
    for(i <- 0 until BSPQEntry){
    	when(io.bufferRead.v && khkw(i)(0) === io.bufferRead.kh && khkw(i)(1) === io.bufferRead.kw && 
           data_in_queue(i) && !req_inflight(i).reduce(_|_)){
    		io.bufferRead.data.bits := data(i)
    		io.bufferRead.data.valid := true.B
    	}
      when(io.bufferRead.v && khkw(i)(0) === io.bufferRead.kh && khkw(i)(1) === io.bufferRead.kw && 
           (data_in_queue(i)||req_inflight(i).reduce(_|_))){
        buffer_req_critical := false.B
      }
    }

    when(io.bufferRead.v && khkw(head)(0)===io.bufferRead.kh && khkw(head)(1)===io.bufferRead.kw &&
         data_in_queue(head) && !req_inflight(head).reduce(_|_)){
      when(sizeenough && io.bufferRead.lastoh && io.bufferRead.lastow && io.bufferRead.lastohb){
        when(head + 1.U === BSPQEntry.U){
          head := 0.U
        }.otherwise{
          head := head + 1.U
        }
        data_in_queue(head) := false.B
      }
      when(!sizeenough && io.bufferRead.lastohb){
        when(head + 1.U === BSPQEntry.U){
          head := 0.U
        }.otherwise{
          head := head + 1.U
        }
        data_in_queue(head) := false.B
      }
    }
//    printf(p"io.bufferRead.v ${io.bufferRead.v} kh ${io.bufferRead.kh} kw ${io.bufferRead.kw} lastoh ${io.bufferRead.lastoh} lastow ${io.bufferRead.lastow} lastohb ${io.bufferRead.lastohb}\n")
//    printf(p"BSCratchpad head $head tail $tail\n")
//    printf(p"data_in_queue $data_in_queue\n req_inflight $req_inflight\n")

    //循环计数
//    printf(p"BSP2MEM oci $oci ici $ici ohi $ohi owi $owi khi $khi kwi $kwi reqInLine $reqInLine\n")
    when(running && ((io.readMem.fire() && reqLineLast) || (lineReqNum === 0.U && reqInLine=/=0.U && data_in_queue_num<kh*kw))){
      when(kwi+1.U===kw){
        kwi := 0.U
      }.otherwise{
        kwi := kwi + 1.U
      }

      when(kwi+1.U===kw){
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

      when((kwi+1.U===kw && khi+1.U===kh && sizeenough)||
           (kwi+1.U===kw && khi+1.U===kh && !sizeenough && owi+PEHigh.U===ow && ohi+ohb>=oh)){
        when(ici+1.U===ic){
          ici := 0.U
        }.otherwise{
          ici := ici +1.U
        }
//        printf(p"BScratchpad ici $ici\n")
      }

      when(kwi+1.U===kw && khi+1.U===kh && !sizeenough){
        when(owi+PEHigh.U===ow){
          owi := 0.U
        }.otherwise{
          owi := owi + PEHigh.U
        }
//        printf(p"BScratchpad owi $owi\n")
      }

      when(kwi+1.U===kw && khi+1.U===kh && owi+PEHigh.U===ow && !sizeenough){
        when(ohi+ohb>=oh){
          ohi := 0.U
        }.otherwise{
          ohi := ohi + ohb
        }
//        printf(p"BScratchpad ohi $ohi\n")
      }

      when((kwi+1.U===kw && khi+1.U===kh && sizeenough && ici+1.U===ic) ||
           (kwi+1.U===kw && khi+1.U===kh && !sizeenough && owi+PEHigh.U===ow && ohi+ohb>=oh && ici+1.U===ic)){
        when(oci+ocPENum.U<oc){
          oci := oci + ocPENum.U
        }.otherwise{
          running := false.B
          printf(p"BScratchpad complete\n")
        }
        printf(p"BScratchpad oci $oci\n")
      }
    }
//  printf(p"BSP2MEM valid ${io.readMem.valid} oci $oci ici $ici ohi $ohi owi $owi khi $khi kwi $kwi\n")
//  printf(p"data_in_queue_num ${data_in_queue_num} data_in_queue ${data_in_queue}\n")
//  printf(p"req_inflight ${req_inflight}\n")
//  printf(p"head ${head} tail ${tail} khkw ${khkw} reqInLine ${reqInLine}\n")
//  printf("-----------------------------\n")
}
