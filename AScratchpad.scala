package boom.acc

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import boom.exu.ygjk._

class AScratchpad extends Module with YGJKParameters with HWParameters{
    val io = IO(new Bundle{
        val config = new ConfigPEIO

        val bufferRead = Vec(PEHigh, new Bundle{
          val v = Input(Bool())
          val ih = Input(UInt(ohWidth.W))
          val iw = Input(UInt(owWidth.W))
          val data = Valid(Vec(ALineDNum, UInt(ALineDWidth.W)))
          val lasth = Input(Bool())
          val lastw = Input(Bool())
        })

        val readMem = DecoupledIO(new Bundle{
            val vaddr = UInt(addrWidth.W)
            val req_state = UInt(log2Ceil(ASPIWEntry).W)
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
    })

    val data = RegInit(VecInit.tabulate(ASPIHEntry, ASPIWEntry, ALineDNum){(x,y,z) => 0.U(dataWidth.W)})
    val req_id = RegInit(VecInit.tabulate(ASPIHEntry, ASPIWEntry){(x,y) => 0.U((log2Ceil(sourceNum+1)+1).W)})
    val req_w2spi = RegInit(VecInit.tabulate(ASPIHEntry, ASPIWEntry){(x, y) => 0.U(log2Ceil(dataNum).W)}) //在取回来的一行中的偏移
    val req_inflight = RegInit(VecInit.tabulate(ASPIHEntry, ASPIWEntry){(x,y) => false.B})
    val in_queue = RegInit(VecInit.tabulate(ASPIHEntry, ASPIWEntry){(x,y) => false.B})  //在队列中
    val data_received = RegInit(VecInit.tabulate(ASPIHEntry, ASPIWEntry){(x,y) => false.B})  //收到了数据，不一定是在队列中，可能是队列中的行收到了多出来的将要用到的数据
    val use = Wire(Vec(ASPIHEntry, Vec(ASPIWEntry, Bool())))
    for{
        i <- 0 until ASPIHEntry
        j <- 0 until ASPIWEntry
      } yield {
        use(i)(j) := req_inflight(i)(j) || data_received(i)(j)
    }

    val firstAddr = RegInit(0.U(addrWidth.W))
    val head = RegInit(VecInit(Seq.fill(ASPIHEntry)(0.U(log2Ceil(ASPIWEntry).W)))) //每个ih列都有一个队列指针
    val tail = RegInit(VecInit(Seq.fill(ASPIHEntry)(0.U(log2Ceil(ASPIWEntry).W))))

    //接收总线发来的数据，然后处理对齐问题，分次写入SCratchpad
    val memBuffer = RegInit(VecInit(Seq.fill(dataNum)(0.U(ADWidth.W))))
    val w2spV = RegInit(VecInit(Seq.fill(dataNum)(0.U(log2Ceil(ASPIHEntry).W))))   //收到数据的时候V置为true开始写入SCratchpad
    val w2spIH = RegInit(VecInit(Seq.fill(dataNum)(0.U(log2Ceil(ASPIHEntry).W))))
    val w2spIW = RegInit(VecInit(Seq.fill(dataNum)(0.U(log2Ceil(ASPIWEntry).W))))
    
    for(i <- 0 until ASPIHEntry){
//      printf(p"req_inflight ${req_inflight(i)}\nreq_w2spi ${req_w2spi(i)}\nin_queue ${in_queue(i)}\ndata_received ${data_received(i)}\n")
    }
//    printf(p"-----------------------\n")

    val oc = RegInit(0.U(ocWidth.W))
    val ic = RegInit(0.U(icWidth.W))
    val ih = RegInit(0.U(ohWidth.W))
    val iw = RegInit(0.U(owWidth.W))
    val ohb = RegInit(0.U(ohbWidth.W))  //OH_BLOCK
    val icb = RegInit(0.U(icWidth.W))
    val kh = RegInit(0.U(khWidth.W))
    val kw = RegInit(0.U(kwWidth.W))
    val strideH = RegInit(0.U(strideWidth.W))
    val strideW = RegInit(0.U(strideWidth.W))
    val paddingH = RegInit(0.U(paddingWidth.W))
    val paddingW = RegInit(0.U(paddingWidth.W))
    val oh = RegInit(0.U(ohWidth.W))
    val ow = RegInit(0.U(owWidth.W))
 //   val oh = (ih-kh)/strideH + 1.U + (paddingH<<1.U) 
 //   val ow = (iw-kw)/strideW + 1.U + (paddingW<<1.U) 

//    printf(p"ASCratchpad oc $oc ic $ic ih $ih iw $iw ohb $ohb icb $icb kh $kh kw $kw strideH $strideH strideW $strideW paddingH $paddingH paddingW $paddingW\n")
    
    val oci = RegInit(0.U(ocWidth.W))
    val ici = RegInit(0.U(icWidth.W))
    val ohi = RegInit(0.U(ohWidth.W))  //每次加ohb
    val owi = RegInit(0.U(owWidth.W))  //每次加PEHigh(OW_BLOCK)
    val iwi = RegInit(0.U(ohbWidth.W))  //输入数据iw维下标
    val ihi = RegInit(0.U(ohbWidth.W))  //输入数据ih维下标
    val khi = RegInit(0.U(khWidth.W))
    val kwi = RegInit(0.U(kwWidth.W))
    val IW_B = RegInit(0.U(owWidth.W))  //控制对外访存的循环次数
    val IH_B = RegInit(0.U(ohbWidth.W))
    val ihbi = RegInit(0.U(ohbWidth.W))  //内循环中记录ih循环起始值
    val iwbi = RegInit(0.U(ohbWidth.W))

//保存当前缓存中的ih和iw的0位置下标，因为可能头尾不在同一个块中，需要保存两个基础下标
    val ih_base_head = RegInit(0.U(ohbWidth.W))  //正在计算的ih维的缓存0列数组下标
    val ih_base_tail = RegInit(0.U(ohbWidth.W))  //正在访存的ih维的缓存0列数组下标
    val ihIndex = RegInit(0.U(ohWidth.W))    //有效的ih维头
    val ihStrideIndex = RegInit(0.U(ohWidth.W))  //当strideH不是1的时候，ih维的无效是不连续的需要记录下来
    val ihBlock = RegInit(0.U(ohbWidth.W))    //ih维将要使用的总列数
    val ic_head = RegInit(0.U(icWidth.W))     //在ih小于一次ohb需要的h维数据时用来判断预取的数据是否被覆盖
    val ic_tail = RegInit(0.U(icWidth.W))

    val running = RegInit(false.B)

    when(io.config.start){
      oci := 0.U
      ici := 0.U
      ohi := 0.U
      owi := 0.U
      ihbi := 0.U
      iwbi := 0.U
      khi := 0.U
      kwi := 0.U
      ihi := 0.U
      iwi := 0.U
//      IW_B := Mux(strideW===2.U && kw===1.U, PEHigh.U*2.U, (PEHigh.U-1.U)*strideW+kw-paddingW)
      IW_B := Mux(icb===1.U, 16.U, Mux(icb===2.U, 8.U, PEHigh.U*strideH))
      IH_B := Mux(strideW===2.U && kw===1.U, ohb*2.U, Mux(ih>(ohb-1.U)*strideH+kh-paddingH, (ohb-1.U)*strideH+kh-paddingH, ih))
      ihBlock := Mux(ih>(ohb-1.U)*strideH+kh-paddingH, (ohb-1.U)*strideH+kh-paddingH, ih)
      running := true.B
//      printf(p"start!!!\n")
    }

//    io.config.idle := !running
//    io.config.idle := oci*ocPENum.U*PEWidth.U >= oc
//    printf(p"oci $oci oc $oc\n")
//    printf(p"AScratchpad run ${!io.config.idle}\n")

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
    when(io.config.strideW.valid){
      strideW := io.config.strideW.bits
    }
    when(io.config.strideH.valid){
      strideH := io.config.strideH.bits
    }
    when(io.config.Aaddr.valid){
      firstAddr := io.config.Aaddr.bits
    }
    when(io.config.paddingH.valid){
      paddingH := io.config.paddingH.bits
    }
    when(io.config.paddingW.valid){
      paddingW := io.config.paddingW.bits
    }

//发出访存请求
//    val curAddr = firstAddr + ((ici*ih*iw*icb + (ih_base_tail+ihbi)*iw*icb + (iw_base_tail+iwbi)*icb) << log2Ceil(ADWidth.U>>3.U))
    val curAddr = firstAddr + ((ici*ih*iw*icb + ihi*iw*icb + iwi*icb) << 2.U)
    //防止预取覆盖正在计算的数据
    val pre_no_cover = (ih_base_head===ih_base_tail && ic_head===ic_tail && ihi-ih_base_tail>=ihIndex) ||    
                        ((ih_base_head=/=ih_base_tail || ic_head=/=ic_tail)&& ihi-ih_base_tail<ihIndex)
//    io.readMem.valid := !io.config.idle && tail(ihbi) + 1.U =/= head(ihbi) && (tail(ihbi) =/= (head(ihbi)+ASPIWEntry.U-1.U))
/*
    io.readMem.valid := !data_received(ihi-ih_base_tail)(iwi(log2Ceil(ASPIWEntry)-1, 0)) &&
                        !req_inflight(ihi-ih_base_tail)(iwi(log2Ceil(ASPIWEntry)-1, 0)) && running && 
                        Mux(icb===1.U, PopCount(use(ihi-ih_base_tail)) < ASPIWEntry.U - dataNum.U, 
                          Mux(icb===2.U, PopCount(use(ihi-ih_base_tail)) < ASPIWEntry.U - (dataNum/2).U, 
                              PopCount(use(ihi-ih_base_tail)) < ASPIWEntry.U - (dataNum/ALineDNum).U)) //&& pre_no_cover
 */
    io.readMem.valid := !(IH_B===ih && ih_base_tail=/=ih_base_head) && Mux(icb===4.U, !in_queue(ihi-ih_base_tail)((iwi+3.U)(log2Ceil(ASPIWEntry)-1, 0)) && 
                                       !in_queue(ihi-ih_base_tail)((iwi+2.U)(log2Ceil(ASPIWEntry)-1, 0)), 
                            Mux(icb===2.U, !in_queue(ihi-ih_base_tail)((iwi+7.U)(log2Ceil(ASPIWEntry)-1, 0)) &&
                                           !in_queue(ihi-ih_base_tail)((iwi+6.U)(log2Ceil(ASPIWEntry)-1, 0)),
                              !in_queue(ihi-ih_base_tail)((iwi+15.U)(log2Ceil(ASPIWEntry)-1, 0)) &&
                              !in_queue(ihi-ih_base_tail)((iwi+14.U)(log2Ceil(ASPIWEntry)-1, 0)))) && running && 
                        Mux(icb===1.U, PopCount(use(ihi-ih_base_tail)) < ASPIWEntry.U - dataNum.U, 
                          Mux(icb===2.U, PopCount(use(ihi-ih_base_tail)) < ASPIWEntry.U - (dataNum/2).U, 
                              PopCount(use(ihi-ih_base_tail)) < ASPIWEntry.U - (dataNum/ALineDNum).U))

    io.readMem.bits.vaddr := Cat(curAddr(addrWidth-1, log2Ceil(ygjk_memWidth)), 0.U(log2Ceil(ygjk_memWidth).W))   //发出对齐的地址
//    io.readMem.bits.req_state := Mux(tail(ihbi)>=head(ihbi), tail(ihbi)-head(ihbi), tail(ihbi)+ASPIWEntry.U-head(ihbi))  //队列中数据行数
    val buffer_req_critical = (io.bufferRead zip io.bufferRead).map{case(h,w)=>h.v && !in_queue(h.ih-ih_base_tail)(w.iw(log2Ceil(ASPIWEntry)-1, 0))}.reduce(_|_)
    io.readMem.bits.req_state := Mux(buffer_req_critical, 0.U, PopCount(in_queue(ihi-ih_base_tail)))

//    printf(p"ih_base_tail $ih_base_tail ih_base_head $ih_base_head ihIndex $ihIndex\n")
//队列维护--访存入队
    when(!in_queue(ihi-ih_base_tail)(iwi(log2Ceil(ASPIWEntry)-1, 0)) && running && 
          (io.readMem.fire() || req_inflight(ihi-ih_base_tail)(iwi(log2Ceil(ASPIWEntry)-1, 0)) || data_received(ihi-ih_base_tail)(iwi(log2Ceil(ASPIWEntry)-1, 0)))){
//      in_queue(ihi-ih_base_tail)(iwi(log2Ceil(ASPIWEntry)-1, 0)) := true.B
      when(tail(ihi-ih_base_tail)+1.U===ASPIWEntry.U){
        tail(ihi-ih_base_tail) := 0.U
      }.otherwise{
        tail(ihi-ih_base_tail) := tail(ihi-ih_base_tail) + 1.U
      }
    }

    when(running && io.readMem.fire()){
      req_id(ihi-ih_base_tail)(iwi(log2Ceil(ASPIWEntry)-1, 0)) := io.req_id
      req_inflight(ihi-ih_base_tail)(iwi(log2Ceil(ASPIWEntry)-1, 0)) := true.B
      in_queue(ihi-ih_base_tail)(iwi(log2Ceil(ASPIWEntry)-1, 0)) := true.B
      req_w2spi(ihi-ih_base_tail)(iwi(log2Ceil(ASPIWEntry)-1, 0)) := curAddr(log2Ceil(ygjk_memWidth)-1, log2Ceil(ADWidth>>3))
      when(icb===1.U){
        for(i <- 1 until dataNum){
          when(iwi < iw - i.U && curAddr(log2Ceil(ygjk_memWidth)-1, log2Ceil(ADWidth>>3)) < dataNum.U - i.U ){
            req_id(ihi-ih_base_tail)((iwi+i.U)(log2Ceil(ASPIWEntry)-1, 0)) := io.req_id
            req_inflight(ihi-ih_base_tail)((iwi+i.U)(log2Ceil(ASPIWEntry)-1, 0)) := true.B
            in_queue(ihi-ih_base_tail)((iwi+i.U)(log2Ceil(ASPIWEntry)-1, 0)) := true.B
            req_w2spi(ihi-ih_base_tail)((iwi+i.U)(log2Ceil(ASPIWEntry)-1, 0)) := curAddr(log2Ceil(ygjk_memWidth)-1, log2Ceil(ADWidth>>3)) + i.U
          }
        }
      }.elsewhen(icb===2.U){
        for(i <- 1 until dataNum/2){
          when(iwi < iw - i.U && curAddr(log2Ceil(ygjk_memWidth)-1, log2Ceil(ADWidth>>3)) < dataNum.U - (i<<1).U ){
            req_id(ihi-ih_base_tail)((iwi+i.U)(log2Ceil(ASPIWEntry)-1, 0)) := io.req_id
            req_inflight(ihi-ih_base_tail)((iwi+i.U)(log2Ceil(ASPIWEntry)-1, 0)) := true.B
            in_queue(ihi-ih_base_tail)((iwi+i.U)(log2Ceil(ASPIWEntry)-1, 0)) := true.B
            req_w2spi(ihi-ih_base_tail)((iwi+i.U)(log2Ceil(ASPIWEntry)-1, 0)) := curAddr(log2Ceil(ygjk_memWidth)-1, log2Ceil(ADWidth>>3)) + (i<<1).U
          }
        }
      }.otherwise{ //icb只能是1、2或者ALineDNum
        for(i <- 1 until dataNum/ALineDNum){
          when(iwi < iw - i.U && curAddr(log2Ceil(ygjk_memWidth)-1, log2Ceil(ADWidth>>3)) < dataNum.U - (i<<log2Ceil(ALineDNum)).U){
            req_id(ihi-ih_base_tail)((iwi+i.U)(log2Ceil(ASPIWEntry)-1, 0)) := io.req_id
            req_inflight(ihi-ih_base_tail)((iwi+i.U)(log2Ceil(ASPIWEntry)-1, 0)) := Mux(kw===1.U && strideW===2.U && i.U(0)===1.U, false.B, true.B)
            in_queue(ihi-ih_base_tail)((iwi+i.U)(log2Ceil(ASPIWEntry)-1, 0)) := Mux(kw===1.U && strideW===2.U && i.U(0)===1.U, false.B, true.B)
            req_w2spi(ihi-ih_base_tail)((iwi+i.U)(log2Ceil(ASPIWEntry)-1, 0)) := curAddr(log2Ceil(ygjk_memWidth)-1, log2Ceil(ADWidth>>3)) + (i<<log2Ceil(ALineDNum)).U
          }
        }
      }
    }

//    printf(p"ASCratchpad iw head ${head} ")
//    printf(p"ASCratchpad iw tail ${tail}\n")
//    printf(p"ASCratchpad data_v ${data_v}\n")
    when(io.readData0.valid){
      for{
        i <- 0 until ASPIHEntry
        j <- 0 until ASPIWEntry
      } yield {
        when(req_inflight(i)(j) && req_id(i)(j)===io.readData0.bits.id){
          when(icb===1.U){
            req_inflight(i)(j) := false.B
            data_received(i)(j) := true.B
            data(i)(j)(0) := io.readData0.bits.data(req_w2spi(i)(j))
          }.otherwise{
            req_inflight(i)(j) := false.B
            data_received(i)(j) := true.B
            for(p <- 0 until ALineDNum){
              data(i)(j)(p) := io.readData0.bits.data(req_w2spi(i)(j)+p.U)
            }    
          }
        }
      }
    }

    when(io.readData1.valid){
      for{
        i <- 0 until ASPIHEntry
        j <- 0 until ASPIWEntry
      } yield {
        when(req_inflight(i)(j) && req_id(i)(j)===io.readData1.bits.id){
          when(icb===1.U){
            req_inflight(i)(j) := false.B
            data_received(i)(j) := true.B
            data(i)(j)(0) := io.readData1.bits.data(req_w2spi(i)(j))
          }.otherwise{
            req_inflight(i)(j) := false.B
            data_received(i)(j) := true.B
            for(p <- 0 until ALineDNum){
              data(i)(j)(p) := io.readData1.bits.data(req_w2spi(i)(j)+p.U)
            }    
          }
        }
      }
    }

//循环计数
//    printf(p"ASP2MEM ici  $ici ihi  ${ihi}  iwi ${iwi}  iwbi $iwbi IW_B ${IW_B} ihbi $ihbi IH_B ${IH_B}\n")
//    printf(p"loop $running ${io.readMem.fire()} ${req_inflight(ihi-ih_base_tail)(iwi(log2Ceil(ASPIWEntry)-1, 0))} ${data_received(ihi-ih_base_tail)(iwi(log2Ceil(ASPIWEntry)-1, 0))}\n")
//    when(!in_queue(ihi-ih_base_tail)(iwi(log2Ceil(ASPIWEntry)-1, 0)) && running /*&& pre_no_cover*/ && (io.readMem.fire() /*|| req_inflight(ihi-ih_base_tail)(iwi(log2Ceil(ASPIWEntry)-1, 0)) || data_received(ihi-ih_base_tail)(iwi(log2Ceil(ASPIWEntry)-1, 0))*/)){
    when(!in_queue(ihi-ih_base_tail)(iwi(log2Ceil(ASPIWEntry)-1, 0)) && running && io.readMem.fire()){
      when(iwi + Mux(icb===4.U, 4.U, Mux(icb===2.U, 8.U, 16.U)) === IW_B){    //最内层循环结束（iw维），iwi停止加1操作
        when(ihi + 1.U === IH_B || (kh===1.U && strideH===2.U && ihi+strideH>=IH_B)){ 
          when(IW_B === iw || (kw===1.U && strideW===2.U && IW_B + 1.U >= iw)){
            iwi := 0.U
          }.otherwise {
            iwi := IW_B
          }
        }.otherwise{
          iwi := iwbi
        }
      }.otherwise{
        when(icb===1.U){
          iwi := iwi + dataNum.U
        }.elsewhen(icb===2.U){
          iwi := iwi + 8.U
        }.otherwise{
          iwi := iwi + 4.U
        }  
      }

      when((ihi + 1.U === IH_B || (kh===1.U && strideH===2.U && ihi+strideH>=IH_B)) && 
            iwi + Mux(icb===4.U, 4.U, Mux(icb===2.U, 8.U, 16.U)) === IW_B){ //iwbi记录iwi循环时候的起始下标,iW_B是内循环的终止下标
        when(IW_B === iw || (kw===1.U && strideW===2.U && IW_B + 1.U >= iw)){
          iwbi := 0.U
        }.otherwise {
          iwbi := IW_B
        }
        when(icb===1.U){
          IW_B := Mux(owi+1.U===ow/PEHigh.U || IW_B === iw, 16.U, Mux(iw-IW_B>16.U, IW_B+16.U, iw))
        }.elsewhen(icb===2.U){
          IW_B := Mux(owi+1.U===ow/PEHigh.U || IW_B === iw, 8.U, Mux(iw-IW_B>8.U, IW_B+8.U, iw))
        }.otherwise{
          IW_B := Mux(owi+1.U===ow/PEHigh.U || IW_B === iw, PEHigh.U*strideH, Mux(iw-IW_B>PEHigh.U*strideH, IW_B+PEHigh.U*strideH, iw))
        }
//        IW_B := Mux(owi+1.U===ow/PEHigh.U, Mux(kw===1.U && strideW===2.U, PEHigh.U*2.U, (PEHigh.U-1.U)*strideW+kw-paddingW), Mux(iw-IW_B>PEHigh.U*strideH, IW_B+PEHigh.U*strideH, iw))
      }

      when(iwi + Mux(icb===4.U, 4.U, Mux(icb===2.U, 8.U, 16.U)) === IW_B){     //最内存循环每结束一次ihi会有一次变化
        when(ihi + 1.U === IH_B || (kh===1.U && strideH===2.U && ihi+strideH>=IH_B)){
          when(owi+1.U===ow/PEHigh.U || IW_B === iw){
            ihi := Mux(IH_B === ih, 0.U, (ohi+ohb)*strideH-paddingH)
          }.otherwise{
            ihi := ihbi
          }
        }.otherwise{
          when(kh===1.U && strideH===2.U){
            ihi := ihi + 2.U
          }.otherwise{
            ihi := ihi + 1.U
          }
        }
      }

      when((owi+1.U===ow/PEHigh.U || IW_B === iw)&& 
            (ihi + 1.U === IH_B || (kh===1.U && strideH===2.U && ihi+strideH>=IH_B)) && 
            iwi + Mux(icb===4.U, 4.U, Mux(icb===2.U, 8.U, 16.U)) === IW_B){  //更新ih维的循环起点和终点
        ihbi := Mux(IH_B===ih, 0.U, (ohi+ohb)*strideH-paddingH)
        IH_B := Mux(ohi+ohb>=oh, Mux(ih>(ohb-1.U)*strideH+kh-paddingH, 
                                  Mux(strideW===2.U && kw===1.U, ohb*2.U, (ohb-1.U)*strideH+kh-paddingH), ih), 
                                  Mux(ih>IH_B+ohb*strideH, IH_B+ohb*strideH, ih))
        ih_base_tail := Mux(IH_B===ih, 0.U, (ohi+ohb)*strideH-paddingH)
        ic_tail := Mux(ohi+ohb>=oh, Mux(ici+1.U===ic, 0.U, ici+1.U), ic_tail)
      }

      when((ihi + 1.U === IH_B || (kh===1.U && strideH===2.U && ihi+strideH>=IH_B)) && 
           iwi + Mux(icb===4.U, 4.U, Mux(icb===2.U, 8.U, 16.U)) === IW_B){  //owi层循环变量
        when(owi + 1.U === ow/PEHigh.U || IW_B === iw){
          owi := 0.U
        }.otherwise{
          owi := owi + 1.U
        }
//        printf(p"AScratchpad owi $owi\n")
      }

      when((owi+1.U===ow/PEHigh.U || IW_B === iw)&& 
            (ihi + 1.U === IH_B || (kh===1.U && strideH===2.U && ihi+strideH>=IH_B)) && 
            iwi + Mux(icb===4.U, 4.U, Mux(icb===2.U, 8.U, 16.U)) === IW_B){  //ohi层循环
        when(ohi+ohb>=oh){
          ohi := 0.U
        }.otherwise{
          ohi := ohi + ohb
        }
//        printf(p"AScratchpad ohi $ohi\n")
        when(ohi+ohb>=oh){ //ici层循环
          when(ici + 1.U === ic){
            ici := 0.U
          }.otherwise{
            ici := ici + 1.U
          }
//          printf(p"AScratchpad ici $ici\n")        
        }

        when(ohi+ohb>=oh && ici + 1.U === ic){
          printf(p"AScratchpad oci $oci\n")
          when(oci+ocPENum.U < oc){      //最后一层循环达到最大值
            oci := oci + ocPENum.U
          }.otherwise{
            running := false.B
            printf(p"AScratchpad complete\n")
          }
        }
      }
    }


//队列维护-Abuffer读数
//    printf(p"io.bufferRead ${io.bufferRead}\n")
//    printf(p"----------------------------------\n")
    val readenough = ((io.bufferRead zip io.bufferRead).map{case(v,data)=> (v.v===false.B ||v.v===data.data.valid)}.reduce(_&_)) && io.bufferRead.map(_.v).reduce(_|_)
    for(i <- 0 until PEHigh){
      io.bufferRead(i).data.valid := false.B
      io.bufferRead(i).data.bits := data(0)(0)
      val ih_read = io.bufferRead(i).ih - ih_base_head
      val iw_read = io.bufferRead(i).iw(log2Ceil(ASPIWEntry)-1, 0)

      when(io.bufferRead(i).v && in_queue(ih_read)(iw_read) && data_received(ih_read)(iw_read)){
        io.bufferRead(i).data.valid := true.B
        io.bufferRead(i).data.bits := data(ih_read)(iw_read)

        when(io.bufferRead(i).lastw && readenough){
          data_received(ih_read)(iw_read) := false.B
          in_queue(ih_read)(iw_read) := false.B
        }

        when(io.bufferRead(i).lasth && io.bufferRead(i).lastw && (io.bufferRead(i).iw+1.U===iw || kw===1.U && io.bufferRead(i).iw+strideW>=iw ) && readenough){  //当前列不再使用
          head(ih_read) := 0.U
          when(kh===1.U && strideH===2.U){
            when(ihIndex+strideH>=ihBlock){
              ihBlock := IH_B - ih_base_tail
              ihIndex := 0.U
              ih_base_head := ih_base_tail
              ic_head := ic_tail
            }.otherwise{
              ihIndex := ihIndex + 2.U
            }
          }.otherwise{
            when(ihIndex+1.U+ihStrideIndex===ihBlock){
              ihIndex := 0.U
              ih_base_head := ih_base_tail
              ic_head := ic_tail
              ihBlock := IH_B - ih_base_tail
              ihStrideIndex := 0.U
            }.elsewhen(strideH=/=1.U && ih_read =/= ihIndex){
              ihStrideIndex := ihStrideIndex + 1.U
            }.otherwise{
              ihIndex := ihIndex + 1.U + ihStrideIndex
              ihStrideIndex := 0.U
            }
          }

        }.elsewhen(io.bufferRead(i).lastw && readenough){
          when(iw_read + 1.U === ASPIWEntry.U){
            head(ih_read) := 0.U
          }.otherwise{
            head(ih_read) := iw_read + 1.U     //会有多有lastw，要保证最后更新的head指针值是对的
          }
        }
      }
    }
}
