package boom.acc

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import boom.exu.ygjk._

class Withacc_CONV extends Config((site,here,up) => {  
    case BuildYGAC =>
        (p:Parameters) => {          
            val myAccel = Module(new conv)
            myAccel
        }
    }
)

class conv extends MyACCModule with HWParameters with YGJKParameters{
    
    val PEs = VecInit(Seq.fill(ocPENum)(Module(new PE).io))
    val Abuffer = VecInit(Seq.fill(ocPENum)(Module(new PEAbuffer).io))
    val Abroadcast = Module(new A2PEbroadcast).io
    val AScratchpad = Module(new AScratchpad).io
    val Bbuffer = VecInit(Seq.fill(ocPENum)(Module(new PEBbuffer).io))
    val Bbroadcast = VecInit(Seq.fill(ocPENum)(Module(new B2PEbroadcast).io))
    val BScratchpad = VecInit(Seq.fill(ocPENum)(Module(new BScratchpad).io))
    val Cbuffer = VecInit(Seq.fill(ocPENum)(Module(new Cbuffer).io))
    val C2PE = VecInit(Seq.fill(ocPENum)(Module(new C2PE).io))
    val CScratchpad = VecInit(Seq.fill(ocPENum)(Module(new CScratchpad).io))
//    printf(p"---------------------------\n")

//    io.ctl.acc_running := !(AScratchpad.config.idle && 
//                            BScratchpad.map(_.config.idle).reduce(_&_) && 
//                            CScratchpad.map(_.config.idle).reduce(_&_))
//    when(io.ctl.acc_running===false){
//      printf(p"convTOP idle!\n")
//    }


    val config_i = RegInit(0.U(4.W))
    val running = RegInit(false.B)

    io.ctl.acc_running := running

    val oc_v = RegInit(false.B)
    val ic_v = RegInit(false.B)
    val ih_v = RegInit(false.B)
    val iw_v = RegInit(false.B)
    val oh_v = RegInit(false.B)
    val ow_v = RegInit(false.B)
    val kh_v = RegInit(false.B)
    val kw_v = RegInit(false.B)
    val ohb_v = RegInit(false.B)
    val icb_v = RegInit(false.B)
    val paddingH_v = RegInit(false.B)
    val paddingW_v = RegInit(false.B)
    val strideH_v = RegInit(false.B)
    val strideW_v = RegInit(false.B)
    val Aaddr_v = RegInit(false.B)
    val Baddr_v = RegInit(false.B)
    val Caddr_v = RegInit(false.B)
    val dataType_v = RegInit(false.B)

    val oc = RegInit(0.U(ocWidth.W))
    val ic = RegInit(0.U(icWidth.W))
    val ih = RegInit(0.U(ohWidth.W))
    val iw = RegInit(0.U(owWidth.W))
    val oh = RegInit(0.U(ohWidth.W))
    val ow = RegInit(0.U(owWidth.W))
    val ohb = RegInit(0.U(ohbWidth.W))
    val icb = RegInit(0.U(icWidth.W))
    val paddingH = RegInit(0.U(paddingWidth.W))
    val paddingW = RegInit(0.U(paddingWidth.W))
    val strideH = RegInit(0.U(strideWidth.W))
    val strideW = RegInit(0.U(strideWidth.W))
    val kh = RegInit(0.U(khWidth.W))
    val kw = RegInit(0.U(kwWidth.W))
    val Aaddr = RegInit(0.U(addrWidth.W))
    val Baddr = RegInit(0.U(addrWidth.W))
    val Caddr = RegInit(0.U(addrWidth.W))
    val dataType = RegInit(0.U(3.W))

    when(config_i === 0.U && io.ctl.config.valid){
      running := true.B
      Aaddr_v := true.B
      Aaddr := io.ctl.config.bits.cfgData1
      ic_v := true.B
      ic := io.ctl.config.bits.cfgData2   //除以icb后的值
      config_i := 1.U
      printf(p"convTOP\n Aaddr ${io.ctl.config.bits.cfgData1} ic ${io.ctl.config.bits.cfgData2}")
    }.elsewhen(config_i === 1.U && io.ctl.config.valid){
      ih_v := true.B
      ih := io.ctl.config.bits.cfgData1
      iw_v := true.B
      iw := io.ctl.config.bits.cfgData2
      config_i := 2.U
      printf(p"ih ${io.ctl.config.bits.cfgData1} iw ${io.ctl.config.bits.cfgData2}")
    }.elsewhen(config_i === 2.U && io.ctl.config.valid){
      strideH_v := true.B
      strideH := io.ctl.config.bits.cfgData1
      strideW_v := true.B
      strideW := io.ctl.config.bits.cfgData2
      config_i := 3.U
      printf(p"strideH ${io.ctl.config.bits.cfgData1} strideW ${io.ctl.config.bits.cfgData2}")
    }.elsewhen(config_i === 3.U && io.ctl.config.valid){
      icb_v := true.B
      icb := io.ctl.config.bits.cfgData1
      Baddr_v := true.B
      Baddr := io.ctl.config.bits.cfgData2
      config_i := 4.U
      printf(p"icb ${io.ctl.config.bits.cfgData1} Baddr ${io.ctl.config.bits.cfgData2}")
    }.elsewhen(config_i === 4.U && io.ctl.config.valid){
      oc_v := true.B
      oc := io.ctl.config.bits.cfgData1   //除以ocb后的值
      kh_v := true.B
      kh := io.ctl.config.bits.cfgData2
      config_i := 5.U
      printf(p"oc ${io.ctl.config.bits.cfgData1} kh ${io.ctl.config.bits.cfgData2}")
    }.elsewhen(config_i === 5.U && io.ctl.config.valid){
      kw_v := true.B
      kw := io.ctl.config.bits.cfgData1
      ohb_v := true.B
      ohb := io.ctl.config.bits.cfgData2
      config_i := 6.U
      printf(p"kw ${io.ctl.config.bits.cfgData1} ohb ${io.ctl.config.bits.cfgData2}")
    }.elsewhen(config_i === 6.U && io.ctl.config.valid){
      paddingH_v := true.B
      paddingH := io.ctl.config.bits.cfgData1
      paddingW_v := true.B
      paddingW := io.ctl.config.bits.cfgData2
      config_i := 7.U
      printf(p"paddingH ${io.ctl.config.bits.cfgData1} paddingW ${io.ctl.config.bits.cfgData2}")
    }.elsewhen(config_i === 7.U && io.ctl.config.valid){
      oh_v := true.B
      ow_v := true.B
//      oh := (ih+(paddingH<<1.U)-kh)/strideH + 1.U
//      ow := (iw+(paddingW<<1.U)-kw)/strideW + 1.U
      oh := io.ctl.config.bits.cfgData1
      ow := io.ctl.config.bits.cfgData2
      config_i := 8.U
      printf(p"Caddr ${io.ctl.config.bits.cfgData1}\n")
    }.elsewhen(config_i === 8.U && io.ctl.config.valid){
      Caddr_v := true.B
      Caddr := io.ctl.config.bits.cfgData1
      dataType_v := true.B
      dataType := io.ctl.config.bits.cfgData2
      config_i := 9.U
      printf(p"convTOP start!\n")
    }.elsewhen(config_i === 9.U ){
      Caddr_v := false.B
      config_i := 10.U
    }.elsewhen(config_i === 10.U && CScratchpad.map(_.idle).reduce(_&_)){
      running := false.B
      config_i := 0.U
      printf("convTOP complete!\n")
    }.otherwise{
      oc_v := false.B
      ic_v := false.B
      ih_v := false.B
      iw_v := false.B
      oh_v := false.B
      ow_v := false.B
      kh_v := false.B
      kw_v := false.B
      ohb_v := false.B
      icb_v := false.B
      paddingH_v := false.B
      paddingW_v := false.B
      strideH_v := false.B
      strideW_v := false.B
      Aaddr_v := false.B
      Baddr_v := false.B
      Caddr_v := false.B
    }

//    printf(p"io.ctl.config.valid ${io.ctl.config.valid} \n")

    PEs.map(_.icb.valid := icb_v)
    PEs.map(_.icb.bits := icb)
    PEs.map(_.datatype.valid := dataType_v)
    PEs.map(_.datatype.bits := dataType)
    Abuffer.map(_.icb.valid := icb_v)
    Abuffer.map(_.icb.bits := icb)
    Abroadcast.config := DontCare
    Abroadcast.config.oc.valid := oc_v
    Abroadcast.config.oc.bits := oc
    Abroadcast.config.ic.valid := ic_v
    Abroadcast.config.ic.bits := ic
    Abroadcast.config.ih.valid := ih_v
    Abroadcast.config.ih.bits := ih
    Abroadcast.config.iw.valid := iw_v
    Abroadcast.config.iw.bits := iw
    Abroadcast.config.oh.valid := oh_v
    Abroadcast.config.oh.bits := oh
    Abroadcast.config.ow.valid := ow_v
    Abroadcast.config.ow.bits := ow
    Abroadcast.config.ohb.valid := ohb_v
    Abroadcast.config.ohb.bits := ohb
    Abroadcast.config.icb.valid := icb_v
    Abroadcast.config.icb.bits := icb
    Abroadcast.config.kh.valid := kh_v
    Abroadcast.config.kh.bits := kh
    Abroadcast.config.kw.valid := kw_v
    Abroadcast.config.kw.bits := kw
    Abroadcast.config.paddingH.valid := paddingH_v
    Abroadcast.config.paddingH.bits := paddingH
    Abroadcast.config.paddingW.valid := paddingW_v
    Abroadcast.config.paddingW.bits := paddingW
    Abroadcast.config.strideH.valid := strideH_v
    Abroadcast.config.strideH.bits := strideH
    Abroadcast.config.strideW.valid := strideW_v
    Abroadcast.config.strideW.bits := strideW
    Abroadcast.config.start := config_i === 9.U

    AScratchpad.config := DontCare
    AScratchpad.config.oc.valid := oc_v
    AScratchpad.config.oc.bits := oc
    AScratchpad.config.ic.valid := ic_v
    AScratchpad.config.ic.bits := ic
    AScratchpad.config.ih.valid := ih_v
    AScratchpad.config.ih.bits := ih
    AScratchpad.config.iw.valid := iw_v
    AScratchpad.config.iw.bits := iw
    AScratchpad.config.oh.valid := oh_v
    AScratchpad.config.oh.bits := oh
    AScratchpad.config.ow.valid := ow_v
    AScratchpad.config.ow.bits := ow  
    AScratchpad.config.ohb.valid := ohb_v
    AScratchpad.config.ohb.bits := ohb
    AScratchpad.config.icb.valid := icb_v
    AScratchpad.config.icb.bits := icb
    AScratchpad.config.kh.valid := kh_v
    AScratchpad.config.kh.bits := kh
    AScratchpad.config.kw.valid := kw_v
    AScratchpad.config.kw.bits := kw
    AScratchpad.config.Aaddr.valid := Aaddr_v
    AScratchpad.config.Aaddr.bits := Aaddr
    AScratchpad.config.strideH.valid := strideH_v
    AScratchpad.config.strideH.bits := strideH
    AScratchpad.config.strideW.valid := strideW_v
    AScratchpad.config.strideW.bits := strideW
    AScratchpad.config.paddingH.valid := paddingH_v
    AScratchpad.config.paddingH.bits := paddingH
    AScratchpad.config.paddingW.valid := paddingW_v
    AScratchpad.config.paddingW.bits := paddingW
    AScratchpad.config.start := config_i === 9.U

    Bbuffer.map(_.icb.valid := icb_v)
    Bbuffer.map(_.icb.bits := icb)
    Bbroadcast.map(_.config := DontCare)
    Bbroadcast.map(_.config.oc.valid := oc_v)
    Bbroadcast.map(_.config.oc.bits := oc)
    Bbroadcast.map(_.config.ic.valid := ic_v)
    Bbroadcast.map(_.config.ic.bits := ic)
    Bbroadcast.map(_.config.ih.valid := ih_v)
    Bbroadcast.map(_.config.ih.bits := ih)
    Bbroadcast.map(_.config.iw.valid := iw_v)
    Bbroadcast.map(_.config.iw.bits := iw)
    Bbroadcast.map(_.config.oh.valid := oh_v)
    Bbroadcast.map(_.config.oh.bits := oh)
    Bbroadcast.map(_.config.ow.valid := ow_v)
    Bbroadcast.map(_.config.ow.bits := ow)    
    Bbroadcast.map(_.config.ohb.valid := ohb_v)
    Bbroadcast.map(_.config.ohb.bits := ohb)
    Bbroadcast.map(_.config.icb.valid := icb_v)
    Bbroadcast.map(_.config.icb.bits := icb)
    Bbroadcast.map(_.config.kh.valid := kh_v)
    Bbroadcast.map(_.config.kh.bits := kh)
    Bbroadcast.map(_.config.kw.valid := kw_v)
    Bbroadcast.map(_.config.kw.bits := kw)
    Bbroadcast.map(_.config.paddingH.valid := paddingH_v)
    Bbroadcast.map(_.config.paddingH.bits := paddingH)
    Bbroadcast.map(_.config.paddingW.valid := paddingW_v)
    Bbroadcast.map(_.config.paddingW.bits := paddingW)
    Bbroadcast.map(_.config.strideH.valid := strideH_v)
    Bbroadcast.map(_.config.strideH.bits := strideH)
    Bbroadcast.map(_.config.strideW.valid := strideW_v)
    Bbroadcast.map(_.config.strideW.bits := strideW)
    Bbroadcast.map(_.config.start := config_i === 9.U)
    Bbroadcast.zipWithIndex.foreach {
      case(b, i) => b.ocn.bits := i.U
//      case(b, i) => b.ocn.valid := start
    }
    Bbroadcast.map(_.ocn.valid := config_i === 9.U)

    BScratchpad.map(_.config := DontCare)
    BScratchpad.map(_.config.oc.valid := oc_v)
    BScratchpad.map(_.config.oc.bits := oc)
    BScratchpad.map(_.config.ic.valid := ic_v)
    BScratchpad.map(_.config.ic.bits := ic)
    BScratchpad.map(_.config.ih.valid := ih_v)
    BScratchpad.map(_.config.ih.bits := ih)
    BScratchpad.map(_.config.iw.valid := iw_v)
    BScratchpad.map(_.config.iw.bits := iw)
    BScratchpad.map(_.config.oh.valid := oh_v)
    BScratchpad.map(_.config.oh.bits := oh)
    BScratchpad.map(_.config.ow.valid := ow_v)
    BScratchpad.map(_.config.ow.bits := ow)    
    BScratchpad.map(_.config.ohb.valid := ohb_v)
    BScratchpad.map(_.config.ohb.bits := ohb)
    BScratchpad.map(_.config.icb.valid := icb_v)
    BScratchpad.map(_.config.icb.bits := icb)
    BScratchpad.map(_.config.kh.valid := kh_v)
    BScratchpad.map(_.config.kh.bits := kh)
    BScratchpad.map(_.config.kw.valid := kw_v)
    BScratchpad.map(_.config.kw.bits := kw)
    BScratchpad.map(_.config.Baddr.valid := Baddr_v)
    BScratchpad.map(_.config.Baddr.bits := Baddr)
    BScratchpad.map(_.config.paddingH.valid := paddingH_v)
    BScratchpad.map(_.config.paddingH.bits := paddingH)
    BScratchpad.map(_.config.paddingW.valid := paddingW_v)
    BScratchpad.map(_.config.paddingW.bits := paddingW)
    BScratchpad.map(_.config.strideH.valid := strideH_v)
    BScratchpad.map(_.config.strideH.bits := strideH)
    BScratchpad.map(_.config.strideW.valid := strideW_v)
    BScratchpad.map(_.config.strideW.bits := strideW)
    BScratchpad.map(_.config.start := config_i === 9.U)
    BScratchpad.zipWithIndex.foreach {
      case(b, i) => b.ocn.bits := i.U
//      case(b, i) => b.ocn.valid := start
    }
    BScratchpad.map(_.ocn.valid := config_i === 9.U)

    Cbuffer.map(_.kw.valid := kw_v)
    Cbuffer.map(_.kw.bits := kw)
    C2PE.map(_.config := DontCare)
    C2PE.map(_.config.oc.valid := oc_v)
    C2PE.map(_.config.oc.bits := oc)
    C2PE.map(_.config.ic.valid := ic_v)
    C2PE.map(_.config.ic.bits := ic)
    C2PE.map(_.config.ih.valid := ih_v)
    C2PE.map(_.config.ih.bits := ih)
    C2PE.map(_.config.iw.valid := iw_v)
    C2PE.map(_.config.iw.bits := iw)
    C2PE.map(_.config.oh.valid := oh_v)
    C2PE.map(_.config.oh.bits := oh)
    C2PE.map(_.config.ow.valid := ow_v)
    C2PE.map(_.config.ow.bits := ow)
    C2PE.map(_.config.ohb.valid := ohb_v)
    C2PE.map(_.config.ohb.bits := ohb)
    C2PE.map(_.config.icb.valid := icb_v)
    C2PE.map(_.config.icb.bits := icb)
    C2PE.map(_.config.kh.valid := kh_v)
    C2PE.map(_.config.kh.bits := kh)
    C2PE.map(_.config.kw.valid := kw_v)
    C2PE.map(_.config.kw.bits := kw)
    C2PE.map(_.config.paddingH.valid := paddingH_v)
    C2PE.map(_.config.paddingH.bits := paddingH)
    C2PE.map(_.config.paddingW.valid := paddingW_v)
    C2PE.map(_.config.paddingW.bits := paddingW)
    C2PE.map(_.config.strideH.valid := strideH_v)
    C2PE.map(_.config.strideH.bits := strideH)
    C2PE.map(_.config.strideW.valid := strideW_v)
    C2PE.map(_.config.strideW.bits := strideW)
    C2PE.map(_.config.start := config_i === 9.U)
    C2PE.map(_.ocn.valid := config_i === 9.U)
    C2PE.zipWithIndex.foreach {
      case(c, i) => c.ocn.bits := i.U
//      case(c, i) => c.ocn.valid := start
    }

    CScratchpad.map(_.config := DontCare)
    CScratchpad.map(_.config.oc.valid := oc_v)
    CScratchpad.map(_.config.oc.bits := oc)
    CScratchpad.map(_.config.ic.valid := ic_v)
    CScratchpad.map(_.config.ic.bits := ic)
    CScratchpad.map(_.config.icb.valid := icb_v)
    CScratchpad.map(_.config.icb.bits := icb)
    CScratchpad.map(_.config.ohb.valid := ohb_v)
    CScratchpad.map(_.config.ohb.bits := ohb)
    CScratchpad.map(_.config.ih.valid := ih_v)
    CScratchpad.map(_.config.ih.bits := ih)
    CScratchpad.map(_.config.oh.valid := oh_v)
    CScratchpad.map(_.config.oh.bits := oh)
    CScratchpad.map(_.config.ow.valid := ow_v)
    CScratchpad.map(_.config.ow.bits := ow)
    CScratchpad.map(_.config.Caddr.valid := Caddr_v)
    CScratchpad.map(_.config.Caddr.bits := Caddr)
    CScratchpad.map(_.config.strideH.valid := strideH_v)
    CScratchpad.map(_.config.strideH.bits := strideH)
    CScratchpad.map(_.config.dataType.valid := dataType_v)
    CScratchpad.map(_.config.dataType.bits := dataType)
    CScratchpad.map(_.config.start := config_i === 9.U)
    CScratchpad.zipWithIndex.foreach {
      case(c, i) => c.ocn.bits := i.U
//      case(c, i) => c.ocn.valid := start
    }
    CScratchpad.map(_.ocn.valid := config_i === 9.U)

    PEs.zip(Abuffer).foreach { case(pe, a) => 
//      pe.A2PE <> a.data2PE
      pe.A2PE.valid := a.data2PE.valid
      pe.A2PE.bits := a.data2PE.bits
      a.data2PE.ready := pe.A2PE.ready
    }
    PEs.zip(Bbuffer).foreach { case(pe, b) =>
//      case(pe, b) => pe.B2PE <> b.data2PE
      pe.B2PE.valid := b.data2PE.valid
      pe.B2PE.bits := b.data2PE.bits
      b.data2PE.ready := pe.B2PE.ready      
    }
    PEs.zip(Cbuffer).foreach { case(pe, c) =>
//      case(pe, c) => pe.PE2C <> c.PEres
      c.PEres.valid := pe.PE2C.valid
      c.PEres.bits := pe.PE2C.bits
      pe.PE2C.ready := c.PEres.ready 
    }


    Abroadcast.PEsAbufferFull.zipWithIndex.foreach {
      case(a, i) => a := Abuffer(i).full
    }
    Abuffer.map(_.datafSP := Abroadcast.Data2PEs)

    Abroadcast.readA <> AScratchpad.bufferRead

    Bbuffer.zip(Bbroadcast).foreach {
      case(a, b) => 
//        a.datafSP <> b.Data2PEs
//        a.full <> b.PEsBbufferFull(0)
        a.datafSP := b.Data2PEs 
        b.PEsBbufferFull(0) := a.full
    }

    Bbroadcast.zipWithIndex.foreach {
      case(b, i) => b.ocn.bits := i.U
    }
    Bbroadcast.zip(BScratchpad).foreach {
      case(a, b) => 
//        a.readB <> b.bufferRead
        b.bufferRead.v := a.readB.v
        b.bufferRead.kh := a.readB.kh
        b.bufferRead.kw := a.readB.kw
        b.bufferRead.lastohb := a.readB.lastohb
        b.bufferRead.lastoh := a.readB.lastoh
        b.bufferRead.lastow := a.readB.lastow
        a.readB.data := b.bufferRead.data
    }


    Cbuffer.zip(C2PE).foreach {
      case(a, b) =>
        a.datafC.valid := b.data2PE.valid
        b.data2PE.ready := a.datafC.ready
        a.datafC.bits.data := b.data2PE.bits.data
//        a.datafC.bits.khi := b.data2PE.bits.khi
        a.datafC.bits.lastn := b.data2PE.bits.lastn
        a.datafC.bits.ohbi := b.data2PE.bits.ohbi

        b.datafPE := a.data2C
    }

    C2PE.zip(CScratchpad).foreach {
      case(a, b) =>
        b.bufferRead.v := a.readC.v
        b.bufferRead.offset := a.readC.offset
        b.bufferRead.lastn := a.readC.lastn
        a.readC.data := b.bufferRead.data

        b.bufferWrite := a.writeC
    }

    val w_i = Wire(UInt(log2Ceil(ocPENum).W))
    w_i := 0.U
    for(i <- 1 until ocPENum){
        when(CScratchpad(i).writeMem.valid && 
             CScratchpad(i-1).writeMem.bits.req_state < CScratchpad(i).writeMem.bits.req_state){
            w_i := i.U
        }
    }

    io.cmd.acc_req_b.bits.cmd := 1.U
    io.cmd.acc_req_b.valid := CScratchpad(w_i).writeMem.valid
    io.cmd.acc_req_b.bits.addr := CScratchpad(w_i).writeMem.bits.vaddr
    io.cmd.acc_req_b.bits.data := CScratchpad(w_i).writeMem.bits.data
    CScratchpad.map(_.writeMem.ready := false.B)
    CScratchpad(w_i).writeMem.ready := io.cmd.acc_req_b.ready


    val r_b_i = Wire(UInt(log2Ceil(ocPENum).W))
    val r_c_i = Wire(UInt(log2Ceil(ocPENum).W))
    r_b_i := 0.U
    r_c_i := 0.U
    for(i <- 1 until ocPENum){
        when((BScratchpad(i).readMem.valid && 
              BScratchpad(i).readMem.bits.req_state < BScratchpad(i-1).readMem.bits.req_state) ||
             (BScratchpad(i).readMem.valid && !BScratchpad(i-1).readMem.valid)){
            r_b_i := i.U
        }
        when((CScratchpad(i).readMem.valid && 
              CScratchpad(i).readMem.bits.req_state < CScratchpad(i-1).readMem.bits.req_state) ||
             (CScratchpad(i).readMem.valid && !CScratchpad(i-1).readMem.valid)){
            r_c_i := i.U
        }
    }


    val memReq_r = Wire(UInt(2.W))
    //0->A   1->B   2->C
//    memReq_r := Mux(AScratchpad.readMem.bits.req_state <= BScratchpad(r_b_i).readMem.bits.req_state, 
//                  Mux(AScratchpad.readMem.bits.req_state <= CScratchpad(r_c_i).readMem.bits.req_state, 0.U, 2.U),
//                  Mux(BScratchpad(r_b_i).readMem.bits.req_state <= CScratchpad(r_c_i).readMem.bits.req_state, 1.U, 2.U))
//    printf(p"AScratchpad.readMem.valid ${BScratchpad(r_b_i).readMem.valid} AScratchpad.readMem.bits.req_state ${AScratchpad.readMem.bits.req_state}\n")
//    printf(p"BScratchpad(r_b_i).readMem.valid ${BScratchpad(r_b_i).readMem.valid} r_b_i $r_b_i BScratchpad(r_b_i).readMem.bits.req_state ${BScratchpad(r_b_i).readMem.bits.req_state}\n")
//    printf(p"CScratchpad(r_c_i).readMem.valid ${CScratchpad(r_c_i).readMem.valid} r_c_i $r_c_i CScratchpad(r_c_i).readMem.bits.req_state ${CScratchpad(r_c_i).readMem.bits.req_state}\n")
    when(AScratchpad.readMem.valid 
            && AScratchpad.readMem.bits.req_state <= BScratchpad(r_b_i).readMem.bits.req_state
            && AScratchpad.readMem.bits.req_state <= CScratchpad(r_c_i).readMem.bits.req_state){
        memReq_r := 0.U
    }.elsewhen(BScratchpad(r_b_i).readMem.valid
                && BScratchpad(r_b_i).readMem.bits.req_state <= CScratchpad(r_c_i).readMem.bits.req_state){
        memReq_r := 1.U
    }.otherwise{
      when(CScratchpad(r_c_i).readMem.valid){
        memReq_r := 2.U
      }.elsewhen(AScratchpad.readMem.valid){
        memReq_r := 0.U
      }.elsewhen(BScratchpad(r_b_i).readMem.valid){
        memReq_r := 1.U
      }.otherwise{
        memReq_r := 2.U
      }
    }
    io.cmd.acc_req_a.bits.cmd := 0.U
    io.cmd.acc_req_a.valid := AScratchpad.readMem.valid | BScratchpad(r_b_i).readMem.valid | CScratchpad(r_c_i).readMem.valid
    io.cmd.acc_req_a.bits.addr := Mux(memReq_r === 0.U, AScratchpad.readMem.bits.vaddr,
                                    Mux(memReq_r === 1.U, BScratchpad(r_b_i).readMem.bits.vaddr,
                                    CScratchpad(r_c_i).readMem.bits.vaddr))
    io.cmd.acc_req_a.bits.data := DontCare

    AScratchpad.readMem.ready := io.cmd.acc_req_a.ready && memReq_r === 0.U
    BScratchpad.map(_.readMem.ready := false.B)
    BScratchpad(r_b_i).readMem.ready := io.cmd.acc_req_a.ready && memReq_r === 1.U
    CScratchpad.map(_.readMem.ready := false.B)
    CScratchpad(r_c_i).readMem.ready := io.cmd.acc_req_a.ready && memReq_r === 2.U

    AScratchpad.req_id := io.cmd.req_id
    BScratchpad.map(_.req_id := io.cmd.req_id)
    CScratchpad.map(_.req_id := io.cmd.req_id)

    AScratchpad.readData0 := io.buffer0
    AScratchpad.readData1 := io.buffer1
    BScratchpad.map(_.readData0 := io.buffer0)
    BScratchpad.map(_.readData1 := io.buffer1)
    CScratchpad.map(_.readData0 := io.buffer0)
    CScratchpad.map(_.readData1 := io.buffer1)
}


