package boom.acc

import chisel3._
import chisel3.util._
import freechips.rocketchip.config._
import boom.exu.ygjk._

trait HWParameters{
    val ocPENum = 2
//    val ocPENum = 2
    val ohPENum = 1

    val PEHigh = 4   //OW_BLOCK
    val PEWidth = 4  //OC_BLOCK
    val ocWidth = 10
    val icWidth = 10
    val ohWidth = 10
    val owWidth = 10
    val khWidth = 10
    val kwWidth = 10
    val ohbWidth = 10
    val paddingWidth = 3
    val strideWidth = 3

//    val ALineDNum = 16  //A缓存中一行的数据个数，IC_BLOCK最大值
    val ALineDNum = 4
    val BICBNum = ALineDNum
    val ALineDWidth = 32 //A缓存中数据的位宽

    val ADWidth = 32
    val BDWidth = 32 //B缓存中数据的位宽
    val CDWidth = 32 //C缓存中数据的位宽

//    val CSPQEntry = 16
    val CSPQEntry = 12   //需要和ASPIHEntry保持相同
    val PrecWriteEntry = 8    //C模块中写回部件行数
//    val BSPQEntry = 9
    val BSPQEntry = 18
//    val ASPIHEntry = 12
    val ASPIHEntry = 12
    val ASPIWEntry = 32   //iw计算在Scratchpad中的下标时需要对这个数取模，所以要是2的幂次
//    val ASPIWEntry = 8   //iw计算在Scratchpad中的下标时需要对这个数取模，所以要是2的幂次

    val MACLatency = 3
    val MACresq = 8
}

//需要配置的信息：oc -- 控制器发来的oc编号, 
//                ic, oh, ow, kh, kw, ohb -- 外层循环次数,
//                icb -- 矩阵乘计算中的中间长度
//                paddingH, paddingW, strideH, strideW -- 卷积层属性
class ConfigPEIO extends Bundle with HWParameters with YGJKParameters{
    val oc = Flipped(Valid(UInt(ocWidth.W)))               //对应reorder分块后的oc层循环次数
    val ic = Flipped(Valid(UInt(icWidth.W)))               //对应reorder分块后的ic层循环次数
    val ih = Flipped(Valid(UInt(ohWidth.W)))
    val iw = Flipped(Valid(UInt(owWidth.W)))
    val oh = Flipped(Valid(UInt(ohWidth.W)))
    val ow = Flipped(Valid(UInt(owWidth.W)))
    val kh = Flipped(Valid(UInt(khWidth.W)))
    val kw = Flipped(Valid(UInt(kwWidth.W)))
    val ohb = Flipped(Valid(UInt(ohbWidth.W)))
    val icb = Flipped(Valid(UInt(icWidth.W)))
    val paddingH = Flipped(Valid(UInt(paddingWidth.W)))
    val paddingW = Flipped(Valid(UInt(paddingWidth.W)))
    val strideH = Flipped(Valid(UInt(strideWidth.W)))
    val strideW = Flipped(Valid(UInt(strideWidth.W)))
    val start = Input(Bool()) //开始运行信号，持续一拍
    val Aaddr = Flipped(Valid(UInt(addrWidth.W))) //A矩阵首地址
    val Baddr = Flipped(Valid(UInt(addrWidth.W))) //B矩阵首地址
    val Caddr = Flipped(Valid(UInt(addrWidth.W))) //C矩阵首地址
    val dataType = Flipped(Valid(UInt(3.W)))  //1-32位，2-16位， 4-32位
//    val idle = Output(Bool())
}
