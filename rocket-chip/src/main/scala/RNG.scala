package ascon

import chisel3._
import chisel3.util._

class LFSR128(initValue: UInt = 1.U(128.W)) extends Module {//默认为1
  val io = IO(new Bundle {
    val out = Output(UInt(128.W))
  })
	val lfsr =RegInit(initValue)
	// 初始化为非0值
   

  // 16位LFSR的反馈多项式：x^16 + x^14 + x^13 + x^11 + 1
  //[128, 29, 27, 2, 1]
  // 使用这些位生成新的反馈位
  val feedback = Reg(UInt(1.W))
  when(lfsr ===0.U)
  {
  	feedback := 1.U
  }.otherwise
  {
  	feedback := lfsr(0) ^lfsr(1) ^ lfsr(26) ^ lfsr(28) ^ lfsr(127)
  }
  
  
  
  // 在每个时钟周期，左移LFSR并在最低位设置反馈
  lfsr := Cat( lfsr(126, 0),feedback)
  io.out := lfsr
  }





