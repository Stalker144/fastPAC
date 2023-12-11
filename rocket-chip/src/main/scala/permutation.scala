package ascon

import chisel3._
import chisel3.util._

object ASCONconst {
//const for each round
def roundconst = VecInit(Seq(
  "h000000000000004b".U(64.W),
  "h000000000000005a".U(64.W),
  "h0000000000000069".U(64.W),
  "h0000000000000078".U(64.W),
  "h0000000000000087".U(64.W),
  "h0000000000000096".U(64.W),
  "h00000000000000a5".U(64.W),
  "h00000000000000b4".U(64.W),
  "h00000000000000c3".U(64.W),
  "h00000000000000d2".U(64.W),
  "h00000000000000e1".U(64.W),
  "h00000000000000f0".U(64.W)
    ))
//sbox   
def Sbox= VecInit(Seq(
	"h04".U(5.W),	"h0b".U(5.W),
	"h1f".U(5.W),	"h14".U(5.W),
	"h1a".U(5.W),	"h15".U(5.W),
	"h09".U(5.W),	"h02".U(5.W),
	"h1b".U(5.W),	"h05".U(5.W),
	"h08".U(5.W),	"h12".U(5.W),
	"h1d".U(5.W),	"h03".U(5.W),
	"h06".U(5.W),	"h1c".U(5.W),
	"h1e".U(5.W),	"h13".U(5.W),
	"h07".U(5.W),	"h0e".U(5.W),
	"h00".U(5.W),	"h0d".U(5.W),
	"h11".U(5.W),	"h18".U(5.W),
	"h10".U(5.W),	"h0c".U(5.W),
	"h01".U(5.W),	"h19".U(5.W),
	"h16".U(5.W),	"h0a".U(5.W),
	"h0f".U(5.W),	"h17".U(5.W)
	))
//IV

def initvecconst = "h80400c0600000000".U(64.W)

}

//permutation for 1 time with given const
class Permutation extends Module{
    val io = IO(new Bundle{
   	val state_i = Input(Vec(5, UInt(64.W)))
   	val state_o = Output(Vec(5, UInt(64.W)))
   	val rconst = Input(UInt(8.W))
    })
	
	
	
	/*
	io.state_o :=io.state_i
	io.state_o(0) := io.state_i(0) +1000.U
	*/
	/**/
	
	val addconst = Module(new Addconst())
	val sbox = Module(new Sbox())
	val diffusion = Module(new Diffusion())
	addconst.io.state_i := io.state_i
	addconst.io.rconst := io.rconst
	sbox.io.state_i <> addconst.io.state_o
	diffusion.io.state_i <> sbox.io.state_o
	io.state_o := diffusion.io.state_o
	
}


class Addconst extends Module{
    val io = IO(new Bundle{
   	val state_i = Input(Vec(5, UInt(64.W)))
   	val state_o = Output(Vec(5, UInt(64.W)))
   	val rconst = Input(UInt(8.W))
    })
    
    
    //add const
	for (i<-0 until 5){
		if(i==2)
			io.state_o(i):=io.state_i(i)^Cat(0.U(56.W), io.rconst)
		else
			io.state_o(i):=io.state_i(i)
	}
}


class Sbox extends Module{
    val io = IO(new Bundle{
   	val state_i = Input(Vec(5, UInt(64.W)))
   	val state_o = Output(Vec(5, UInt(64.W)))
    })
    
    //substitution
	//slice x0 to x4 
	//may bit operate?//
	val svector_i = Reg(Vec(64,UInt(5.W)))
	val svector_o = Reg(Vec(64,UInt(5.W)))
	for (i<-0 until 64)
		svector_i(i) := Cat(io.state_i.map(word => word(i)))
	//run sbox 
	for (j<-0 until 64)
		svector_o(j) := ASCONconst.Sbox(svector_i(j))
		
	//reverse
	for(k<-0 until 5)
		io.state_o(k) := Cat(svector_o.map(word => word(k)))
		
}

class Diffusion extends Module{
    val io = IO(new Bundle{
   	val state_i = Input(Vec(5, UInt(64.W)))
   	val state_o = Output(Vec(5, UInt(64.W)))
    })
    
def rotateLeft(x: UInt, n: Int): UInt = {
  val width = x.getWidth
  Cat(x(width-n-1, 0), x(width-1, width-n))
}

def rotateRight(x: UInt, n: Int): UInt = {
  val width = x.getWidth
  Cat(x(n-1, 0), x(width-1, n))
}
    
	//linear diffusion
	io.state_o(0):=io.state_i(0)^rotateRight(io.state_i(0),19)^rotateRight(io.state_i(0),28)
	io.state_o(1):=io.state_i(1)^rotateRight(io.state_i(1),61)^rotateRight(io.state_i(1),39)
	io.state_o(2):=io.state_i(2)^rotateRight(io.state_i(2),1)^rotateRight(io.state_i(2),6)
	io.state_o(3):=io.state_i(3)^rotateRight(io.state_i(3),10)^rotateRight(io.state_i(3),17)
	io.state_o(4):=io.state_i(4)^rotateRight(io.state_i(4),7)^rotateRight(io.state_i(4),41)
		
}
