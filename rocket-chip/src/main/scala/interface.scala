package ascon

import Chisel._

import freechips.rocketchip.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.InOrderArbiter
import freechips.rocketchip.tile._


class WithPac extends Config((site, here, up) => {
  case BuildRoCC => List(
    (p: Parameters) => {
        val pacaccel = LazyModule(new PacModule(OpcodeSet.all)(p))
        pacaccel
    })
})



class  PacModule(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes, nPTWPorts = 1) {
  override lazy val module = new PacModuleImp(this)
}

class PacModuleImp(outer: PacModule)(implicit p: Parameters) extends LazyRoCCModuleImp(outer)
 with HasCoreParameters{
 
 
val perm = Module(new Permutation())
val keyRNG = Module(new LFSR128(42.U(128.W)))
val nonceRNG = Module(new LFSR128(12.U(128.W)))
val key = Wire(UInt(128.W))
val nonce = Wire(UInt(128.W))

    
  val wdata = Reg(UInt(64.W))
  val req_rd = Reg(UInt(64.W))
  val cmdrs1 = Reg(UInt(64.W))
  val cmdfunct = RegInit(3.U(2.W))
  
	val RAS = Reg(UInt(64.W))
	val sponge = Reg(Vec(5, UInt(64.W)))
	val IV = Reg(Vec(5, UInt(64.W)))
	val count = RegInit(0.U(4.W))
	val interrupt = RegInit(false.B)
	
	
  	val funct = io.cmd.bits.inst.funct
  	val dopac = funct === 2.U
  	val doaut = funct === 1.U
  	val dolkey = funct === 0.U
	
	
  val sIdle :: sComp :: sInit :: sResp :: sFail:: Nil = Enum(5)
  val state = RegInit(sInit)

  when(state === sInit){
  	interrupt := false.B
  	when(count === 0.U){
  		//key :=keyRNG.io.out
  		//nonce :=nonceRNG.io.out
  		key := 12.U
  		nonce := 42.U
  		RAS := 0.U
  		IV(0) := ASCONconst.initvecconst
		IV(1) := key(127,64)
		IV(2) := key (63,0) 
		IV(3) := nonce(127,64)
		IV(4) := nonce(63,0) 
		count := count + 1.U 		
  	}.elsewhen(count<13.U){
  		//1~13
  		perm.io.state_i := IV
  		perm.io.rconst := ASCONconst.roundconst(12.U-count) //11 to 0
  		IV := perm.io.state_o
  		count := count + 1.U 
  	}.otherwise{
  		state := sIdle
  		count := 0.U
  		sponge(0) := 0.U
  		sponge(1) := 0.U
  		sponge(2) := 0.U
  		sponge(3) := 0.U
  		sponge(4) := 0.U
  		//RAS = 0
  	}
  }

  when (io.cmd.fire) {//cmd in and sIdle
  	//store the state
    req_rd := io.cmd.bits.inst.rd
    cmdrs1 := io.cmd.bits.rs1
    cmdfunct := funct
    
    
    when(io.cmd.bits.rs1(43,0) === RAS(43,0)){
    	when(count === 6.U){
    		//ras match
    		state := sResp
    		when(funct === 2.U)//pac
    		{
    			wdata := Cat(sponge(0)(19,0),io.cmd.bits.rs1(43,0))
    		}.elsewhen(funct === 1.U)//aut
    		{
    			when(sponge(0)(19,0)===io.cmd.bits.rs1(63,44))
    			{
    				//aut pass
    				wdata := Cat(0.U(20.W),io.cmd.bits.rs1(43,0))
    			}.otherwise{
    				//aut fail
    				wdata :=  io.cmd.bits.rs1
    				//wdata := sponge(0)(19,0) + 100000000.U
    				interrupt := true.B
    			}
    		}.otherwise{
    			// unrecognized insn
    			wdata :=  io.cmd.bits.rs1 //+ 200000000.U
    			interrupt := true.B
    			//state := sInit
    		}
    	}.otherwise{
		//computing and match
		//contine compute
    		state := sComp
    		count := count + 1.U
    		perm.io.state_i := sponge
  		perm.io.rconst := ASCONconst.roundconst(5.U-count) //5 to 0
    		sponge := perm.io.state_o
    	}    
    }.otherwise{
    	// mismatch RAS
    	RAS := io.cmd.bits.rs1
	state := sComp
	//init computation
	count := 0.U
	sponge := IV
	sponge(0) := IV(0) ^ Cat(0.U(20.W),io.cmd.bits.rs1(43,0))
    }
  }.otherwise{
  
  	when(state === sIdle)
 	 {
 	 	when(RAS =/= io.ras)
	  	{
	  	//update the RAS
 	 		RAS := io.ras
			sponge := IV
			sponge(0) := IV(0) ^ Cat(0.U(20.W),io.ras(43,0))
			count := 0.U
  		}.elsewhen(count <6.U){
			count := count+ 1.U 
  			perm.io.state_i := sponge
  			perm.io.rconst := ASCONconst.roundconst(5.U-count) //5 to 0
    			sponge := perm.io.state_o
		}
  	}
  }
  
  when(state === sComp){
  	when(count === 6.U){
 		//computation done
  		state := sResp
    		when(cmdfunct === 2.U)//pac
    		{
    			wdata := Cat(sponge(0)(19,0),cmdrs1(43,0))
    		}.elsewhen(cmdfunct === 1.U)//aut
    		{
    			when(sponge(0)(19,0)===cmdrs1(63,44))
    			{	
    				//aut pass
    				wdata := Cat(0.U(20.W),cmdrs1(43,0))
    			}.otherwise{
    				//aut fail
    				interrupt := true.B
    				wdata :=cmdrs1
    				//wdata := sponge(0) //(19,0)+300000000.U
    			}
    		}.otherwise{
    			// unrecognized insn
    			interrupt := true.B
    			//wdata :=cmdrs1
    			wdata :=cmdrs1 //+ 400000000.U
    		}
  	}.elsewhen(count <6.U){
  		//computing
  		count := count +1.U
  		perm.io.state_i := sponge
  		perm.io.rconst := ASCONconst.roundconst(5.U-count) //5 to 0
    		sponge := perm.io.state_o 		
  	}
  
  }
  
  
  

	


  when (io.resp.fire) { 
  	when(interrupt){
  		state := sInit
  		count := 0.U
	}.otherwise{
		state := sIdle
	} 
  }

  io.cmd.ready := (state === sIdle)
  io.resp.valid := (state === sResp)
  io.resp.bits.rd := req_rd
  io.resp.bits.data := wdata

  io.busy := (state =/= sIdle)
  io.interrupt := interrupt
  //io.interrupt := false.B

}
