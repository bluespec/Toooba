// Copyright (c) 2020 Bluespec, Inc. All Rights Reserved.

package Trace_Data2_to_Trace_Data;

// ================================================================
// This package defines a module to transform a stream of Trace_Data2
// to a stream of (serialnum, Trace_Data)

// ================================================================
// BSV library imports

import FIFOF  :: *;
import GetPut :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import GetPut_Aux :: *;

// ================================================================
// Project riscy-ooo imports (for fields in Trace_Data2)

import Types         :: *;
import ProcTypes     :: *;
import ReorderBuffer :: *;    // for PPCVAddrCSRData

// ================================================================
// Project Toooba imports

import ISA_Decls   :: *;
import TV_Info     :: *;
import Trace_Data2 :: *;

// ================================================================

interface Trace_Data2_to_Trace_Data_IFC;
   method Action init;

   // From Toooba's CommitStage
   interface Put #(Trace_Data2) in;

   interface Get #(Tuple2 #(Bit #(64), Trace_Data)) out;
endinterface

// ================================================================

(* synthesize *)
module mkTrace_Data2_to_Trace_Data (Trace_Data2_to_Trace_Data_IFC);

   Integer verbosity = 0;    // for debugging

   // Input stream
   FIFOF #(Trace_Data2) f_in <- mkFIFOF;

   // Output stream
   FIFOF #(Tuple2 #(Bit #(64), Trace_Data))  f_out <- mkFIFOF;

   // ================================================================
   // Transformer: Trace_Data2 -> (serialnum, Trace_Data)

   function ActionValue #(Tuple2 #(Bit #(64), Trace_Data)) fav_xform (Trace_Data2  td2);
      actionvalue
	 let serialnum = td2.serialnum;
	 Trace_Data td    = ?;
	 ISize      isize = ((td2.orig_inst [1:0] == 2'b11) ? ISIZE32BIT : ISIZE16BIT);

	 if (   (td2.iType == Alu)
	     || (td2.iType == J)
	     || (td2.iType == Jr)
	     || (td2.iType == Auipc))
	    td = mkTrace_I_RD (td2.pc,
			       isize,
			       td2.orig_inst,
			       0,    // TODO: rd
			       0);   // TODO: rd_val

	 else if (   (td2.iType == Br)
		  || (td2.iType == Fence)
		  || (td2.iType == FenceI)
		  || (td2.iType == SFence)
		  || (td2.iType == Ecall)
		  || (td2.iType == Ebreak)
		  || (td2.iType == Mret)
		  || (td2.iType == Sret))
	    td = mkTrace_OTHER (td2.pc, isize, td2.orig_inst);

	 else if (   (td2.iType == Amo)
		  || (td2.iType == Lr)
		  || (td2.iType == Sc))
	    td = mkTrace_AMO (td2.pc,
			      0,                // TODO: funct3
			      isize,
			      td2.orig_inst,
			      0,                // TODO: rd
			      0,                // TODO: rd_val
			      0,                // TODO: rs2_val
			      0                 // TODO: eaddr
			      );
	 else begin
	    if (verbosity != 0) begin
	       $display ("    fav_xform: TBD: Using mkTrace_I_RD for now");
	       $display ("      ", fshow (td2));
	    end
	    td = mkTrace_I_RD (td2.pc,
			       isize,
			       td2.orig_inst,
			       0,    // TODO: rd
			       0);   // TODO: rd_val
	 end
	 return tuple2 (serialnum, td);
      endactionvalue
   endfunction

   // ================================================================
   // RULES

   rule rl_xform;
      Trace_Data2 td2 <- pop (f_in);

      if (verbosity != 0)
	 $display ("%0d: %m.rl_xform: serialnum:%0d  PC:0x%0h  instr:0x%08h",
		   cur_cycle, td2.serialnum, td2.pc, td2.orig_inst,
		   "  iType:", fshow (td2.iType));

      match { .serialnum, .td } <- fav_xform (td2);
      f_out.enq (tuple2 (serialnum, td));
   endrule

   // ================================================================
   // INTERFACE

   method Action init;
      f_in.clear;
      f_out.clear;
   endmethod

   interface in  = toPut (f_in);
   interface out = toGet (f_out);
endmodule

// ================================================================

endpackage
