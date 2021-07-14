// Copyright (c) 2017-2019 Bluespec, Inc. All Rights Reserved.

package DM_Abstract_Commands;

// ================================================================
// This package implements the 'Abstract Command' part of the RISC-V
// Debug Module, i.e., read/write access to RISC-V GPRs, FPRs and CSRs.

// ================================================================
// BSV library imports

import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;
import ConfigReg    :: *;
import Vector       :: *;

// ----------------
// Other library imports

import GetPut_Aux :: *;
import Semi_FIFOF :: *;
import Cur_Cycle  :: *;

// ================================================================

import ISA_Decls      :: *;
import DM_Common      :: *;
import DM_CPU_Req_Rsp :: *;
import ProcTypes      :: *;

// ================================================================
// Interface

interface DM_Abstract_Commands_IFC;
   method Action reset;

   // ----------------
   // DMI facing GDB/host
   method ActionValue #(DM_Word) av_read  (DM_Addr dm_addr);
   method Action  write (DM_Addr dm_addr, DM_Word dm_word);

   // ----------------
   // Facing CPU/hart
   interface Vector #(CoreNum, Client #(DM_CPU_Req #(5,  XLEN), DM_CPU_Rsp #(XLEN))) harts_gpr_mem_client;
`ifdef ISA_F
   interface Vector #(CoreNum, Client #(DM_CPU_Req #(5,  FLEN), DM_CPU_Rsp #(FLEN))) harts_fpr_mem_client;
`endif
   interface Vector #(CoreNum, Client #(DM_CPU_Req #(12, XLEN), DM_CPU_Rsp #(XLEN))) harts_csr_mem_client;
endinterface

// ================================================================

(* synthesize *)
module mkDM_Abstract_Commands (DM_Abstract_Commands_IFC);

   Integer verbosity = 0;    // Normally 0; non-zero for debugging

   // ----------------------------------------------------------------

   Reg #(Bool) rg_start_reg_access <- mkReg (False);

   Reg #(Bit#(20)) rg_dmcontrol_hartsel <- mkConfigReg(0);

   // FIFOs for request/response to access GPRs
   Vector #(CoreNum, FIFOF #(DM_CPU_Req #(5,  XLEN))) f_harts_gpr_reqs <- replicateM(mkFIFOF);
   Vector #(CoreNum, FIFOF #(DM_CPU_Rsp #(XLEN)))     f_harts_gpr_rsps <- replicateM(mkFIFOF);

   // FIFOs for request/response to access FPRs
`ifdef ISA_F
   Vector #(CoreNum, FIFOF #(DM_CPU_Req #(5,  FLEN))) f_harts_fpr_reqs <- replicateM(mkFIFOF);
   Vector #(CoreNum, FIFOF #(DM_CPU_Rsp #(FLEN)))     f_harts_fpr_rsps <- replicateM(mkFIFOF);
`endif

   // FIFOs for request/response to access CSRs
   Vector #(CoreNum, FIFOF #(DM_CPU_Req #(12, XLEN))) f_harts_csr_reqs <- replicateM(mkFIFOF);
   Vector #(CoreNum, FIFOF #(DM_CPU_Rsp #(XLEN)))     f_harts_csr_rsps <- replicateM(mkFIFOF);

   // ----------------------------------------------------------------
   // rg_data0

   Reg #(DM_Word ) rg_data0 <- mkRegU;
`ifdef RV64
   Reg #(DM_Word ) rg_data1 <- mkRegU;
`endif

   // ----------------------------------------------------------------
   // rg_data2..11:    not implemented
   // rg_abstractauto: not implemented
   // rg_progbuf0..15: not implemented
   // ----------------------------------------------------------------
   // rg_abstractcs

   Reg #(Bool)                 rg_abstractcs_busy   <- mkRegU;
   Reg #(DM_abstractcs_cmderr) rg_abstractcs_cmderr <- mkRegU;

   // Size of program buffer, in 32b words
   Bit #(5) abstractcs_progbufsize = 0;
   // Number of data registers implemented (rg_data0, rg_data1)
   Bit #(4) abstractcs_datacount = ((xlen == 32) ? 1 : 2);

   DM_Word virt_rg_abstractcs = {3'b0,
				 abstractcs_progbufsize,
				 11'b0,
				 pack (rg_abstractcs_busy),
				 1'b0,
				 pack (rg_abstractcs_cmderr),
				 4'b0,
				 abstractcs_datacount};

   function Action fa_rg_abstractcs_write (DM_Word dm_word);
      action
	 if (rg_abstractcs_busy) begin
	    rg_abstractcs_cmderr <= DM_ABSTRACTCS_CMDERR_BUSY;
	    $display ("%0d: DM_Abstract_Commands.write: [abstractcs] <= 0x%08h: ERROR", cur_cycle, dm_word);
	    $display ("    DM is busy with a previous abstract command");
	 end
	 else if (fn_abstractcs_cmderr (dm_word) != DM_ABSTRACTCS_CMDERR_NONE) begin
	    rg_abstractcs_cmderr <= DM_ABSTRACTCS_CMDERR_NONE;
	    if (verbosity != 0)
	       $display ("%0d: DM_Abstract_Commands.write [abstractcs]: clearing cmderr", cur_cycle);
	 end
	 else begin
	    if (verbosity != 0)
	       $display ("%0d: DM_Abstract_Commands.write [abstractcs]: cmderr unchanged", cur_cycle);
	 end
      endaction
   endfunction

   // ----------------------------------------------------------------
   // rg_command
   // cmdtype  no register, since we only support 'access reg'
   // size     no register, since we only support 'lower 32b' for RV32
   //          and 'lower 64b' for RV64
   // postexec no register, since we don't support Program Buffer
   // transfer no register, since we always do transfers

   Reg #(Bool) rg_command_access_reg_write <- mkRegU;

   // regno: we only implement lower 13 bits of this 16-bit field
   Reg #(Bit #(13)) rg_command_access_reg_regno <- mkRegU;

   DM_Word virt_rg_command = fn_mk_command_access_reg (
        DM_COMMAND_ACCESS_REG_SIZE_LOWER32
      , False     // postexec
      , True      // transfer
      , rg_command_access_reg_write
      , zeroExtend (rg_command_access_reg_regno));

   function Action fa_rg_command_write (DM_Word dm_word);
      action
	 // TODO: check that CPU is halted, else set cmderr = DM_ABSTRACTCS_CMDERR_HALT_RESUME

	 DM_abstractcs_cmderr cmderr = rg_abstractcs_cmderr;
	 let size = fn_command_access_reg_size (dm_word);

	 // Ignore if 'cmderr' is non-zero
	 if (cmderr != DM_ABSTRACTCS_CMDERR_NONE) begin
	    $display ("%0d: DM_Abstract_Commands.write: [command] <= 0x%08h: ERROR", cur_cycle, dm_word);
	    $display ("    Ignoring since 'cmderr' is 0x%0h", cmderr);
	 end
	 else begin
	    if (rg_abstractcs_busy) begin
	       cmderr = DM_ABSTRACTCS_CMDERR_BUSY;
	       $display ("%0d: DM_Abstract_Commands.write: [command] <= 0x%08h: ERROR", cur_cycle, dm_word);
	       $display ("    DM is busy with a previous abstract command");
	    end

	    // Only 'Access Reg' cmdtype is supported
	    else if (fn_command_cmdtype (dm_word) != DM_COMMAND_CMDTYPE_ACCESS_REG) begin
	       cmderr = DM_ABSTRACTCS_CMDERR_NOT_SUPPORTED;
	       $display ("%0d: DM_Abstract_Commands.write: [command] <= 0x%08h: ERROR", cur_cycle, dm_word);
	       $display ("    ", fshow (fn_command_cmdtype (dm_word)), " not supported");
	    end

`ifdef RV32
	    // Only lower 32-bit access is supported
	    else if (size != DM_COMMAND_ACCESS_REG_SIZE_LOWER32) begin
	       cmderr = DM_ABSTRACTCS_CMDERR_NOT_SUPPORTED;
	       $display ("%0d: DM_Abstract_Commands.write: [command] <= 0x%08h: ERROR", cur_cycle, dm_word);
	       $display ("    For DM_COMMAND_CMDTYPE_ACCESS_REG, ",
			 fshow (fn_command_access_reg_size (dm_word)), " not supported in RV32 mode");
	    end
`endif
`ifdef RV64
	    // Only lower 32-bit and 64-bit access is supported
	    else if (size != DM_COMMAND_ACCESS_REG_SIZE_LOWER64)
	       begin
		  cmderr = DM_ABSTRACTCS_CMDERR_NOT_SUPPORTED;
		  $display ("%0d: DM_Abstract_Commands.write: [command] <= 0x%08h: ERROR", cur_cycle, dm_word);
		  $display ("    For DM_COMMAND_CMDTYPE_ACCESS_REG, ",
			    fshow (fn_command_access_reg_size (dm_word)), " not supported in RV64 mode");
	       end
`endif

	    // 'postexec' is not supported
	    else if (fn_command_access_reg_postexec (dm_word) == True) begin
	       cmderr = DM_ABSTRACTCS_CMDERR_NOT_SUPPORTED;
	       $display ("%0d: DM_Abstract_Commands.write: [command] <= 0x%08h: ERROR", cur_cycle, dm_word);
	       $display ("    For DM_COMMAND_CMDTYPE_ACCESS_REG, postexec not supported");
	    end

	    // non-'transfer' is not supported
	    else if (fn_command_access_reg_transfer (dm_word) == False) begin
	       cmderr = DM_ABSTRACTCS_CMDERR_NOT_SUPPORTED;
	       $display ("%0d: DM_Abstract_Commands.write: [command] <= 0x%08h: ERROR", cur_cycle, dm_word);
	       $display ("    For DM_COMMAND_CMDTYPE_ACCESS_REG, no-transfer not supported");
	    end

	    else begin
	       Bool      is_write = fn_command_access_reg_write (dm_word);
	       Bit #(13) regno    = truncate (fn_command_access_reg_regno (dm_word));

	       rg_command_access_reg_write <= is_write;
	       rg_command_access_reg_regno <= regno;
	       rg_abstractcs_busy          <= True;
	       rg_start_reg_access         <= True;
	       cmderr = DM_ABSTRACTCS_CMDERR_NONE;
	       if (verbosity != 0)
	          $display ("%0d: DM_Abstract_Commands.write: [command] <= 0x%08h: OKAY", cur_cycle, dm_word);
	    end
	    rg_abstractcs_cmderr <= cmderr;
	 end
      endaction
   endfunction

   // ----------------------------------------------------------------
   // Register reads and writes

   Bool is_csr = (   (fromInteger (dm_command_access_reg_regno_csr_0) <= rg_command_access_reg_regno)
		  && (rg_command_access_reg_regno <= fromInteger (dm_command_access_reg_regno_csr_FFF)));

   Bool is_gpr = (   (fromInteger (dm_command_access_reg_regno_gpr_0) <= rg_command_access_reg_regno)
		  && (rg_command_access_reg_regno <= fromInteger (dm_command_access_reg_regno_gpr_1F)));

`ifdef ISA_F
   Bool is_fpr = (   (fromInteger (dm_command_access_reg_regno_fpr_0) <= rg_command_access_reg_regno)
		  && (rg_command_access_reg_regno <= fromInteger (dm_command_access_reg_regno_fpr_1F)));
`else
   Bool is_fpr = False;
`endif

   Bit #(12) csr_addr = truncate (rg_command_access_reg_regno - fromInteger (dm_command_access_reg_regno_csr_0));
   Bit #(5)  gpr_addr = truncate (rg_command_access_reg_regno - fromInteger (dm_command_access_reg_regno_gpr_0));
   Bit #(5)  fpr_addr = truncate (rg_command_access_reg_regno - fromInteger (dm_command_access_reg_regno_fpr_0));

   // ----------------------------------------------------------------
   // Write CSR


   Rules finishRules = emptyRules;

   for (Integer core = 0; core < valueOf(CoreNum); core = core + 1) begin
      rule rl_csr_write_start (   rg_abstractcs_busy
			       && rg_start_reg_access
			       && rg_command_access_reg_write
				  && is_csr
				  && (fromInteger(core) == rg_dmcontrol_hartsel));
	 let req = DM_CPU_Req {write:   True,
			       address: csr_addr,
`ifdef RV32
			       data:    rg_data0
`endif
`ifdef RV64
			       data:    {rg_data1, rg_data0}
`endif
			       };
	 f_harts_csr_reqs[core].enq (req);
	 rg_start_reg_access <= False;

	 if (verbosity != 0)
	    $display ("%0d: DM_Abstract_Commands.rl_csr_write_start hart %0d: ", cur_cycle, core, fshow (req));
      endrule

      // ----------------

      finishRules = rJoinMutuallyExclusive(finishRules,
	 rules
	    rule rl_csr_write_finish (rg_abstractcs_busy
				   && rg_command_access_reg_write
				   && is_csr);
	       let rsp <- pop (f_harts_csr_rsps[core]);
	       if (verbosity != 0)
		  $display ("%0d: DM_Abstract_Commands.rl_csr_write_finish hart %0d: ", cur_cycle, core, fshow (rsp));

	       rg_abstractcs_cmderr <= (rsp.ok ? DM_ABSTRACTCS_CMDERR_NONE : DM_ABSTRACTCS_CMDERR_HALT_RESUME);
	       rg_abstractcs_busy   <= False;
	    endrule
	 endrules
      );

      // ----------------------------------------------------------------
      // Read CSR

      rule rl_csr_read_start (   rg_abstractcs_busy
			      && rg_start_reg_access
			      && (! rg_command_access_reg_write)
			      && is_csr
			      && (fromInteger(core) == rg_dmcontrol_hartsel));
	 Bit #(XLEN) data = ?;
	 let req = DM_CPU_Req {write: False, address: csr_addr, data: data};
	 f_harts_csr_reqs[core].enq (req);
	 rg_start_reg_access <= False;

	 if (verbosity != 0)
	    $display ("%0d: DM_Abstract_Commands.rl_csr_read_start hart %0d: ", cur_cycle, core, fshow (req));
      endrule

      // ----------------

      finishRules = rJoinMutuallyExclusive(finishRules,
	 rules
	    rule rl_csr_read_finish (   rg_abstractcs_busy
				     && (! rg_command_access_reg_write)
				     && is_csr);
	       let rsp <- pop (f_harts_csr_rsps[core]);
	       if (verbosity != 0)
		  $display ("%0d: DM_Abstract_Commands.rl_csr_read_finish hart %0d: ", cur_cycle, core, fshow (rsp));

	       rg_abstractcs_cmderr <= (rsp.ok ? DM_ABSTRACTCS_CMDERR_NONE : DM_ABSTRACTCS_CMDERR_HALT_RESUME);
`ifdef RV32
	       rg_data0 <= rsp.data;
`endif
`ifdef RV64
	       rg_data0 <= truncate (rsp.data);
	       rg_data1 <= rsp.data[63:32];
`endif
	       rg_abstractcs_busy <= False;
	    endrule
	 endrules
      );

      // ----------------------------------------------------------------
      // Write GPR

      rule rl_gpr_write_start (   rg_abstractcs_busy
			       && rg_start_reg_access
			       && rg_command_access_reg_write
			       && is_gpr
			       && (fromInteger(core) == rg_dmcontrol_hartsel));
	 let req = DM_CPU_Req {write:   True,
			       address: gpr_addr,
`ifdef RV32
			       data:    rg_data0
`endif
`ifdef RV64
			       data:    {rg_data1, rg_data0}
`endif
			       };
	 f_harts_gpr_reqs[core].enq (req);
	 rg_start_reg_access <= False;
	 if (verbosity != 0)
	    $display ("%0d: DM_Abstract_Commands.rl_gpr_write_start hart %0d: ", cur_cycle, core, fshow (req));
      endrule

      // ----------------

      finishRules = rJoinMutuallyExclusive(finishRules,
	 rules
	    rule rl_gpr_write_finish (   rg_abstractcs_busy
				      && rg_command_access_reg_write
				      && is_gpr);
	       let rsp <- pop (f_harts_gpr_rsps[core]);
	       if (verbosity != 0)
		  $display ("%0d: DM_Abstract_Commands.rl_gpr_write_finish hart %0d: ", cur_cycle, core, fshow (rsp));

	       rg_abstractcs_cmderr <= (rsp.ok ? DM_ABSTRACTCS_CMDERR_NONE : DM_ABSTRACTCS_CMDERR_HALT_RESUME);
	       rg_abstractcs_busy   <= False;
	    endrule
	 endrules
      );

      // ----------------------------------------------------------------
      // Read GPR

      rule rl_gpr_read_start (   rg_abstractcs_busy
			      && rg_start_reg_access
			      && (! rg_command_access_reg_write)
			      && is_gpr
			      && (fromInteger(core) == rg_dmcontrol_hartsel));
	 Bit #(XLEN) data = ?;
	 let req = DM_CPU_Req {write: False, address: gpr_addr, data: data };
	 f_harts_gpr_reqs[core].enq (req);
	 rg_start_reg_access <= False;

	 if (verbosity != 0)
	    $display ("%0d: DM_Abstract_Commands.rl_gpr_read_start hart %0d: ", cur_cycle, core, fshow (req));
      endrule

      // ----------------

      finishRules = rJoinMutuallyExclusive(finishRules,
	 rules
	    rule rl_gpr_read_finish (   rg_abstractcs_busy
				     && (! rg_command_access_reg_write)
				     && is_gpr);
	       let rsp <- pop (f_harts_gpr_rsps[core]);
	       if (verbosity != 0)
		  $display ("%0d: DM_Abstract_Commands.rl_gpr_read_finish hart %0d: ", cur_cycle, core, fshow (rsp));

`ifdef RV32
	       rg_data0 <= rsp.data;
`endif
`ifdef RV64
	       rg_data0 <= truncate (rsp.data);
	       rg_data1 <= rsp.data[63:32];
`endif
	       rg_abstractcs_cmderr <= (rsp.ok ? DM_ABSTRACTCS_CMDERR_NONE : DM_ABSTRACTCS_CMDERR_HALT_RESUME);
	       rg_abstractcs_busy <= False;
	    endrule
	 endrules
      );

      // ----------------------------------------------------------------
      // Write FPR

`ifdef ISA_F

      rule rl_fpr_write_start (   rg_abstractcs_busy
			       && rg_start_reg_access
			       && rg_command_access_reg_write
			       && is_fpr
			       && (fromInteger(core) == rg_dmcontrol_hartsel));
	 DM_CPU_Req#(5, ISA_Decls::FLEN) req = DM_CPU_Req {write:   True,
							   address: fpr_addr,
`ifdef RV32
							   data:    unpack(zeroExtend(rg_data0))
`endif
`ifdef RV64
							   data:    unpack({rg_data1, rg_data0})
`endif
							  };
	 f_harts_fpr_reqs[core].enq (req);
	 rg_start_reg_access <= False;
	 if (verbosity != 0)
	    $display ("%0d: DM_Abstract_Commands.rl_fpr_write_start hart %0d: ", cur_cycle, core, fshow (req));
      endrule

      // ----------------

      finishRules = rJoinMutuallyExclusive(finishRules,
	 rules
	    rule rl_fpr_write_finish (   rg_abstractcs_busy
				      && rg_command_access_reg_write
				      && is_fpr);
	       let rsp <- pop (f_harts_fpr_rsps[core]);
	       if (verbosity != 0)
		  $display ("%0d: DM_Abstract_Commands.rl_fpr_write_finish hart %0d: ", cur_cycle, core, fshow (rsp));

	       rg_abstractcs_cmderr <= (rsp.ok ? DM_ABSTRACTCS_CMDERR_NONE : DM_ABSTRACTCS_CMDERR_HALT_RESUME);
	       rg_abstractcs_busy   <= False;
	    endrule
	 endrules
      );

      // ----------------------------------------------------------------
      // Read FPR

      rule rl_fpr_read_start (   rg_abstractcs_busy
			      && rg_start_reg_access
			      && (! rg_command_access_reg_write)
			      && is_fpr
			      && (fromInteger(core) == rg_dmcontrol_hartsel));
	 Bit #(FLEN) data = ?;
	 let req = DM_CPU_Req {write: False, address: fpr_addr, data: data };
	 f_harts_fpr_reqs[core].enq (req);
	 rg_start_reg_access <= False;

	 if (verbosity != 0)
	    $display ("%0d: DM_Abstract_Commands.rl_fpr_read_start hart %0d: ", cur_cycle, core, fshow (req));
      endrule

      // ----------------

      finishRules = rJoinMutuallyExclusive(finishRules,
	 rules
	    rule rl_fpr_read_finish (   rg_abstractcs_busy
				     && (! rg_command_access_reg_write)
				     && is_fpr);
	       let rsp <- pop (f_harts_fpr_rsps[core]);
	       if (verbosity != 0)
		  $display ("%0d: DM_Abstract_Commands.rl_fpr_read_finish hart: ", cur_cycle, core, fshow (rsp));

	       rg_data0 <= truncate (rsp.data);
`ifdef RV64
	       rg_data1 <= rsp.data[63:32];
`endif
	       rg_abstractcs_cmderr <= (rsp.ok ? DM_ABSTRACTCS_CMDERR_NONE : DM_ABSTRACTCS_CMDERR_HALT_RESUME);
	       rg_abstractcs_busy <= False;
	    endrule
	 endrules
      );
`endif
   end

   addRules(finishRules);

   // ----------------------------------------------------------------
   // Read/Write unknown address

   rule rl_unknown_write_start (   rg_abstractcs_busy
				&& rg_start_reg_access
				&& rg_command_access_reg_write
				&& (! is_csr) && (! is_gpr) && (! is_fpr));
      if (verbosity != 0)
	 $display ("%0d: DM_Abstract_Commands.rl_unknown_write_start: unknown RISC-V regno [0x%0h] <= 0x%08h",
		   cur_cycle, rg_command_access_reg_regno, rg_data0);

      rg_abstractcs_cmderr <= DM_ABSTRACTCS_CMDERR_OTHER;
      rg_start_reg_access  <= False;
      rg_abstractcs_busy   <= False;
   endrule

   rule rl_unknown_read_start (   rg_abstractcs_busy
			       && rg_start_reg_access
			       && (! rg_command_access_reg_write)
			       && (! is_csr) && (! is_gpr) && (! is_fpr));
      if (verbosity != 0)
	 $display ("%0d: DM_Abstract_Commands.rl_unknown_read_start: unknown RISC-V regno [0x%0h]",
		   cur_cycle, rg_command_access_reg_regno);

      rg_abstractcs_cmderr <= DM_ABSTRACTCS_CMDERR_OTHER;
      rg_start_reg_access  <= False;
      rg_abstractcs_busy   <= False;
   endrule

   // ================================================================
   // INTERFACE

   method Action reset;
      rg_start_reg_access <= False;

      function proj_clear (x) = x.clear();

      mapM_(proj_clear, f_harts_gpr_reqs);
      mapM_(proj_clear, f_harts_gpr_rsps);
      mapM_(proj_clear, f_harts_csr_reqs);
      mapM_(proj_clear, f_harts_csr_rsps);

      rg_abstractcs_busy   <= False;
      rg_abstractcs_cmderr <= DM_ABSTRACTCS_CMDERR_NONE;

      rg_command_access_reg_write <= False;
      rg_command_access_reg_regno <= fromInteger (dm_command_access_reg_regno_gpr_0);

      rg_data0 <= 0;
`ifdef RV64
      rg_data1 <= 0;
`endif

      if (verbosity != 0)
	 $display ("%0d: DM_Abstract_Commands: reset", cur_cycle);
   endmethod

   // ----------------
   // Facing DMI/GDB

   method ActionValue #(DM_Word) av_read (DM_Addr dm_addr);
      actionvalue
	 let dm_addr_name = fshow_dm_addr (dm_addr);
	 DM_Word dm_word = case (dm_addr)
			      dm_addr_abstractcs:   virt_rg_abstractcs;
			      dm_addr_command:      virt_rg_command;
			      dm_addr_data0:        rg_data0;
`ifdef RV64
			      dm_addr_data1:        rg_data1;
`endif
			      // dm_addr_data2..data3
			      // dm_addr_abstractauto
			      // dm_addr_progbuf0..15
			   endcase;
	 if (verbosity != 0)
	    $display ("%0d: DM_Abstract_Commands.av_read: [", cur_cycle, dm_addr_name, "] => 0x%08h", dm_word);
	 return dm_word;
      endactionvalue
   endmethod

   method Action write (DM_Addr dm_addr, DM_Word dm_word);
      action
	 let dm_addr_name = fshow_dm_addr (dm_addr);

	 if (dm_addr == dm_addr_dmcontrol) begin
	    rg_dmcontrol_hartsel <= fn_dmcontrol_hartsel(dm_word);
	    // It is specified that the debugger must not change hartsel while this module is busy.
	    // If this is done, the debug unit will wedge, so print a warning.
	    if (rg_abstractcs_busy) begin
	       $display ("%0d: DM_Abstract_Commands.write: [", cur_cycle, dm_addr_name,
			 "] <= 0x%08h: ERROR: must not change hartsel while busy", dm_word);
	    end
	 end

	 else if (dm_addr == dm_addr_abstractcs)
	    fa_rg_abstractcs_write (dm_word);

	 else if (rg_abstractcs_cmderr != DM_ABSTRACTCS_CMDERR_NONE) begin
	    if (verbosity != 0) begin
	       $display ("%0d: DM_Abstract_Commands.write: [", cur_cycle, dm_addr_name, "] <= 0x%08h: ERROR", dm_word);
	       $display ("    Ignoring: previous cmderr ", fshow (rg_abstractcs_cmderr));
	    end
	 end

	 else if (dm_addr == dm_addr_command)
	    fa_rg_command_write (dm_word);

	 else if (dm_addr == dm_addr_data0) begin
	    rg_data0 <= dm_word;

	    if (verbosity != 0)
	       $display ("%0d: DM_Abstract_Commands.write: [", cur_cycle, dm_addr_name, "] <= 0x%08h", dm_word);
	 end
`ifdef RV64
	 else if (dm_addr == dm_addr_data1) begin
	    rg_data1 <= dm_word;

	    if (verbosity != 0)
	       $display ("%0d: DM_Abstract_Commands.write: [", cur_cycle, dm_addr_name, "] <= 0x%08h", dm_word);
	 end
`endif
	 else begin
	    // dm_addr_data2..12
	    // dm_addr_abstractauto
	    // dm_addr_progbuf0..15
	    rg_abstractcs_cmderr <= DM_ABSTRACTCS_CMDERR_NOT_SUPPORTED;

	    $display ("%0d: DM_Abstract_Commands.write: [", cur_cycle, dm_addr_name,
		      "] <= 0x%08h: ERROR: not supported", dm_word);
	 end
      endaction
   endmethod

   // ----------------
   // Facing CPU/hart
      interface harts_gpr_mem_client = zipWith (toGPClient, f_harts_gpr_reqs, f_harts_gpr_rsps);
`ifdef ISA_F
      interface harts_fpr_mem_client = zipWith (toGPClient, f_harts_fpr_reqs, f_harts_fpr_rsps);
`endif
      interface harts_csr_mem_client = zipWith (toGPClient, f_harts_csr_reqs, f_harts_csr_rsps);
endmodule

// ================================================================

endpackage
