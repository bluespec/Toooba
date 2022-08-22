// Copyright (c) 2013-2020 Bluespec, Inc. All Rights Reserved.

//
//-
// RVFI_DII + CHERI modifications:
//     Copyright (c) 2020 Jonathan Woodruff
//     All rights reserved.
//
//     This software was developed by SRI International and the University of
//     Cambridge Computer Laboratory (Department of Computer Science and
//     Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
//     DARPA SSITH research programme.
//
//     This work was supported by NCSC programme grant 4212611/RFA 15971 ("SafeBet").
//-

package Top_HW_Side;

// ================================================================
// mkTop_HW_Side is the top-level system for simulation.
// mkMem_Model is a memory model.

// **** CAVEAT FOR IVERILOG USERS: The 'C_Imports' sections below are
// disabled for IVerilog until we find a clean solution.  They depend
// on imported C which is non-trivial in IVerilog because IVerilog
// still depends on the older Verilog VPI standard instead of the
// newer DPI-C standard.  C-imported functions are used for:
//     UART input polling and character-reading
//     Writing tandem-verfication encoded trace data

// (Note: UART output does not depend on C-imported functions and so
// will work ok even in IVerilog)

// ================================================================
// BSV lib imports

`include "ProcConfig.bsv"

import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;
import Connectable  :: *;
import Clocks       :: *;

// ----------------
// BSV additional libs

import Cur_Cycle  :: *;
import GetPut_Aux :: *;

// ================================================================
// Project imports

import WindCoreInterface :: *;
import AXI4Lite :: *;
import SourceSink :: *;
import ISA_Decls      :: *;
import TV_Info        :: *;
import SoC_Top        :: *;
import Mem_Controller :: *;
import Mem_Model      :: *;
import Fabric_Defs    :: *;

import PLIC :: *;

`ifndef IVERILOG
import C_Imports      :: *;
`endif

`ifdef INCLUDE_GDB_CONTROL
import External_Control :: *;
import Debug_Module     :: *;
`endif

`ifdef RVFI_DII
import RVFI_DII  :: *;
import Types     :: *;
import ProcTypes :: *;
`endif

// ================================================================
// Top-level module.
// Instantiates the SoC.
// Instantiates a memory model.

`ifndef RVFI_DII
(* synthesize *)
module mkTop_HW_Side (Empty);
`else
module mkPre_Top_HW_Side (Toooba_RVFI_DII_Server);
`endif

   // ================================================================
   // The RISC-V Debug Module is at the following point in the module hierarchy:
   //     soc_top.corew.debug_module
   //     (instances of mkSoC_Top, mkCoreW, mkDebug_Module)

   // The Debug Module is reset only once, on power-up, hence we pass
   // its reset down from here.

   // (power-on reset) and the Debug Module's 'hart_reset' control.

   let power_on_reset <- exposeCurrentReset;
   let dm_power_on_reset = power_on_reset;

   // The rest of the system (soc_top and mem_model) are reset:
   // - on power-on, and
   // - when the Debug Module requests an NDM reset (for non-DebugModule).

   let ndm_reset = power_on_reset;

   // ================================================================
   // STATE

   SoC_Top_IFC    soc_top   <- mkSoC_Top (dm_power_on_reset, reset_by ndm_reset);
   Mem_Model_IFC  mem_model <- mkMem_Model (reset_by ndm_reset);

   // Connect SoC to raw memory
   let memCnx <- mkConnection (soc_top.to_raw_mem, mem_model.mem_server, reset_by ndm_reset);

   // ================================================================
   // Actions on reset

   function Action fa_reset_actions;
      action
`ifndef RVFI_DII
	 $display ("================================================================");
	 $display ("Bluespec RISC-V standalone system simulation v1.2");
	 $display ("Copyright (c) 2017-2019 Bluespec, Inc. All Rights Reserved.");
	 $display ("================================================================");
`endif
	 // Set CPU verbosity and logdelay (simulation only)
	 Bool v1 <- $test$plusargs ("v1");
	 Bool v2 <- $test$plusargs ("v2");
	 Bit #(4)  verbosity = ((v2 ? 2 : (v1 ? 1 : 0)));
	 Bit #(64) logdelay  = 0;    // # of instructions after which to set verbosity

	 // ----------------
	 // Load optional tohost and fromhost addrs from symbol-table file
	 Fabric_Addr tohost_addr   = 0;
	 Fabric_Addr fromhost_addr = 0;

	 Bool watch_tohost <- $test$plusargs ("tohost");
`ifndef IVERILOG
	 // Note: see 'CAVEAT FOR IVERILOG USERS' above
	 if (watch_tohost) begin
	    let tha <- c_get_symbol_val ("tohost");
	    tohost_addr   = truncate (tha);

	    let fha <- c_get_symbol_val ("fromhost");
	    fromhost_addr = truncate (fha);
	 end
`endif
	 $display ("INFO: watch_tohost %d, tohost_addr = 0x%0h, fromhost_addr = 0x%0h",
		   watch_tohost, tohost_addr, fromhost_addr);
	 soc_top.start (tohost_addr, fromhost_addr);
      endaction
   endfunction

   // ================================================================
   // BEHAVIOR

   Reg #(Bool) rg_banner_printed <- mkReg (False);

   // Display a banner
   rule rl_step0 (! rg_banner_printed);
      rg_banner_printed <= True;

      fa_reset_actions;

      // ----------------
      // Open file for Tandem Verification trace output
`ifdef INCLUDE_TANDEM_VERIF
`ifndef IVERILOG
      // Note: see 'CAVEAT FOR IVERILOG USERS' above
      let success <- c_trace_file_open ('h_AA);
      if (success == 0) begin
	 $display ("ERROR: Top_HW_Side.rl_step0: error opening trace file.");
	 $display ("    Aborting.");
	 $finish (1);
      end
      else
	 $display ("Top_HW_Side.rl_step0: opened trace file.");
`else
      $display ("Warning: tandem verification output logs not available in IVerilog");
`endif
`endif

      // ----------------
      // Open connection to remote debug client
`ifdef INCLUDE_GDB_CONTROL
`ifndef IVERILOG
      // Note: see 'CAVEAT FOR IVERILOG USERS' above
      let dmi_status <- c_debug_client_connect (dmi_default_tcp_port);
      if (dmi_status != dmi_status_ok) begin
	 $display ("ERROR: Top_HW_Side.rl_step0: error opening debug client connection.");
	 $display ("    Aborting.");
	 $finish (1);
      end
`else
      $display ("Warning: Debug client connection not available in IVerilog");
`endif
`endif

   endrule

   // ================================================================
   // Tandem verifier: drain and output vectors of bytes

`ifdef INCLUDE_TANDEM_VERIF
   rule rl_tv_vb_out;
      let tv_info <- soc_top.tv_verifier_info_get.get;
      let n  = tv_info.num_bytes;
      let vb = tv_info.vec_bytes;

`ifndef IVERILOG
      Bit #(32) success = 1;

      for (Bit #(32) j = 0; j < fromInteger (valueOf (TV_VB_SIZE)); j = j + 8) begin
	 Bit #(64) w64 = { vb [j+7], vb [j+6], vb [j+5], vb [j+4], vb [j+3], vb [j+2], vb [j+1], vb [j] };
	 let success1 <- c_trace_file_load_word64_in_buffer (j, w64);
      end

      if (success == 0)
	 $display ("ERROR: Top_HW_Side.rl_tv_vb_out: error loading %0d bytes into buffer", n);
      else begin
	 // Send the data
	 success <- c_trace_file_write_buffer (n);
	 if (success == 0)
	    $display ("ERROR: Top_HW_Side.rl_tv_vb_out: error writing out bytevec data buffer (%0d bytes)", n);
      end

      if (success == 0) begin
	 $finish (1);
      end
`endif
   endrule
`endif

   // ================================================================
   // UART console I/O

   // Relay system console output to terminal

   rule rl_relay_console_out;
      let ch <- soc_top.get_to_console.get;
      $write ("%c", ch);
      $fflush (stdout);
   endrule

   // Poll terminal input and relay any chars into system console input.
   // Note: rg_console_in_poll is used to poll only every N cycles, whenever it wraps around to 0.
   // Note: see 'CAVEAT FOR IVERILOG USERS' above for why this is ifdef'd out for iVerilog users.

`ifndef IVERILOG

   Reg #(Bit #(12)) rg_console_in_poll <- mkReg (0);

   rule rl_relay_console_in;
      if (rg_console_in_poll == 0) begin
	 Bit #(8) ch <- c_trygetchar (?);
	 if (ch != 0) begin
	    soc_top.put_from_console.put (ch);
	    /*
	    $write ("%0d: Top_HW_Side.bsv.rl_relay_console: ch = 0x%0h", cur_cycle, ch);
	    if (ch >= 'h20) $write (" ('%c')", ch);
	    $display ("");
	    */
	 end
      end
      rg_console_in_poll <= rg_console_in_poll + 1;
   endrule

`endif

   // ================================================================
   // Interaction with remote debug client

`ifdef INCLUDE_GDB_CONTROL

   FIFOF #(Control_Req) f_external_control_reqs <- mkFIFOF;
   FIFOF #(Control_Rsp) f_external_control_rsps <- mkFIFOF;

   rule rl_debug_client_request_recv;
      Bit #(64) req <- c_debug_client_request_recv ('hAA);
      Bit #(8)  status = req [63:56];
      Bit #(32) data   = req [55:24];
      Bit #(16) addr   = req [23:8];
      Bit #(8)  op     = req [7:0];
      if (status == dmi_status_err) begin
	 $display ("%0d: Top_HW_Side.rl_debug_client_request_recv: receive error; aborting",
		   cur_cycle);
	 $finish (1);
      end
      else if (status == dmi_status_ok) begin
	 // $write ("%0d: Top_HW_Side.rl_debug_client_request_recv:", cur_cycle);
	 if (op == dmi_op_read) begin
	    // $display (" READ 0x%0h", addr);
	    let control_req = Control_Req {op: external_control_req_op_read_control_fabric,
					   arg1: zeroExtend (addr),
					   arg2: 0};
	    f_external_control_reqs.enq (control_req);
	 end
	 else if (op == dmi_op_write) begin
	    // $display (" WRITE 0x%0h 0x%0h", addr, data);
	    let control_req = Control_Req {op: external_control_req_op_write_control_fabric,
					   arg1: zeroExtend (addr),
					   arg2: zeroExtend (data)};
	    f_external_control_reqs.enq (control_req);
	 end
	 else if (op == dmi_op_shutdown) begin
	    $display ("Top_HW_Side.rl_debug_client_request_recv: SHUTDOWN");
	    $finish (0);
	 end
	 else if (op == dmi_op_start_command) begin    // For debugging only
	    // $display (" START COMMAND ================================");
	 end
	 else
	    $display (" Top_HW_Side.rl_debug_client_request_recv: UNRECOGNIZED OP %0d; ignoring", op);
      end
   endrule

   rule rl_debug_client_response_send;
      let control_rsp <- pop (f_external_control_rsps);
      // $display ("Top_HW_Side.rl_debug_client_response_send: 0x%0h", control_rsp.result);
      let status <- c_debug_client_response_send (truncate (control_rsp.result));
      if (status == dmi_status_err) begin
	 $display ("%0d: Top_HW_Side.rl_debug_client_response_send: send error; aborting",
		   cur_cycle);
	 $finish (1);
      end
   endrule

   // ----------------------------------------------------------------
   // External debug requests and responses

   Control_Req req = f_external_control_reqs.first;
   Integer dmi_verbosity = 0;    // For debugging

   rule rl_handle_external_req_read_request (req.op == external_control_req_op_read_control_fabric);
      f_external_control_reqs.deq;
      soc_top.debug_subordinate.ar.put(AXI4Lite_ARFlit { araddr: truncate (req.arg1)
                                                       , arprot: ?, aruser: ? });
      if (dmi_verbosity != 0) begin
	 $display ("%0d: %m.rl_handle_external_req_read_request", cur_cycle);
         $display ("    ", fshow (req));
      end
   endrule

   rule rl_handle_external_req_read_response;
      let x <- get (soc_top.debug_subordinate.r);
      let rsp = Control_Rsp {status: external_control_rsp_status_ok, result: signExtend (x.rdata)};
      f_external_control_rsps.enq (rsp);
      if (dmi_verbosity != 0) begin
	 $display ("%0d: %m.rl_handle_external_req_read_response", cur_cycle);
         $display ("    ", fshow (rsp));
      end
   endrule

   rule rl_handle_external_req_write (req.op == external_control_req_op_write_control_fabric);
      f_external_control_reqs.deq;
      soc_top.debug_subordinate.aw.put(AXI4Lite_AWFlit { awaddr: truncate (req.arg1)
                                                       , awprot: ?, awuser: ? });
      soc_top.debug_subordinate.w.put(AXI4Lite_WFlit { wdata: truncate (req.arg2)
                                                     , wstrb: ~0, wuser: ? });
      // let rsp = Control_Rsp {status: external_control_rsp_status_ok, result: 0};
      // f_external_control_rsps.enq (rsp);
      if (dmi_verbosity != 0) begin
         $display ("%0d: %m.rl_handle_external_req_write", cur_cycle);
         $display ("    ", fshow (req));
      end
   endrule

   rule rl_drain_debug_write_rsps; soc_top.debug_subordinate.b.drop; endrule

   rule rl_handle_external_req_err (   (req.op != external_control_req_op_read_control_fabric)
				    && (req.op != external_control_req_op_write_control_fabric));
      f_external_control_reqs.deq;
      let rsp = Control_Rsp {status: external_control_rsp_status_err, result: 0};
      f_external_control_rsps.enq (rsp);

      $display ("%0d: %m.rl_handle_external_req_err: unknown req.op", cur_cycle);
      $display ("    ", fshow (req));
   endrule

   (* descending_urgency = "rl_handle_external_req_read_request,  rl_handle_external_req_read_response" *)
   (* descending_urgency = "rl_handle_external_req_read_response, rl_handle_external_req_write"         *)
   (* descending_urgency = "rl_handle_external_req_read_response, rl_handle_external_req_err"           *)
   (* descending_urgency = "rl_handle_external_req_write,         rl_handle_external_req_err"           *)
   rule rl_handle_external_dummy_for_urgency_attribs_only;
   endrule
`endif

   // ================================================================
   // INTERFACE

   //  None (this is top-level)
   
   //  Except RVFI_DII interface if enabled
`ifdef RVFI_DII
   return soc_top.rvfi_dii_server;
`endif

endmodule

// ================================================================

`ifdef RVFI_DII
// ================================================================
// mkPiccolo_RVFI_DII instantiates the toplevel with the RVFI_DII
// interfaces enabled, allowing testing with directly
// ================================================================

(* synthesize *)
module mkTop_HW_Side(Empty)

    provisos (Add#(a__, TDiv#(DataSz,8), 8), Add#(b__, DataSz, 64), Add#(c__, TDiv#(DataSz,8), 8), Add#(d__, DataSz, 64));

    Reg #(Bool) rg_banner_printed <- mkReg (False);

    // Display a banner
    rule rl_step0 (! rg_banner_printed);
       $display ("================================================================");
       $display ("Bluespec RISC-V standalone system simulation v1.2");
       $display ("Copyright (c) 2017-2018 Bluespec, Inc. All Rights Reserved.");
       $display ("================================================================");

       rg_banner_printed <= True;
    endrule

    RVFI_DII_Bridge #(DataSz, DataSz, TMul#(SupSize, 2), SupSize) bridge <- mkRVFI_DII_Bridge("", 5001);
    let    dut <- mkPre_Top_HW_Side(reset_by bridge.new_rst);
    mkConnection(bridge.client.report, dut.trace_report);

    rule rl_provide_instr;
        Dii_Parcel_Id req <- dut.seqReqFirst.get;
        Dii_Parcel_Resps resps <- bridge.client.getParcels(req);
        dut.parcelResps.put(resps);
    endrule
endmodule

`endif
// ================================================================

endpackage: Top_HW_Side
