package MMIO_AXI4_Adapter;

// ================================================================
// BSV lib imports

import Assert       :: *;
import ConfigReg    :: *;
import FIFOF        :: *;
import GetPut       :: *;
import ClientServer :: *;

// ----------------
// BSV additional libs

import GetPut_Aux     :: *;
import Cur_Cycle      :: *;
import Semi_FIFOF     :: *;
import CreditCounter  :: *;

// ================================================================
// Project imports

// ----------------
// From MIT RISCY-OOO

import ProcTypes :: *;

// ----------------
// From Bluespec Pipes

import AXI4_Types   :: *;
import Fabric_Defs  :: *;

// ================================================================

interface MMIO_AXI4_Adapter_IFC;
   method Action reset;

   interface Server #(MMIOCRq, MMIODataPRs) core_side;

   // Fabric master interface for IO
   interface AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User)  mmio_master;
endinterface

// ================================================================

module mkMMIO_AXI4_Adapter (MMIO_AXI4_Adapter_IFC);

   // Verbosity: 0: quiet; 1: transactions
   Integer verbosity = 0;
   Reg #(Bit #(4)) cfg_verbosity <- mkConfigReg (fromInteger (verbosity));

   // ================================================================
   // Requests from and responses to core

   FIFOF #(MMIOCRq)     f_reqs_from_core <- mkFIFOF;
   FIFOF #(MMIODataPRs) f_rsps_to_core   <- mkFIFOF;

   // ================================================================
   // Fabric request/response

   AXI4_Master_Xactor_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) master_xactor <- mkAXI4_Master_Xactor_2;

   // For discarding write-responses
   CreditCounter_IFC #(4) ctr_wr_rsps_pending <- mkCreditCounter; // Max 15 writes outstanding

   // ================================================================
   // Functions to interact with the fabric

   // Send a read-request into the fabric
   function Action fa_fabric_send_read_req (Fabric_Addr  addr);
      action
	 AXI4_Size  size = axsize_8;
	 let mem_req_rd_addr = AXI4_Rd_Addr {arid:     fabric_default_id,
					     araddr:   addr,
					     arlen:    0,           // burst len = arlen+1
					     arsize:   size,
					     arburst:  fabric_default_burst,
					     arlock:   fabric_default_lock,
					     arcache:  fabric_default_arcache,
					     arprot:   fabric_default_prot,
					     arqos:    fabric_default_qos,
					     arregion: fabric_default_region,
					     aruser:   fabric_default_user};

	 master_xactor.i_rd_addr.enq (mem_req_rd_addr);

	 // Debugging
	 if (cfg_verbosity > 0) begin
	    $display ("    ", fshow (mem_req_rd_addr));
	 end
      endaction
   endfunction

   // Send a write-request into the fabric
   function Action fa_fabric_send_write_req (Fabric_Addr  addr, Fabric_Strb  strb, Bit #(64)  st_val);
      action
	 AXI4_Size  size = axsize_8;
	 let mem_req_wr_addr = AXI4_Wr_Addr {awid:     fabric_default_id,
					     awaddr:   addr,
					     awlen:    0,           // burst len = awlen+1
					     awsize:   size,
					     awburst:  fabric_default_burst,
					     awlock:   fabric_default_lock,
					     awcache:  fabric_default_awcache,
					     awprot:   fabric_default_prot,
					     awqos:    fabric_default_qos,
					     awregion: fabric_default_region,
					     awuser:   fabric_default_user};

	 let mem_req_wr_data = AXI4_Wr_Data {wid:    fabric_default_id,
					     wdata:  st_val,
					     wstrb:  strb,
					     wlast:  True,
					     wuser:  fabric_default_user};

	 master_xactor.i_wr_addr.enq (mem_req_wr_addr);
	 master_xactor.i_wr_data.enq (mem_req_wr_data);

	 // Expect a fabric response
	 ctr_wr_rsps_pending.incr;

	 // Debugging
	 if (cfg_verbosity > 0) begin
	    $display ("            To fabric: ", fshow (mem_req_wr_addr));
	    $display ("                       ", fshow (mem_req_wr_data));
	 end
      endaction
   endfunction

   // ================================================================
   // Handle read requests and responses.
   // Don't do a read while a write is outstanding.
   // This is just an adapter from MMIOCRq/MMIODataPRs to AXI4

   rule rl_handle_read_req (f_reqs_from_core.first.func matches Ld
			    &&& (ctr_wr_rsps_pending.value == 0));
      let req <- pop (f_reqs_from_core);

      if (cfg_verbosity > 0) begin
	 $display ("%0d: MMIO_AXI4_Adapter.rl_handle_read_req: Ld request", cur_cycle);
	 $display ("    ", fshow (req));
      end

      fa_fabric_send_read_req (req.addr);
   endrule

   // ----------------

   rule rl_handle_read_rsps;
      let  mem_rsp <- pop_o (master_xactor.o_rd_data);

      if (cfg_verbosity > 0) begin
	 $display ("%0d: MMIO_AXI4_Adapter.rl_handle_read_rsps ", cur_cycle);
	 $display ("    ", fshow (mem_rsp));
      end

      if ((cfg_verbosity > 0) && (mem_rsp.rresp != axi4_resp_okay)) begin
	 $display ("%0d: MMIO_AXI4_Adapter.rl_handle_read_rsp: fabric response error", cur_cycle);
	 $display ("    ", fshow (mem_rsp));
      end

      let rsp = MMIODataPRs {valid: (mem_rsp.rresp == axi4_resp_okay),
			     data: mem_rsp.rdata};
      f_rsps_to_core.enq (rsp);

      if (cfg_verbosity > 0)
	 $display ("    Response MMIO to core: ", fshow (rsp));
   endrule

   // ================================================================
   // Handle write requests and responses

   rule rl_handle_write_req (f_reqs_from_core.first.func matches St);
      let req <- pop (f_reqs_from_core);

      if (cfg_verbosity > 0) begin
	 $display ("%d: MMIO_AXI4_Adapter.rl_handle_write_req: St request:", cur_cycle);
	 $display ("    ", fshow (req));
      end

      fa_fabric_send_write_req (req.addr, pack (req.byteEn), req.data);
   endrule

   // ----------------
   // Discard write-responses from the fabric

   rule rl_discard_write_rsp;
      let wr_resp <- pop_o (master_xactor.o_wr_resp);

      if (cfg_verbosity > 0) begin
	 $display ("%0d: MMIO_AXI4_Adapter.rl_discard_write_rsp", cur_cycle);
	 $display ("    ", fshow (wr_resp));
      end

      if (ctr_wr_rsps_pending.value == 0) begin
	 $display ("%0d: ERROR: MMIO_AXI4_Adapter.rl_discard_write_rsp: unexpected Wr response (ctr_wr_rsps_pending.value == 0)",
		   cur_cycle);
	 $display ("    ", fshow (wr_resp));
	 $finish (1);    // Assertion failure
      end

      ctr_wr_rsps_pending.decr;

      if (wr_resp.bresp != axi4_resp_okay) begin
	 // TODO: need to raise a non-maskable interrupt (NMI) here
	 $display ("%0d: MMIO_AXI4_Adapter.rl_discard_write_rsp: fabric response error: exit", cur_cycle);
	 $display ("    ", fshow (wr_resp));
	 $finish (1);
      end
      else begin
	 let rsp = MMIODataPRs {valid: True, data: 0};
	 f_rsps_to_core.enq (rsp);
      end
   endrule

   // ================================================================
   // This adapter should only receive Ld/St requests, no Inst or AMO reqs.

   function Bool fn_is_Ld_or_St (MMIOCRq  req);
      return case (req.func) matches
		Ld     : True;
		St     : True;
		default: False;
	     endcase;
   endfunction

   rule rl_handle_non_Ld_St (! fn_is_Ld_or_St (f_reqs_from_core.first));
      let req <- pop (f_reqs_from_core);

      $display ("%0d: ERROR: MMIO_AXI4_Adapter.rl_handle_non_Ld_St",
		cur_cycle);
      $display ("    ", fshow (req));
      $finish (1);    // Assertion failure
   endrule

   // ================================================================
   // INTERFACE

   method Action reset;
      ctr_wr_rsps_pending.clear;
   endmethod

   interface Server core_side = toGPServer (f_reqs_from_core, f_rsps_to_core);

   // Fabric master interface for IO
   interface mmio_master = master_xactor.axi_side;
endmodule

// ================================================================

endpackage
