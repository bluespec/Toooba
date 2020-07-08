// Copyright (c) 2020 Bluespec, Inc. All Rights Reserved.
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

package Trace_Data2_to_Trace_Data;

// ================================================================
// This package defines a module to transform a stream of Trace_Data2
// to a stream of (serial_num, Trace_Data)

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
   // From Toooba's CommitStage
   interface Put #(Trace_Data2) in;

   // To Trace Encoder
   interface Get #(Tuple2 #(Bit #(64), Trace_Data)) out;
endinterface

// ================================================================

(* synthesize *)
module mkTrace_Data2_to_Trace_Data (Trace_Data2_to_Trace_Data_IFC);

   Integer verbosity = 1;    // for debugging

   // Input stream
   FIFOF #(Trace_Data2) f_in <- mkFIFOF;

   // Output stream
   FIFOF #(Tuple2 #(Bit #(64), Trace_Data))  f_out <- mkFIFOF;

   // ================================================================
   // Transformer: Trace_Data2 -> (serial_num, Trace_Data)

   function ActionValue #(Tuple2 #(Bit #(64), Trace_Data)) fav_td2_to_td (Trace_Data2  td2);
      actionvalue
         let serial_num     = td2.serial_num;
         Trace_Data td      = ?;
         ISize isize        = ((td2.orig_inst [1:0] == 2'b11) ? ISIZE32BIT : ISIZE16BIT);
         Addr  fall_thru_PC = td2.pc + ((td2.orig_inst [1:0] == 2'b11) ? 4 : 2);

         Bit #(3) st_funct3 = (td2.store_data_BE [7] ? 3'b_011               // Doubleword
                               : (td2.store_data_BE [3] ? 3'b_010            // Word
                                  : (td2.store_data_BE [1] ? 3'b_001         // HalfWord
                                     :                         3'b000)));    // Byte

         Bit #(5) gpr_rd = 0;
         if (td2.dst matches tagged Valid (tagged Gpr .r)) gpr_rd = r;

         if (serial_num == 0)
            td = mkTrace_RESET;

         else if (td2.maybe_csr_upd matches tagged Valid { .csr_addr, .csr_value })
            td = mkTrace_CSR_WRITE (csr_addr, csr_value);

         else if (isValid (td2.trap))
            td = mkTrace_TRAP (td2.tvec,
                               isize,
                               td2.orig_inst,
                               td2.prv,
                               td2.status,
                               td2.cause,
                               td2.epc,
                               td2.tval);

         else if (td2.ppc_vaddr_csrData matches tagged PPC .target_addr
                  &&&   (td2.iType == Br))
            td = mkTrace_OTHER (target_addr, isize, td2.orig_inst);

         else if (td2.ppc_vaddr_csrData matches tagged PPC .target_addr
                  &&& (   (td2.iType == J)
                       || (td2.iType == Jr)))
            td = mkTrace_I_RD (target_addr,
                               isize,
                               td2.orig_inst,
                               gpr_rd,
                               td2.dst_data);   // return-pc

         else if (   (td2.iType == Alu)
                  || (td2.iType == Auipc))
            td = mkTrace_I_RD (fall_thru_PC,
                               isize,
                               td2.orig_inst,
                               gpr_rd,
                               td2.dst_data);   // rd_val

         else if (td2.dst matches tagged Valid (tagged Fpu .fpr_rd)
                  &&& (td2.iType == Fpu))
            td = mkTrace_F_FRD (fall_thru_PC,
                                isize,
                                td2.orig_inst,
                                fpr_rd,
                                td2.dst_data,    // rdval
                                td2.fflags,
                                td2.mstatus);   // [FX] updated

         else if (td2.iType == Fpu)
            td = mkTrace_F_GRD (fall_thru_PC,
                                isize,
                                td2.orig_inst,
                                gpr_rd,
                                td2.dst_data,    // rdval
                                td2.fflags,
                                td2.mstatus);    // [FX] updated

         else if (td2.ppc_vaddr_csrData matches tagged VAddr .eaddr
                  &&& td2.dst matches tagged Valid (tagged Fpu .fpr_rd)
                  &&& (td2.iType == Ld))
            td = mkTrace_F_LOAD (fall_thru_PC,
                                 isize,
                                 td2.orig_inst,
                                 fpr_rd,
                                 td2.dst_data,    // rd_val
                                 eaddr,
                                 td2.mstatus);

         else if (td2.ppc_vaddr_csrData matches tagged VAddr .eaddr
                  &&& (td2.iType == Ld))
            td = mkTrace_I_LOAD (fall_thru_PC,
                                 isize,
                                 td2.orig_inst,
                                 gpr_rd,
                                 td2.dst_data,    // rd_val
                                 eaddr);

         else if (td2.ppc_vaddr_csrData matches tagged VAddr .eaddr
                  &&& (td2.iType == St))
            td = mkTrace_I_STORE (fall_thru_PC,
                                  st_funct3,
                                  isize,
                                  td2.orig_inst,
                                  td2.store_data,    // rs2_val
                                  eaddr);

         else if (td2.ppc_vaddr_csrData matches tagged CSRData .csr_data
                  &&& (td2.iType == Csr))
            begin
               Bit #(3) funct3     = td2.orig_inst [14:12];
               Bit #(5) rs1_or_imm = td2.orig_inst [19:15];
               Bool     csr_valid = False;
               CSR_Addr csr_addr  = 0;
               if (td2.csr matches tagged Valid .c) begin
                  csr_addr  = pack (c);
                  csr_valid = (   (funct3 [1:0] == 2'b01)    // CSRRW, CSRRWI
                               || (   (   (funct3 [1:0] == 2'b10)     // CSRRS, CSRRSI
                                       || (funct3 [1:0] == 2'b11))    // CSRRC, CSRRCI
                                   && (rs1_or_imm != 0)));
               end
               td = mkTrace_CSRRX (fall_thru_PC,
                                   isize,
                                   td2.orig_inst,
                                   gpr_rd,
                                   td2.dst_data,    // rdval
                                   csr_valid,
                                   csr_addr,
                                   csr_data,
                                   // For CSR writes to FFLAGS/FRM/FCSR, also changes MSTATUS
                                   td2.will_dirty_fpu_state,
                                   td2.mstatus);
            end

         else if (   (td2.iType == Mret)
                  || (td2.iType == Sret))
            td = mkTrace_RET (td2.pc, isize, td2.orig_inst, td2.prv, td2.status);

         else if (   (td2.iType == Fence)
                  || (td2.iType == FenceI)
                  || (td2.iType == SFence)
                  || (td2.iType == Ecall)    // Handled by TRAP above?
                  || (td2.iType == Ebreak))  // Handled by TRAP above?
            td = mkTrace_OTHER (fall_thru_PC, isize, td2.orig_inst);

         else if (td2.ppc_vaddr_csrData matches tagged VAddr .eaddr
                  &&& (   (td2.iType == Amo)
                       || (td2.iType == Lr)
                       || (td2.iType == Sc)))
            td = mkTrace_AMO (fall_thru_PC,
                              st_funct3,
                              isize,
                              td2.orig_inst,
                              gpr_rd,
                              td2.dst_data,      // rd_val
                              td2.store_data,    // rs2_val
                              eaddr);

         else if (   (td2.iType == Unsupported)
                  || (td2.iType == Nop)
                  || (td2.iType == Interrupt))
            td = mkTrace_OTHER (fall_thru_PC, isize, td2.orig_inst);

         else begin
            if (verbosity > 0) begin
               $display ("    fav_td2_to_td: TBD: Unknown iType: Using mkTrace_OTHER for now");
               $display ("      ", fshow (td2));
            end
            td = mkTrace_OTHER (fall_thru_PC, isize, td2.orig_inst);
         end
         return tuple2 (serial_num, td);
      endactionvalue
   endfunction

   // ================================================================
   // RULES

   rule rl_td2_to_td;
      Trace_Data2 td2 <- pop (f_in);

      if (verbosity > 1)
         $display ("%0d: %m.rl_td2_to_td: serial_num:%0d  PC:0x%0h  instr:0x%08h",
                   cur_cycle, td2.serial_num, td2.pc, td2.orig_inst,
                   "  iType:", fshow (td2.iType));

      match { .serial_num, .td } <- fav_td2_to_td (td2);
      f_out.enq (tuple2 (serial_num, td));
   endrule

   // ================================================================
   // INTERFACE

   interface in  = toPut (f_in);
   interface out = toGet (f_out);
endmodule

// ================================================================

endpackage
