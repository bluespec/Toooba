// Copyright (c) 2020 Bluespec, Inc. All Rights Reserved.

package Trace_Data2;

// ================================================================
// Project imports

// ----------------
// From RISCY-OOO

import Types         :: *;
import ProcTypes     :: *;
import ReorderBuffer :: *;

// ================================================================
// This struct has a subset of the fields of struct ToReorderBuffer in
// Toooba/RISCY-OOO, to be encoded and emitted for Tandem
// Verification.

// In RISCY-OOO's CommitStage, when we dequeue (retire) an entry
// (struct ToReorderBuffer), we simply copy out these fields and
// enqueue this struct into a FIFO.  All transformations/encoding for
// TV are done on the dequeue side of the FIFO.  Thus, this should not
// add to the critical path or scheduling requirements of CommitStage.

typedef struct {
   // TV message serial number
   Bit #(64)          serial_num;

   // For asynchronous CSR updates (e.g., MIP change on external interrupt)
   Maybe #(Tuple2 #(Bit #(12), Data))  maybe_csr_upd;

   // Remaining fields relevant only if maybe_csr_upd is Invalid
   Addr               pc;
   Bit #(32)          orig_inst;    // original 16b or 32b instruction ([1:0] will distinguish 16b or 32b)
   IType              iType;
   Maybe#(ArchRIndx)  dst;          // Invalid, GPR or FPR destination ("Rd")
   Data               dst_data;
   Data               store_data;    // For mem instrs that store data
   ByteEn             store_data_BE;
   Maybe #(CSR)       csr;
   Maybe #(Trap)      trap;
   Addr               tval;    // in case of trap
   PPCVAddrCSRData    ppc_vaddr_csrData;
   Bit #(5)           fflags;
   Bool               will_dirty_fpu_state; // True means 2'b11 will be written to FS
   Data               mstatus;    // For Fpu ops, since [MX] bit may have changed

   // Trap updates
   Bit #(2)  prv;
   Addr      tvec;
   Data      status;
   Data      cause;
   Data      epc;
   } Trace_Data2
deriving (Bits, Eq, FShow);

// ================================================================

endpackage
