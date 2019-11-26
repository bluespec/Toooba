/*-
 * Copyright (c) 2018 Jack Deeley
 * Copyright (c) 2018-2019 Peter Rugg
 * All rights reserved.
 *
 * This software was developed by SRI International and the University of
 * Cambridge Computer Laboratory (Department of Computer Science and
 * Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
 * DARPA SSITH research programme.
 *
 * @BERI_LICENSE_HEADER_START@
 *
 * Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
 * license agreements.  See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.  BERI licenses this
 * file to you under the BERI Hardware-Software License, Version 1.0 (the
 * "License"); you may not use this file except in compliance with the
 * License.  You may obtain a copy of the License at:
 *
 *   http://www.beri-open-systems.org/legal/license-1-0.txt
 *
 * Unless required by applicable law or agreed to in writing, Work distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations under the License.
 *
 * @BERI_LICENSE_HEADER_END@
 */

package Verifier;

import RVFI_DII :: *;
import ISA_Decls   :: *;
import CPU_Globals :: *;
`ifdef ISA_CHERI
import CHERICap :: *;
import CHERICC_Fat :: *;
`endif

`ifdef RVFI
// This function relies on info that is only passed in RVFI-mode.

Bit#(32) ecall_insn = 32'h73;

function RVFI_DII_Execution #(XLEN,MEMWIDTH) getRVFIInfoCondensed(
            // Using the full data gives us access to rd_valid and a few other fields.
            Data_Stage2_to_Stage3   data_s2_s3,
            WordXL                  trapPC,
            Bit#(64)                order,
            Bool                    isTrap,
            Exc_Code                exc,
            Bool                    handler,
            Bool                    halted
            );
            
    Data_RVFI_Stage2 s2 = data_s2_s3.info_RVFI_s2;
    Data_RVFI_Stage1 s1 = s2.stage1;
    
    return RVFI_DII_Execution {
        rvfi_order:     order,
        // Not all traps are relevant in the Clifford-RVFI framework, e.g. page faults.
        rvfi_trap:      isTrap,
        rvfi_halt:      (halted) ? True : (data_s2_s3.instr == ecall_insn),
        rvfi_intr:      handler,
        rvfi_insn:      data_s2_s3.instr,
        rvfi_rs1_addr:  s1.rs1_addr,
        rvfi_rs2_addr:  s1.rs2_addr,
        rvfi_rd_addr:   data_s2_s3.rd_valid ? data_s2_s3.rd : 0,
        rvfi_rs1_data:  s1.rs1_data,
        rvfi_rs2_data:  s1.rs2_data,
`ifdef ISA_CHERI
        rvfi_rd_wdata:  data_s2_s3.rd == 0 ? 0 : getAddr(data_s2_s3.rd_val),
`else
        rvfi_rd_wdata:  data_s2_s3.rd == 0 ? 0 : data_s2_s3.rd_val,
`endif
        rvfi_pc_rdata:  data_s2_s3.pc,
        rvfi_pc_wdata:  isTrap ? trapPC : s1.pc_wdata,
        rvfi_mem_wdata: s1.mem_wdata,
        rvfi_mem_addr:  s1.mem_addr,
        rvfi_mem_rmask: s2.mem_rmask,
        rvfi_mem_wmask: s2.mem_wmask,
`ifdef ISA_CHERI
        rvfi_mem_rdata: getAddr(data_s2_s3.rd_val)
`else
        rvfi_mem_rdata: data_s2_s3.rd_val
`endif
    };
endfunction : getRVFIInfoCondensed
            
            
            
function RVFI_DII_Execution #(XLEN,MEMWIDTH) getRVFIInfoS1 (
            Data_Stage1_to_Stage2   data_s1_s2,
            WordXL                  next_pc,
            Bit#(64)                order,
            Bool                    isTrap,
            Exc_Code                exc,
            Bool                    handler,
            Bool                    halted
            );
    
    Data_RVFI_Stage1 s1 = data_s1_s2.info_RVFI_s1;
    
    return RVFI_DII_Execution {
        rvfi_order:     order,
        rvfi_trap:      isTrap,
        rvfi_halt:      (halted) ? True : (data_s1_s2.instr == ecall_insn),
        rvfi_intr:      handler,
        rvfi_insn:      data_s1_s2.instr,
        rvfi_rs1_addr:  s1.rs1_addr,
        rvfi_rs2_addr:  s1.rs2_addr,
        rvfi_rs1_data:  s1.rs1_data,
        rvfi_rs2_data:  s1.rs2_data,
        rvfi_pc_rdata:  data_s1_s2.pc,
        rvfi_pc_wdata:  isTrap ? next_pc : s1.pc_wdata,
        rvfi_mem_addr:  s1.mem_addr,
        // Although we know what rd *would* be, the fact that we're using this function
        // means we can't have actually written to it.
        rvfi_rd_addr:   0,
        rvfi_rd_wdata:  0,
        rvfi_mem_rmask: 0,
        rvfi_mem_wmask: 0,
        rvfi_mem_wdata: 0,
        rvfi_mem_rdata: 0
    };
endfunction : getRVFIInfoS1

function Bit #(TDiv#(MEMWIDTH,8)) getMemMask(Bit #(3) width_code, Bit #(XLEN) addr);
    Bit #(5) width = 5'b1 << width_code;
    Bit #(TDiv#(MEMWIDTH,8)) result = truncate((9'b1 << width) - 9'b1);
    return result;
endfunction : getMemMask

`endif

endpackage

