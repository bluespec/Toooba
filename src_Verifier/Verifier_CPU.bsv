/*-
 * Copyright (c) 2018 Jack Deeley
 * Copyright (c) 2018 Peter Rugg
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

package Verifier_CPU;

//export mkVerifier_CPU, Verif_IFC(..);

import Memory           :: *;
import GetPut           :: *;
import ClientServer     :: *;

import Verifier         :: *;
import ISA_Decls        :: *;
import RVFI_DII         :: *;

import Core             :: *;
import Verif_IFC        :: *;
import Fabric_Defs      :: *;
import AXI4_Lite_Types  :: *;
import CPU_Globals      :: *;
import FIFOF            :: *;

module mkVerifier_CPU #(parameter Bit#(64) pc_reset_value) (Verif_IFC);
    
    // The core we are going to verify
    Core core <- mkCore(0);

    rule displayData;
        
        RVFI_DII_Execution #(XLEN) x <- core.to_verifier.get;
        $display("[[0x%4h]] insn:0x%8h, pc:0x%8h, rd:0x%8h, mem_addr:0x%8h, mem_wdata:0x%8h",
                    x.rvfi_order,x.rvfi_insn,x.rvfi_pc_rdata[31:0],x.rvfi_rd_wdata[31:0],
                    x.rvfi_mem_addr[31:0],x.rvfi_mem_wdata[31:0]);
         //$display("[[%4d]] pc: 0x%0h, insn: 0x%0h, wmask: 0x%0h, rmask: 0x%0h,
         //           mem_rdata: 0x%0h, rs1_data: 0x%0h, rs2_data: 0x%0h", x.rvfi_order,  
         //           x.rvfi_pc_rdata, x.rvfi_insn, x.rvfi_mem_wmask, x.rvfi_mem_rmask,   
         //           x.rvfi_mem_rdata, x.rvfi_rs1_data, x.rvfi_rs2_data);             
    endrule

    interface RVFI_DII_Server rvfi_dii_server = core.rvfi_dii_server;
        
    interface iCacheToParent = core.iCacheToParent;
    interface dCacheToParent = core.dCacheToParent;
    interface tlbToMem       = core.tlbToMem;
    
endmodule : mkVerifier_CPU

endpackage
