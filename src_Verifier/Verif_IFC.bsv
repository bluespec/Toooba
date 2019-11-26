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

package Verif_IFC;

import Memory           :: *;
import GetPut           :: *;
import ClientServer     :: *;

import Verifier         :: *;
import ISA_Decls        :: *;
import RVFI_DII         :: *;

import Fabric_Defs      :: *;
import AXI4_Lite_Types  :: *;
import CPU_Globals      :: *;

interface Verif_IFC;
    // Standard CPU interfaces that we will pass through

    // coherent caches to LLC
    interface ChildCacheToParent#(L1Way, void) dCacheToParent;
    interface ChildCacheToParent#(L1Way, void) iCacheToParent;
    // DMA to LLC
    interface TlbMemClient tlbToMem;
    
    interface Server #(Token, Token)  hart0_server_reset;
    
    method Action external_interrupt_req (Bool set_not_clear);
    method Action timer_interrupt_req (Bool set_not_clear);
    method Action software_interrupt_req (Bool set_not_clear);

    interface Toooba_RVFI_DII_Server rvfi_dii_server;
endinterface

endpackage
