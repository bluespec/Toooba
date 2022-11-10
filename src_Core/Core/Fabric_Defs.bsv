// Copyright (c) 2018-2019 Bluespec, Inc. All Rights Reserved
//-
// RVFI_DII + CHERI modifications:
//     Copyright (c) 2020 Alexandre Joannou
//     All rights reserved.
//
//     This software was developed by SRI International and the University of
//     Cambridge Computer Laboratory (Department of Computer Science and
//     Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
//     DARPA SSITH research programme.
//
//     This work was supported by NCSC programme grant 4212611/RFA 15971 ("SafeBet").
//-

package Fabric_Defs;

// ================================================================
// Defines key parameters of the AXI4/AXI4-Lite system interconnect
// fabric to which the core connects, such as address bus width, data
// bus width, etc.

// ***** WARNING! WARNING! WARNING! *****

// During system integration, these parameters should be checked to be
// identical to the system interconnect settings.  Strong
// type-checking (EXACT match on bus widths) will do this; but some
// languages/tools may silently ignore mismatched widths.

// ================================================================
// BSV lib imports

import AXI4 :: *;
import ISA_Decls_CHERI :: *;

// ================================================================
// Core local Fabric parameters

typedef 3  CoreW_Bus_Num_Masters;
typedef 3  CoreW_Bus_Num_Slaves;

typedef Bit#(TLog #(CoreW_Bus_Num_Masters))  CoreW_Bus_Master_Num;
typedef Bit#(TLog #(CoreW_Bus_Num_Slaves))   CoreW_Bus_Slave_Num;

// ----------------
// Width of fabric 'Id' buses
typedef 4 Wd_CoreW_Bus_MId;
typedef TAdd#(Wd_CoreW_Bus_MId, TLog#(CoreW_Bus_Num_Masters)) Wd_CoreW_Bus_SId;
typedef Wd_CoreW_Bus_SId Wd_MId;

// ----------------
// Width of fabric 'addr' buses
`ifdef FABRIC64
typedef 64   Wd_Addr;
`else
typedef 32   Wd_Addr;
`endif

typedef  Bit #(Wd_Addr)      Fabric_Addr;
typedef  TDiv #(Wd_Addr, 8)  Bytes_per_Fabric_Addr;

Integer  bytes_per_fabric_addr = valueOf (Bytes_per_Fabric_Addr);

// ----------------
// Widths of the main bus data are 128 bits. Peripherals each have a shim
// converting this down to 64 bits (and stripping tags).
// (caches <==> Bus <==> (tag controller) <==> main memory
//               |
//             Periph

typedef 512  Wd_Data;

// ----------------
// Width of fabric 'user' datapaths. Carry capability tags on data lines.
typedef 0 Wd_AW_User;
typedef 0 Wd_B_User;
typedef 0 Wd_AR_User;
typedef TMax#(TDiv#(Wd_Data, CLEN),1) Wd_W_User;
typedef TMax#(TDiv#(Wd_Data, CLEN),1) Wd_R_User;

typedef  TDiv #(Wd_Data, 8)         Bytes_per_Fabric_Data;
Integer  bytes_per_fabric_data = valueOf (Bytes_per_Fabric_Data);

typedef  Bit #(Wd_Data)             Fabric_Data;
typedef  Bit #(TDiv #(Wd_Data, 8))  Fabric_Strb;

// ----------------
typedef 64   Wd_Data_Periph;

typedef 0    Wd_AW_User_Periph;
typedef 0    Wd_W_User_Periph;
typedef 0    Wd_B_User_Periph;
typedef 0    Wd_AR_User_Periph;
typedef 0    Wd_R_User_Periph;

typedef  Bit #(Wd_Data_Periph)             Fabric_Data_Periph;
typedef  Bit #(TDiv #(Wd_Data_Periph, 8))  Fabric_Strb_Periph;
typedef  TDiv #(Wd_Data_Periph, 8)         Bytes_per_Fabric_Data_Periph;

// ----------------
// Number of zero LSBs in a fabric address aligned to the fabric data width

typedef  TLog #(Bytes_per_Fabric_Data)  ZLSBs_Aligned_Fabric_Addr;
Integer  zlsbs_aligned_fabric_addr = valueOf (ZLSBs_Aligned_Fabric_Addr);

// ================================================================
// AXI4 defaults for this project
Bit#(Wd_CoreW_Bus_MId) fabric_corew_bus_default_mid = 0;
Bit#(Wd_MId)     fabric_default_mid     = 0;
AXI4_Burst       fabric_default_burst   = INCR;
AXI4_Lock        fabric_default_lock    = NORMAL;
AXI4_Cache       fabric_default_arcache = arcache_dev_nonbuf;
AXI4_Cache       fabric_default_awcache = awcache_dev_nonbuf;
AXI4_Prot        fabric_default_prot    = axi4Prot(DATA, SECURE, UNPRIV);
AXI4_QoS         fabric_default_qos     = 0;
AXI4_Region      fabric_default_region  = 0;
Bit#(Wd_AW_User) fabric_default_awuser  = 0;
Bit#(Wd_W_User)  fabric_default_wuser   = 0;
Bit#(Wd_B_User)  fabric_default_buser   = 0;
Bit#(Wd_AR_User) fabric_default_aruser  = 0;
Bit#(Wd_R_User)  fabric_default_ruser   = 0;

// ================================================================

endpackage
