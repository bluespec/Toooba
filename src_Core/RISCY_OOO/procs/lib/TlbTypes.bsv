
// Copyright (c) 2017 Massachusetts Institute of Technology
//
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

`include "ProcConfig.bsv"

import Vector::*;
import Types::*;
import ProcTypes::*;

// processor req/resp with I/D TLB
typedef struct{
    Addr  addr;
    Bool  write;
    Bool  capStore;
    Bool  potentialCapLoad;
} TlbReq deriving(Eq, Bits, FShow);
typedef Tuple3#(Addr, Maybe#(Exception), Bool) TlbResp;

// non-blocking DTLB
typedef `DTLB_REQ_NUM DTlbReqNum;
typedef Bit#(TLog#(DTlbReqNum)) DTlbReqIdx;

// non-blocking L2 TLB
typedef `L2TLB_REQ_NUM L2TlbReqNum;
typedef Bit#(TLog#(L2TlbReqNum)) L2TlbReqIdx;

// Only for Sv39
typedef 27 VpnSz;
typedef Bit#(VpnSz) Vpn;
typedef 44 PpnSz;
typedef Bit#(PpnSz) Ppn;
typedef 12 PageOffsetSz; // 4KB basic page
typedef Bit#(PageOffsetSz) PageOffset;
typedef 9 VpnIdxSz; // Vpn is broken down to 3 indexes to 3 levels of page table
typedef Bit#(VpnIdxSz) VpnIdx;
typedef Bit#(2) PageWalkLevel; // 2: 1GB page, 1: 2MB page, 0: 4KB page
typedef 3 NumPageWalkLevels;
PageWalkLevel maxPageWalkLevel = fromInteger(valueof(NumPageWalkLevels) - 1);

typedef struct {
    Bool dirty;
    Bool accessed;
    Bool global;
    Bool user;
    Bool executable;
    Bool writable;
    Bool readable;
} PTEType deriving (Bits, Eq, FShow);

typedef struct {
    Bool cap_writable;
    Bool cap_readable;
    Bool cap_dirty;
    Bool cap_read_mod;
    Bool cap_read_gen;
} PTEUpperType deriving (Bits, Eq, FShow);

typedef struct {
    PTEUpperType pteUpperType;
    Bit#(5) reserved;
    Ppn ppn;
    Bit#(2) reserved_sw; // reserved for supervisor software
    PTEType pteType;
    Bool valid;
} PTESv39 deriving(Bits, Eq, FShow);

// TLB entry
typedef struct {
    Vpn           vpn;
    Ppn           ppn;
    PTEType       pteType;
    PTEUpperType  pteUpperType;
    PageWalkLevel level;
    Asid          asid;
} TlbEntry deriving (Bits, Eq, FShow);

// SV39 translate
function Vpn getVpn(Addr addr) = addr[38:12];

function PageOffset getPageOffset(Addr addr) = truncate(addr);

// All the upper bits should be equal for a valid SV39 virtual address.
function Bool validVirtualAddress(Addr addr) = ((addr[63:39] ^ {addr[39],addr[63:40]}) == 0);

function Addr getPTBaseAddr(Ppn basePpn);
    PageOffset offset = 0;
    return zeroExtend({basePpn, offset});
endfunction

`ifdef SECURITY
// Check if an access from enclave or security monitor is out of its protection
// domain. This should always return false for normal program or OS, becuase in
// vminfo of OS/normal program, evbase = 0xfff...fff and evmask = 0
function Bool outOfProtectionDomain(VMInfo vm_info, Addr vaddr);
    // Default value for when the program requiring the translation is not protected
    if (vm_info.sanctum_evbase == maxBound && vm_info.sanctum_evmask == 0) return False;
    // If it is protected, then the size of the protection domain is a power of
    // 2 starting at sanctum_evbase
    else return ((vaddr & vm_info.sanctum_evmask) != vm_info.sanctum_evbase );
endfunction

// get the bitmask for accessed DRAM regions
// FIXME This code assumes 64 x 32MB regions
function Addr getAddrRegions(Addr addr, Bool isLeaf, PageWalkLevel level) provisos (
    Add#(0, 6, `LOG_DRAM_REGION_NUM), // 6 regions
    Add#(0, 25, `LOG_DRAM_REGION_SIZE) // 25 regions
);
    Addr res = (1 << (addr[30:25]));
    if (isLeaf && (level == 2)) begin // giga pages cross multiple regions
        res = ((addr[30] == 1'b1) ? 64'hFFFFFFFF00000000 : 64'h00000000FFFFFFFF); // Assume 64 x 32-MB regions
    end
    return res;
endfunction
`endif

function Addr getPTEAddr(Addr baseAddr, Vpn vpn, PageWalkLevel level);
    Vector#(NumPageWalkLevels, VpnIdx) vpnVec = unpack(vpn); // index 0 is LSB
    return baseAddr + (zeroExtend(vpnVec[level]) << 3); // PTE is 2^3 bytes
endfunction

function Bool isLeafPTE(PTEType t);
    return t.executable || t.readable || t.writable;
endfunction

function Addr translate(Addr addr, Ppn ppn, PageWalkLevel level);
    return zeroExtend(case (level)
        0: {ppn, getPageOffset(addr)}; // 4KB page
        1: {ppn[43:9], addr[20:0]};   // 2MB page
        2: {ppn[43:18], addr[29:0]};  // 1GB page
        default: 0; // should not happen
    endcase);
endfunction

function Vpn getMaskedVpn(Vpn vpn, PageWalkLevel level);
    return (case (level)
        0: (vpn);
        1: ((vpn >> 9) << 9);   // 2MB mask
        2: ((vpn >> 18) << 18); // 1GB mask
        default: 0; // should not happen
    endcase);
endfunction

function Ppn getMaskedPpn(Ppn ppn, PageWalkLevel level);
    return (case (level)
        0: (ppn);
        1: ((ppn >> 9) << 9);   // 2MB mask
        2: ((ppn >> 18) << 18); // 1GB mask
        default: 0; // should not happen
    endcase);
endfunction

function Bool isPpnAligned(Ppn ppn, PageWalkLevel level);
    return (case(level)
        0: True;
        1: (ppn[8:0] == 0);
        2: (ppn[17:0] == 0);
        default: False;
    endcase);
endfunction

typedef enum {
    InstFetch,
    DataLoad,
    DataStore // also contain DataLoad
} TlbAccessType deriving(Bits, Eq, FShow);

typedef struct {
    Bool allowed;
    Exception excCode; // Only defined if !allowed
    Bool allowCap; // Whether we can load caps
} TlbPermissionCheck deriving(Bits, Eq, FShow);

function TlbPermissionCheck hasVMPermission(
    VMInfo vm_info,
    PTEType pte_type, PTEUpperType pte_upper_type,
     Ppn ppn, PageWalkLevel level,
    TlbAccessType access,
    Bool capStore, Bool potentialCapLoad
);
    // try to find any page fault
    Bool fault = False;

    // check if we are still in sv39
    if(!vm_info.sv39) begin
        fault = True;
    end

    // check PTE itself is well-formed or not
    if(pte_type.writable && !pte_type.readable) begin
        fault = True; // page writable but not readable
    end
    if(!isPpnAligned(ppn, level)) begin
        fault = True; // unaligned super page
    end
    if ((!pte_upper_type.cap_readable && pte_upper_type.cap_read_gen) ||
        (pte_upper_type.cap_readable && !pte_upper_type.cap_read_mod &&
        pte_upper_type.cap_read_gen)) begin
        fault = True;
    end

    // check permission related to user page
    if(pte_type.user) begin
        // S mode may not access user page. We need to consider mstatus.sum
        // bit. XXX Spike will raise page fault in case S-mode inst-fetch even
        // when mstatus.sum is set. We follow spike here.
        if (vm_info.prv == prvS &&
            (access == InstFetch || !vm_info.userAccessibleByS)) begin
            fault = True;
        end
    end
    else begin
        // U mode cannot access non-user page
        if(vm_info.prv == prvU) begin
            fault = True;
        end
    end

    Exception excCode = excLoadPageFault; // Unused default.  Just choose one valid option.

    // check execute/read/write permission
    case(access)
        InstFetch: begin
            excCode = excLoadPageFault;
            if(!pte_type.executable) begin
                fault = True;
            end
        end
        DataLoad: begin
            excCode = excLoadPageFault;
            // need to consider mstatus.mxr bit
            if (!pte_type.readable &&
                !(pte_type.executable && vm_info.exeReadable)) begin
                fault = True;
            end
            if (potentialCapLoad) begin
                if (!fault) excCode = excLoadCapPageFault;
                // load traps if page not cap readable and using cap_read_mod set
                if (!pte_upper_type.cap_readable && pte_upper_type.cap_read_mod) begin
                    fault = True;
                end
                // perform generation check
                if (pte_upper_type.cap_read_mod
                    && ((pte_type.user ? vm_info.globalCapLoadGenU : vm_info.globalCapLoadGenS)
                       != pack(pte_upper_type.cap_read_gen))) begin
                    fault = True;
                end
            end
        end
        DataStore: begin
            excCode = excStorePageFault;
            // store requires page to be both readable and writable
            if(!(pte_type.readable && pte_type.writable)) begin
                fault = True;
            end
            else if(capStore && !pte_upper_type.cap_writable) begin
                if (!fault) excCode = excStoreCapPageFault;
                fault = True;
            end
        end
    endcase

    TlbPermissionCheck ret = TlbPermissionCheck {
        allowed:  !fault,
        excCode:  excCode,
        allowCap: pte_upper_type.cap_readable};

    if (!fault) begin
        // check if accessed or dirty bit needs to be set
        if(capStore && access == DataStore && !pte_upper_type.cap_dirty) begin
            ret.allowed = False;
            ret.excCode = excStoreCapPageFault;
        end
        if(access == DataStore && !pte_type.dirty) begin
            ret.allowed = False;
            ret.excCode = excStorePageFault;
        end
        if(!pte_type.accessed) begin
            ret.allowed = False;
            ret.excCode = access == DataStore ? excStorePageFault : excLoadPageFault;
        end
    end

    return ret;
endfunction
