// Copyright (c) 2019 Bluepec, Inc
//
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
// This package implements utility functions used by the floating point
// related logic
package FP_Utils;
import Types::*;
import FloatingPoint::*;

function FloatingPoint#(e,m) canonicalNaN = FloatingPoint{sign: False, exp: '1, sfd: 1 << (valueof(m)-1)};

// nanbox-ing and its inverse (unbox-ing)
// If the raw bits are nan-boxed, the fv_nanbox(fv_unbox) are identity
// functions. However, if the raw input was not properly nanboxed, then the
// output would be a canonical NaN

// Take a single precision value and nanboxes it to be able to write it to a
// 64-bit FPR register file. This is necessary if single precision operands
// used with a register file capable of holding double precision values
function Bit #(64) fv_nanbox (Bit #(64) x);
   Bit #(64) fill_bits = (64'h1 << 32) - 1;  // [31: 0] all ones
   Bit #(64) fill_mask = (fill_bits << 32);  // [63:32] all ones
   return (x | fill_mask);
endfunction
function MemTaggedData fv_nanbox_MemTaggedData (MemTaggedData x) =
  MemTaggedData { tag: x.tag
                , data: dataToMemData(fv_nanbox(memDataToData(x.data))) };

// Take a 64-bit value and check if it is properly nanboxed if operating in a DP
// capable environment. If not properly nanboxed, return canonicalNaN32
function Float fv_unbox (Bit #(64) x);
//`ifdef ISA_D
   if (x [63:32] == 32'hffffffff)
      return (unpack (x [31:0]));
   else
      return (canonicalNaN);
//`else
//   return (unpack (x [31:0]));
//`endif
endfunction
endpackage
