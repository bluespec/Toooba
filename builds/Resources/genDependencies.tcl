#! /usr/bin/env bluetcl
#
# Copyright (c) 2020 Alexandre Joannou
# All rights reserved.
#
# This software was developed by SRI International and the University of
# Cambridge Computer Laboratory (Department of Computer Science and
# Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
# DARPA SSITH research programme.
#
# @BERI_LICENSE_HEADER_START@
#
# Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
# license agreements.  See the NOTICE file distributed with this work for
# additional information regarding copyright ownership.  BERI licenses this
# file to you under the BERI Hardware-Software License, Version 1.0 (the
# "License"); you may not use this file except in compliance with the
# License.  You may obtain a copy of the License at:
#
#   http://www.beri-open-systems.org/legal/license-1-0.txt
#
# Unless required by applicable law or agreed to in writing, Work distributed
# under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
# CONDITIONS OF ANY KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations under the License.
#
# @BERI_LICENSE_HEADER_END@
#

namespace import ::Bluetcl::*

# there does not seam to be a nice "getopt" in tcl, so for now, falling back on:
#
# - env variables:
# BSC_PATH
# BSC_DEFINES
# BSC_BUILDDIR
# BSC_TOPFILE
# OUTPUTFILE
#
# - if argv is non empty, each arg is passed individually to a flags set command
#   (that is, can't pass flags that expect arguments themselves like -D XXX)
################################################################################

if { [info exists env(BSC_PATH)] } {
  set path $env(BSC_PATH)
} else {
  #set path ".:+"
  set path "../../libs/BlueStuff:../../libs/BlueStuff/AXI:../../libs/BlueStuff/BlueUtils:../../libs/BlueStuff/BlueBasics:../../src_Core/RISCY_OOO/procs/RV64G_OOO:../../src_Core/RISCY_OOO/procs/lib:../../src_Core/RISCY_OOO/coherence/src:../../src_Core/RISCY_OOO/fpgautils/lib:../../src_Core/RISCY_OOO/connectal/bsv:../../src_Core/RISCY_OOO/connectal/tests/spi:../../src_Core/RISCY_OOO/connectal/lib/bsv:../../src_Core/RISCY_OOO/../../src_Verifier:../../src_Core/RISCY_OOO/../../src_Verifier/BSV-RVFI-DII:../../src_Core/RISCY_OOO/../CHERI:../../src_Core/RISCY_OOO/../../libs/cheri-cap-lib:../../src_Core/CPU:../../src_Core/ISA:../../src_Core/Core:../../src_Core/PLIC:../../src_Core/Debug_Module:../../src_Core/BSV_Additional_Libs:../../src_Testbench/Top:../../src_Testbench/SoC:../../libs/TagController/TagController:../../libs/TagController/TagController/CacheCore:+"
}

if { [info exists env(BSC_DEFINES)] } {
  set defines [split $env(BSC_DEFINES)]
} else {
  #set defines {}
  set defines [split "RISCV RV64 ISA_PRIV_M ISA_PRIV_U ISA_PRIV_S SV39 ISA_I ISA_M ISA_A ISA_F ISA_D ISA_FD_DIV ISA_C SHIFT_BARREL MULT_SYNTH Near_Mem_Caches FABRIC64 CAP128 MEM64 BSIM CORE_SMALL NUM_CORES=1 CACHE_SMALL XILINX_FP_FMA_LATENCY=3 XILINX_INT_MUL_LATENCY=2 USE_BSV_BRAM_SYNC_FIFO"]
}

if { [info exists env(BSC_BUILDDIR)] } {
  set builddir $env(BSC_BUILDDIR)
} else {
  set builddir "build"
}

if { [info exists env(BSC_TOPFILE)] } {
  set topfile $env(BSC_TOPFILE)
} else {
  set topfile "Top.bsv"
}

if { [info exists env(OUTPUTFILE)] } {
  set outputfile $env(OUTPUTFILE)
} else {
  set outputfile ".depends.mk"
}

# debug prints
#puts "path: $path"
#puts "defines: $defines"
#puts "builddir: $builddir"
#puts "topfile: $topfile"
#puts "outputfile: $outputfile"
#puts "argv: $argv"

# setting compiler flags
################################################################################

flags set -p $path
flags set -bdir $builddir
flags set -verilog
foreach i $defines {
  flags set -D $i
}
if { $argc > 0 } { foreach i $argv { flags set $i } }

# output of "depend make" seams to be of the form
# { {tgtA {depA0 depA1 ...}}
#   {tgtB {depB0 depB1 ...}}
#   ...
# }
# reformating it for make
################################################################################
try {
  set res [depend make $topfile]
} finally {
  if { [info exists res] } {
    set fd [open $outputfile w]
    foreach i $res {
      set tgt [lindex $i 0]
      set deps [join [lindex $i 1]]
      puts $fd [append tgt ": " $deps]
    }
    close $fd
    puts "generated make dependency rules for \"$topfile\" in: $outputfile"
  } else {
    puts "could not generate make dependency rules for \"$topfile\""
  }
}
