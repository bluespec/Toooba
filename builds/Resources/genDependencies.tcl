#! /usr/bin/env bluetcl

namespace import ::Bluetcl::*

flags set -p "../RV64ACDFIMSU_Toooba_verilator:../../libs/BlueStuff:../../libs/BlueStuff/AXI:../../libs/BlueStuff/BlueUtils:../../libs/BlueStuff/BlueBasics:../../src_Core/RISCY_OOO/procs/RV64G_OOO:../../src_Core/RISCY_OOO/procs/lib:../../src_Core/RISCY_OOO/coherence/src:../../src_Core/RISCY_OOO/fpgautils/lib:../../src_Core/RISCY_OOO/connectal/bsv:../../src_Core/RISCY_OOO/connectal/tests/spi:../../src_Core/RISCY_OOO/connectal/lib/bsv:../../src_Core/RISCY_OOO/../../src_Verifier:../../src_Core/RISCY_OOO/../../src_Verifier/BSV-RVFI-DII:../../src_Core/RISCY_OOO/../CHERI:../../src_Core/RISCY_OOO/../../libs/cheri-cap-lib:../../src_Core/CPU:../../src_Core/ISA:../../src_Core/Core:../../src_Core/PLIC:../../src_Core/Debug_Module:../../src_Core/BSV_Additional_Libs:../../src_Testbench/Top:../../src_Testbench/SoC:../../libs/TagController/TagController:../../libs/TagController/TagController/CacheCore:+"

flags set -D RISCV
flags set -D RV64
flags set -D ISA_PRIV_M
flags set -D ISA_PRIV_U
flags set -D ISA_PRIV_S
flags set -D SV39
flags set -D ISA_I
flags set -D ISA_M
flags set -D ISA_A
flags set -D ISA_F
flags set -D ISA_D
flags set -D ISA_FD_DIV
flags set -D ISA_C
flags set -D SHIFT_BARREL
flags set -D MULT_SYNTH
flags set -D Near_Mem_Caches
flags set -D FABRIC64
flags set -D CAP128
flags set -D MEM64
#flags set -D RVFI_DII
#flags set -D RVFI
flags set -D BSIM
flags set -D CORE_SMALL
flags set -D NUM_CORES=1
flags set -D CACHE_SMALL
flags set -D XILINX_FP_FMA_LATENCY=3
flags set -D XILINX_INT_MUL_LATENCY=2
flags set -D USE_BSV_BRAM_SYNC_FIFO

flags set -bdir build_dir

set topfile [lindex $argv 0]

foreach i [depend make $topfile] {
  set tgt [lindex $i 0]
  set deps [join [lindex $i 1]]
  puts [append tgt ": " $deps]
}
