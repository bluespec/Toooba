###  -*-Makefile-*-

REPO ?= ../..
ARCH ?= RV64ACDFIMSUxCHERI

# ================================================================
# Path to RISCY-OOO sources not included in Common

EXTRA_DIRS = $(RISCY_HOME)/../../src_Verifier:$(RISCY_HOME)/../../src_Verifier/BSV-RVFI-DII

# ================================================================
# RISC-V config macros passed into Bluespec 'bsc' compiler

BSC_COMPILATION_FLAGS += \
	-D RV64 \
	-D ISA_PRIV_M  -D ISA_PRIV_U  -D ISA_PRIV_S  \
	-D SV39  \
	-D ISA_I  -D ISA_M  -D ISA_A  -D ISA_F  -D ISA_D  -D ISA_FD_DIV  -D ISA_C  \
	-D SHIFT_BARREL    \
	-D MULT_SYNTH    \
	-D Near_Mem_Caches    \
	-D FABRIC64    \
	-D CheriBusBytes=8 \
	-D CheriMasterIDWidth=1 \
	-D CheriTransactionIDWidth=6 \
	-D CAP128 -D BLUESIM \
	-D MEM64 \
	-D RISCV \
	-D INCLUDE_GDB_CONTROL

# Default ISA test

TEST ?= rv64ui-p-add

#================================================================
# Common boilerplate rules

include $(REPO)/builds/Resources/Include_Common.mk

#================================================================
# Parameter settings for MIT RISCY

include $(REPO)/builds/Resources/Include_RISCY_Config.mk

#================================================================
# Makefile rules for building for specific simulator: bluesim

include $(REPO)/builds/Resources/Include_bluesim.mk
