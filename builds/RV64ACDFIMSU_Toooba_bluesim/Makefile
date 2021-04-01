###  -*-Makefile-*-

# ================================================================
# Path to RISCY-OOO sources

RISCY_HOME ?= ../../src_Core/RISCY_OOO
# RISCY_HOME ?= $(HOME)/Projects/RISCV/MIT-riscy/riscy-OOO

RISCY_DIRS = $(RISCY_HOME)/procs/RV64G_OOO:$(RISCY_HOME)/procs/lib:$(RISCY_HOME)/coherence/src:$(RISCY_HOME)/fpgautils/lib

CONNECTAL_DIRS = $(RISCY_HOME)/connectal/bsv:$(RISCY_HOME)/connectal/tests/spi:$(RISCY_HOME)/connectal/lib/bsv

# ALL_RISCY_DIRS = $(RISCY_DIRS)
ALL_RISCY_DIRS = $(RISCY_DIRS):$(CONNECTAL_DIRS)

# ================================================================

REPO ?= ../..
ARCH ?= RV64ACDFIMSU

# ================================================================
# RISC-V config macros passed into Bluespec 'bsc' compiler

BSC_COMPILATION_FLAGS += \
	-D RV64 \
	-D ISA_PRIV_M  -D ISA_PRIV_U  -D ISA_PRIV_S  \
	-D SV39  \
	-D ISA_I  -D ISA_M  -D ISA_A  -D ISA_F  -D ISA_D  -D ISA_FD_DIV  -D ISA_C  \
	-D PERFORMANCE_MONITORING \
	-D SHIFT_BARREL    \
	-D MULT_SYNTH    \
	-D Near_Mem_Caches    \
	-D FABRIC64    \


# Default ISA test

TEST ?= rv64ui-p-add

#================================================================
# Parameter settings for MIT RISCY

BSC_COMPILATION_FLAGS += -D BSIM \


include $(REPO)/builds/Resources/Include_RISCY_Config.mk

#================================================================
# Common boilerplate rules

include $(REPO)/builds/Resources/Include_Common.mk

#================================================================
# Makefile rules for building for specific simulator: bluesim

include $(REPO)/builds/Resources/Include_bluesim.mk
