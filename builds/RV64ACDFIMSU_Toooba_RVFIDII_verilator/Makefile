###  -*-Makefile-*-

# ================================================================
# Path to RISCY-OOO sources

RISCY_HOME ?= ../../src_Core/RISCY_OOO
# RISCY_HOME ?= $(HOME)/Projects/RISCV/MIT-riscy/riscy-OOO

RISCY_DIRS = $(RISCY_HOME)/procs/RV64G_OOO:$(RISCY_HOME)/procs/lib:$(RISCY_HOME)/coherence/src:$(RISCY_HOME)/fpgautils/lib

CONNECTAL_DIRS = $(RISCY_HOME)/connectal/bsv:$(RISCY_HOME)/connectal/tests/spi:$(RISCY_HOME)/connectal/lib/bsv

RVFI_DII_DIRS = $(RISCY_HOME)/../../src_Verifier:$(RISCY_HOME)/../../src_Verifier/BSV-RVFI-DII

CHERI_DIRS = $(RISCY_HOME)/../../libs/cheri-cap-lib

# ALL_RISCY_DIRS = $(RISCY_DIRS)
ALL_RISCY_DIRS = $(RISCY_DIRS):$(CONNECTAL_DIRS):$(RVFI_DII_DIRS):$(CHERI_DIRS)

# ================================================================

REPO ?= ../..
ARCH ?= RV64ACDFIMSU

# ================================================================
# RISC-V config macros passed into Bluespec 'bsc' compiler

MORE_DEFINES = RV64 \
               ISA_PRIV_M ISA_PRIV_U ISA_PRIV_S \
               SV39 \
               ISA_I ISA_M ISA_A ISA_F ISA_D ISA_FD_DIV ISA_C \
               SHIFT_BARREL \
               MULT_SYNTH \
               Near_Mem_Caches \
               FABRIC64 \
               CAP128 \
               MEM64 \
               RVFI_DII \
               RVFI BLUESIM\
               RISCV
BSC_DEFINES += $(MORE_DEFINES)
BSC_COMPILATION_FLAGS += $(addprefix -D , $(MORE_DEFINES))

# Default ISA test

TEST ?= rv64ui-p-add

#================================================================
# Parameter settings for MIT RISCY

BSC_COMPILATION_FLAGS += -D BSIM \

CACHE_SIZE ?= TEST

include $(REPO)/builds/Resources/Include_RISCY_Config.mk

#================================================================
# Common boilerplate rules

include $(REPO)/builds/Resources/Include_Common.mk

#================================================================
# Makefile rules for building for specific simulator: verilator

include $(REPO)/builds/Resources/Include_verilator.mk
