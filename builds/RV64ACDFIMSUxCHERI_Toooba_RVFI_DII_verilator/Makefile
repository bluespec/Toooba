###  -*-Makefile-*-

# ================================================================
# Path to RISCY-OOO sources not included in Common

EXTRA_DIRS = $(RISCY_HOME)/../../src_Verifier:$(RISCY_HOME)/../../src_Verifier/BSV-RVFI-DII

# ================================================================

REPO ?= ../..
ARCH ?= RV64ACDFIMSUxCHERI

# ================================================================
# RISC-V config macros passed into Bluespec 'bsc' compiler

BSC_COMPILATION_FLAGS += \
	-D RVFI_DII \
	-D RVFI \

# Default ISA test

TEST ?= rv64ui-p-add

#================================================================
# Common boilerplate rules

include $(REPO)/builds/Resources/Include_Common.mk

#================================================================
# Parameter settings for MIT RISCY

CACHE_SIZE ?= TEST

include $(REPO)/builds/Resources/Include_RISCY_Config.mk

#================================================================
# Makefile rules for building for specific simulator: verilator

include $(REPO)/builds/Resources/Include_verilator.mk
