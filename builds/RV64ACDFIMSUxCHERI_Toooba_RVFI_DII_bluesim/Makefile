###  -*-Makefile-*-

# ================================================================
# Path to RISCY-OOO sources not included in Common

EXTRA_DIRS = $(RISCY_HOME)/../../src_Verifier

# ================================================================

REPO ?= ../..
ARCH ?= RV64ACDFIMSUxCHERI

# ================================================================
# RISC-V config macros passed into Bluespec 'bsc' compiler

# "-D RVFI" is set by Include_RISCY_Config
# "-D RVFI_DII" enables injecting insructions, not just logging them,
# and requires the EXTRA_DIRS set above

BSC_COMPILATION_FLAGS += \
	-D RVFI_DII \
	-D CONTRACTS_VERIFY \

# Default ISA test

TEST ?= rv64ui-p-add

#================================================================
# Parameter settings for MIT RISCY, setup paths etc. for Include_Common

CACHE_SIZE ?= TEST
RVFI = true

include $(REPO)/builds/Resources/Include_RISCY_Config.mk

#================================================================
# Common boilerplate rules

include $(REPO)/builds/Resources/Include_Common.mk

#================================================================
# Makefile rules for building for specific simulator: bluesim

include $(REPO)/builds/Resources/Include_bluesim.mk
