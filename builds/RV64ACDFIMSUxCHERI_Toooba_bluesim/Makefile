###  -*-Makefile-*-

REPO ?= ../..
ARCH ?= RV64ACDFIMSUxCHERI

# ================================================================
# Path to RISCY-OOO sources not included in Common

EXTRA_DIRS = $(RISCY_HOME)/../../src_Verifier:$(RISCY_HOME)/../../src_Verifier/BSV-RVFI-DII

# ================================================================
# RISC-V config macros passed into Bluespec 'bsc' compiler

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
