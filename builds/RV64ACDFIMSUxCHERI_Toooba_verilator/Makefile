###  -*-Makefile-*-

REPO ?= ../..
ARCH ?= RV64ACDFIMSUxCHERI

# ================================================================
# RISC-V config macros passed into Bluespec 'bsc' compiler

# Default ISA test

TEST ?= rv64ui-p-add

#================================================================
# Parameter settings for MIT RISCY, setup paths etc. for Include_Common

include $(REPO)/builds/Resources/Include_RISCY_Config.mk

#================================================================
# Common boilerplate rules

include $(REPO)/builds/Resources/Include_Common.mk

#================================================================
# Makefile rules for building for specific simulator: verilator

include $(REPO)/builds/Resources/Include_verilator.mk
