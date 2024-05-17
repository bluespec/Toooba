###  -*-Makefile-*-

# Copyright (c) 2018-2019 Bluespec, Inc. All Rights Reserved

# ================================================================
# Macros from RISCY_HOME/procs/RV64G_OOO/Makefile

PROC := RV64G_OOO

# core size
CORE_SIZE ?= SMALL
# cache size
CACHE_SIZE ?= LARGE
# always include perf counter
PERF_COUNT := true
# dram type in simulation: VC707 or AWSF1
SIM_DRAM_TYPE := AWSF1
# use Xilinx FPU IP cores
USE_XILINX_FPU ?= false

# default 1 core
CORE_NUM ?= 1
# TSO or WEAK
TSO_MM ?= true
# Lr upgrades line to E (no forward progress guarantee)
LR_UP_TO_E ?= false
# Forbid LLC from respoding a load (toS) request with E state
NO_LOAD_RESP_E ?= false
# Use self inv cache? (only for WEAK mm), value is the max hits in D$
SELF_INV_CACHE ?=
# self inv D$ in case of system inst or trap
SYSTEM_SELF_INV_L1D ?= false
# security
SECURITY ?= false
SECURE_LLC ?= false
SECURE_FLUSH ?= false
SECURE_NONE ?= false
SECURE_MSHR ?= false
SECURE_ARBITER ?= false
DISABLE_SECURE_BW ?= false
SIM_LOG_LLC_PARTITION_NUM ?=
SIM_LOG_LLC_MSHR_BANK_NUM ?=
SIM_LLC_ARBITER_NUM ?=
SIM_LLC_ARBITER_LAT ?=
# default check cache deadlock and rename error
CHECK_DEADLOCK ?= true
RENAME_DEBUG ?= false
INSTR_PREFETCHER_LOCATION ?= NONE
INSTR_PREFETCHER_TYPE ?= SINGLE_WINDOW_TARGET
DATA_PREFETCHER_LOCATION ?= L1
DATA_PREFETCHER_TYPE ?= STRIDE

# clk frequency depends on core size
ifneq (,$(filter $(CORE_SIZE),TINY SMALL BOOM MEDIUM))
USER_CLK_PERIOD ?= 24
else ifneq (,$(filter $(CORE_SIZE),SMALL_WIDE LARGE))
USER_CLK_PERIOD ?= 32
else ifneq (,$(filter $(CORE_SIZE),LARGE_WIDE))
USER_CLK_PERIOD ?= 40
else
$(error unsupported CORE_SIZE)
endif

ifeq (,$(filter $(CACHE_SIZE),TEST SMALL LARGE MC_1MB MC_2MB))
$(error unsupported CACHE_SIZE)
endif


ifeq (,$(filter $(INSTR_PREFETCHER_LOCATION),NONE L1 L1LL LL))
	$(error unsupported INSTR_PREFETCHER_LOCATION)
endif
ifeq (,$(filter $(INSTR_PREFETCHER_TYPE),NEXT_LINE_ON_MISS NEXT_LINE_ON_ALL SINGLE_WINDOW MULTI_WINDOW SINGLE_WINDOW_TARGET MULTI_WINDOW_TARGET))
	$(error unsupported INSTR_PREFETCHER_TYPE)
endif


ifeq (,$(filter $(DATA_PREFETCHER_LOCATION),NONE L1 L1LL LL))
	$(error unsupported DATA_PREFETCHER_LOCATION)
endif
ifeq (,$(filter $(DATA_PREFETCHER_TYPE),MARKOV MARKOV_ON_HIT MARKOV_ON_HIT_2 BLOCK STRIDE STRIDE_ADAPTIVE))
	$(error unsupported DATA_PREFETCHER_TYPE)
endif

# ================================================================
# These are taken from: RISCY_HOME/procs/scripts/Makefile.common

XILINX_FP_FMA_LATENCY = 3
XILINX_INT_MUL_LATENCY = 2


BSC_COMPILATION_FLAGS += \
	-D CORE_$(CORE_SIZE) \
	-D NUM_CORES=$(CORE_NUM) \
	-D CACHE_$(CACHE_SIZE) \
        -D XILINX_FP_FMA_LATENCY=$(XILINX_FP_FMA_LATENCY) \
        -D XILINX_INT_MUL_LATENCY=$(XILINX_INT_MUL_LATENCY) \
	-D USE_BSV_BRAM_SYNC_FIFO \
	-D INSTR_PREFETCHER_IN_$(INSTR_PREFETCHER_LOCATION) \
	-D INSTR_PREFETCHER_$(INSTR_PREFETCHER_TYPE) \
	-D DATA_PREFETCHER_IN_$(DATA_PREFETCHER_LOCATION) \
	-D DATA_PREFETCHER_$(DATA_PREFETCHER_TYPE) \
	-D CAP128 \
	-D MEM512 \
	-D RISCV \
	-D TSO_MM \
	-D RV64 \
	-D ISA_PRIV_M  -D ISA_PRIV_S  -D ISA_PRIV_U  \
	-D SV39 \
	-D ISA_I  -D ISA_M  -D ISA_A  -D ISA_F  -D ISA_D  -D ISA_FD_DIV  -D ISA_C  \
	-D NO_SPEC_TRAINING -D NO_SPEC_REDIRECT -D NO_SPEC_STRAIGHT_PATH -D SPEC_RSB_FIXUP -D MELTDOWN_CF \
	-D CheriBusBytes=64 \
	-D CheriMasterIDWidth=1 \
	-D CheriTransactionIDWidth=6

# TODO:
#    -D SELF_INV_CACHE -D L1D_MAX_HITS=$(SELF_INV_CACHE)
#    -D SYSTEM_SELF_INV_L1D
#    -D LR_UP_TO_E
#    -D NO_LOAD_RESP_E
# various SECURITY related flags
#    -D PERF_COUNT    -D CHECK_DEADLOCK    -D RENAME_DEBUG ...
#    -D NO_SPEC_RSB_PUSH -D NO_SPEC_STL -D RVFI

# +RTS -K1G -RTS " --bscflags=" -steps-max-intervals 200  -check-assert

# ================================================================

# ================================================================
# Search path for bsc for .bsv files
CORE_DIR ?= $(REPO)
COREW_DIRS = $(CORE_DIR)/src_Core/Core:$(CORE_DIR)/src_Core/CPU:$(CORE_DIR)/src_Core/ISA:$(CORE_DIR)/src_Core/PLIC:$(CORE_DIR)/src_Core/Debug_Module:$(CORE_DIR)/src_Core/BSV_Additional_Libs:$(CORE_DIR)/src_Core/RISCY_OOO/procs/RV64G_OOO:$(CORE_DIR)/src_Core/RISCY_OOO/procs/lib:$(CORE_DIR)/src_Core/RISCY_OOO/coherence/src:$(CORE_DIR)/src_Core/RISCY_OOO/fpgautils/lib
WINDCORE_IFC_DIR ?= $(CORE_DIR)/libs/WindCoreInterface
CHERICAPLIB_DIR ?= $(CORE_DIR)/libs/cheri-cap-lib
TAG_CONTROLLER_DIR ?= $(CORE_DIR)/libs/TagController
RISCV_HPM_EVENTS_DIR ?= $(CORE_DIR)/libs/RISCV_HPM_Events
TAG_CONTROLLER_DIRS = $(TAG_CONTROLLER_DIR)/TagController:$(TAG_CONTROLLER_DIR)/TagController/CacheCore
BLUESTUFFDIR ?= $(CORE_DIR)/libs/BlueStuff
include $(BLUESTUFFDIR)/bluestuff.inc.mk # sets the BLUESTUFF_DIRS variable

# search path for bsc imports
ifdef BSC_CONTRIB_DIR
BSC_CONTRIB_LIB_DIR = $(BSC_CONTRIB_DIR)/lib/Libraries
else
BSC_CONTRIB_LIB_DIR = %/Libraries
endif
BSC_CONTRIB_DIRS = $(BSC_CONTRIB_LIB_DIR)/Bus

BSVPATH = +:$(BSC_CONTRIB_DIRS):$(WINDCORE_IFC_DIR):$(RISCV_HPM_EVENTS_DIR):$(CHERICAPLIB_DIR):$(TAG_CONTROLLER_DIRS):$(COREW_DIRS):$(BLUESTUFF_DIRS)
BSC_PATH = -p $(BSVPATH)
