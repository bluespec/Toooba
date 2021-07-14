###  -*-Makefile-*-

# ================================================================

.PHONY: help
help:
	@echo '    make  compile      Recompile Core (CPU, caches) into Verilog_RTL and copies into xilinx_ip/hdl'
	@echo '                           NOTE: needs Bluespec bsc compiler'
	@echo ''
	@echo '    make  clean        Remove intermediate build-files'
	@echo '    make  full_clean   Restore this directory to pristine state'

.PHONY: compile
compile: compile_sim compile_synth

# ================================================================

REPO ?= ..
ARCH ?= RV64ACDFIMSU

# ================================================================
# RISC-V config macros passed into Bluespec 'bsc' compiler

BSC_COMPILATION_FLAGS += \
	-D RV64 \
	-D ISA_PRIV_M  -D ISA_PRIV_S  -D ISA_PRIV_U  \
	-D SV39 \
	-D ISA_I  -D ISA_M  -D ISA_A  -D ISA_F  -D ISA_D  -D ISA_FD_DIV  -D ISA_C  \
	-D SHIFT_BARREL    \
	-D MULT_SERIAL    \
	-D Near_Mem_Caches    \
	-D PERFORMANCE_MONITORING \
	-D FABRIC64    \
	-D INCLUDE_GDB_CONTROL \
	-D INCLUDE_TANDEM_VERIF \
	-D BRVF_TRACE \
	-D XILINX_BSCAN  -D JTAG_TAP

# Synth only BSC_COMPILATION_FLAGS
SYNTH_BSC_OPTIONS = -D XILINX_XCVU9P

# Sim only BSC_COMPILATION_FLAGS
SIM_BSC_OPTIONS = -D BSIM

# Only used if we don't have INCLUDE_GDB_CONTROL
#	-D EXTERNAL_DEBUG_MODULE

include $(REPO)/builds/Resources/Include_RISCY_Config.mk

# ================================================================
# Path to RISCY-OOO sources

RISCY_HOME ?= ../src_Core/RISCY_OOO

RISCY_DIRS = $(RISCY_HOME)/procs/RV64G_OOO:$(RISCY_HOME)/procs/lib:$(RISCY_HOME)/coherence/src:$(RISCY_HOME)/fpgautils/lib

CONNECTAL_DIRS = $(RISCY_HOME)/connectal/bsv:$(RISCY_HOME)/connectal/tests/spi:$(RISCY_HOME)/connectal/lib/bsv

BLUESTUFF_DIRS = $(REPO)/src_Core/BSV_Additional_Libs/BlueStuff:$(REPO)/src_Core/BSV_Additional_Libs/BlueStuff/BlueUtils:$(REPO)/src_Core/BSV_Additional_Libs/BlueStuff/BlueBasics

# ALL_RISCY_DIRS = $(RISCY_DIRS)
ALL_RISCY_DIRS = $(RISCY_DIRS):$(CONNECTAL_DIRS):$(BLUESTUFF_DIRS)

# ================================================================
# Search path for bsc for .bsv files

CORE_DIRS = $(REPO)/src_Core/CPU:$(REPO)/src_Core/ISA:$(REPO)/src_Core/Core:$(REPO)/src_Core/PLIC:$(REPO)/src_Core/Debug_Module:$(REPO)/src_Core/BSV_Additional_Libs

TESTBENCH_DIRS  = $(REPO)/src_Testbench/Fabrics/AXI4

BSC_PATH = -p $(ALL_RISCY_DIRS):$(CORE_DIRS):src_BSV:$(TESTBENCH_DIRS):+:%/Libraries/AMBA_TLM3/TLM3:%/Libraries/AMBA_TLM3/Axi:%/Libraries/AMBA_TLM3/Axi4:%/Libraries/Bus

# ----------------
# Top-level file and module

TOPFILE   = src_BSV/P3_Core.bsv
TOPMODULE = mkP3_Core

# ================================================================
# More bsc compilation flags
BSC_COMPILATION_FLAGS += \
	-keep-fires -aggressive-conditions \
	-no-warn-action-shadowing -no-show-timestamps \
	-suppress-warnings G0020    \
	+RTS -K128M -RTS  -show-range-conflict \
	-unspecified-to X -opt-undetermined-vals \
	-steps-max-intervals 10000000 \
	-steps-warn-interval 1000000

# ================================================================
# Generate Verilog RTL from BSV sources (needs Bluespec 'bsc' compiler)

BUILD_DIRS_SYNTH = -bdir build_dir_synth  -info-dir build_dir_synth
BUILD_DIRS_SIM = -bdir build_dir_sim  -info-dir build_dir_sim

build_dir_synth:
	mkdir -p $@

build_dir_sim:
	mkdir -p $@

Verilog_RTL:
	mkdir -p $@

Verilog_RTL_sim:
	mkdir -p $@

$(REPO)/src_Core/BSV_Additional_Libs/BlueStuff/.git:
	git submodule update --init --recursive

.PHONY: compile_synth
compile_synth:  build_dir_synth Verilog_RTL $(REPO)/src_Core/BSV_Additional_Libs/BlueStuff/.git
	@echo  "INFO: Generating RTL into Verilog_RTL for synthesis ..."
	bsc -u -elab -verilog  -vdir Verilog_RTL $(BUILD_DIRS_SYNTH)  $(BSC_COMPILATION_FLAGS)  $(SYNTH_BSC_OPTIONS) $(BSC_PATH)  $(TOPFILE)
	@echo  "INFO: Generated Synth RTL into Verilog_RTL"
	cp  Verilog_RTL/*  xilinx_ip/hdl/
	@echo  "INFO: Copied RTL from  Verilog_RTL/  to  xilinx_ip/hdl/"

.PHONY: compile_sim
compile_sim:  build_dir_sim Verilog_RTL_sim $(REPO)/src_Core/BSV_Additional_Libs/BlueStuff/.git
	@echo  "INFO: Generating RTL into Verilog_RTL_sim for simulation ..."
	bsc -u -elab -verilog  -vdir Verilog_RTL_sim $(BUILD_DIRS_SIM)  $(BSC_COMPILATION_FLAGS) $(SIM_BSC_OPTIONS) $(BSC_PATH)  $(TOPFILE)

# ================================================================

.PHONY: clean
clean:
	rm -r -f  *~  Makefile_*  build_dir_sim build_dir_synth

.PHONY: full_clean
full_clean: clean
	rm -r -f  *.log  Verilog_RTL Verilog_RTL_sim

# ================================================================
