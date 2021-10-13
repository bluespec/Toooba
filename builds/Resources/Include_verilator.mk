###  -*-Makefile-*-

# Copyright (c) 2018-2019 Bluespec, Inc. All Rights Reserved

# This file is not a standalone Makefile, but 'include'd by other Makefiles

# ================================================================
# Generate Verilog RTL from BSV sources (needs Bluespec 'bsc' compiler)

RTL_GEN_DIRS = -vdir Verilog_RTL  -bdir build_dir  -info-dir build_dir

build_dir:
	mkdir -p $@

Verilog_RTL:
	mkdir -p $@

ifeq (,$(filter clean full_clean,$(MAKECMDGOALS)))
include .depends.mk

.depends.mk: TagTableStructure.bsv StatCounters.bsv GenerateHPMVector.bsv | build_dir Verilog_RTL
	if ! bluetcl -exec makedepend -verilog -elab  $(RTL_GEN_DIRS)  $(BSC_COMPILATION_FLAGS) -p $(BSC_PATH) -o $@ $(TOPFILE); then rm -f $@ && false; fi
endif

%.bo:
	$(info building $@)
	bsc -verilog -elab  $(RTL_GEN_DIRS)  $(BSC_COMPILATION_FLAGS) -p $(BSC_PATH) $<

.PHONY: compile
compile: build_dir/Top_HW_Side.bo | build_dir Verilog_RTL
#Verilog_RTL/mkTop_HW_Side.v:  build_dir Verilog_RTL /tmp/src_dir $(VERILOG_SUB_MODULES)
#Verilog_RTL/mkTop_HW_Side.v: $(TOPFILE) build_dir/Top_HW_Side.bo build_dir Verilog_RTL
#	@echo  "INFO: Verilog RTL generation ..."
#	bsc -u -verilog  $(RTL_GEN_DIRS)  $(BSC_COMPILATION_FLAGS) -p $(BSC_PATH) $<
#	@echo  "INFO: Verilog RTL generation finished"

# ================================================================
# Compile and link Verilog RTL sources into an verilator executable

SIM_EXE_FILE = exe_HW_sim

# Verilator flags: notes
#    stats              Dump stats on the design, in file {prefix}__stats.txt
#    -O3                Verilator optimization level
#    -CFLAGS -O3        C++ optimization level
#    --x-assign fast    Optimize X value
#    --x-initial fast   Optimize uninitialized value
#    --noassert         Disable all assertions

VERILATOR_FLAGS = --stats --x-assign fast --x-initial fast --noassert
# VERILATOR_FLAGS = --stats -O3 -CFLAGS -O3 -LDFLAGS -static --x-assign fast --x-initial fast --noassert

# XXX: Allow lint_off DEPRECATED for older Verilator versions.
#      This was added around the same time as -msg was deprecated, so we need
#      to suppress the deprecation messages without breaking older versions.
#      See verilator_config.vlt. Remove once 4.026 can be relied upon.
VERILATOR_FLAGS += -Wfuture-DEPRECATED

# Verilator flags: use the following to include code to generate VCDs
# Select trace-depth according to your module hierarchy
# VERILATOR_FLAGS += --trace  --trace-depth 2  -CFLAGS -DVM_TRACE

VTOP                = V$(TOPMODULE)_edited
VERILATOR_RESOURCES = $(REPO)/builds/Resources/Verilator_resources

.PHONY: simulator
simulator: build_dir/Top_HW_Side.bo
	@echo "INFO: Verilating Verilog files (in newly created obj_dir)"
	sed  -f $(VERILATOR_RESOURCES)/sed_script.txt  Verilog_RTL/$(TOPMODULE).v > tmp1.v
	cat  $(VERILATOR_RESOURCES)/verilator_config.vlt \
	     $(VERILATOR_RESOURCES)/import_DPI_C_decls.v \
	     tmp1.v                                     > Verilog_RTL/$(TOPMODULE)_edited.v
	rm   -f  tmp1.v
	verilator \
		-IVerilog_RTL \
		-I$(RISCY_HOME)/fpgautils/xilinx/fpu \
		-I$(RISCY_HOME)/fpgautils/xilinx/reset_regs \
		-I$(RISCY_HOME)/procs/asic/bluespec_verilog \
		-I$(REPO)/src_bsc_lib_RTL \
		$(VERILATOR_FLAGS) \
		--cc  $(TOPMODULE)_edited.v \
		--exe  sim_main.cpp \
		$(REPO)/src_Testbench/Top/C_Imported_Functions.c \
		$(REPO)/src_Verifier/BSV-RVFI-DII/SocketPacketUtils/socket_packet_utils.c
	@echo "INFO: Linking verilated files"
	cp  -p  $(VERILATOR_RESOURCES)/sim_main.cpp  obj_dir/sim_main.cpp
	cd obj_dir; \
	   make -j -f V$(TOPMODULE)_edited.mk  $(VTOP); \
	   cp -p  $(VTOP)  ../$(SIM_EXE_FILE)
	@echo "INFO: Created verilator executable:    $(SIM_EXE_FILE)"

# ================================================================
