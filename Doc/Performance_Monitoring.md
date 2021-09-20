# Performance Monitoring Module

**PLEASE NOTE: THIS DOCUMENT IS DEPRECATED AND ONLY EXISTS FOR A PERIOD OF TRANSITION**. For the current mapping, see [here](https://github.com/CTSRD-CHERI/RISCV_HPM_Events/blob/master/counters.yaml).

This overview is based on this [document](https://github.com/CTSRD-CHERI/Flute/blob/CHERI/Doc/Performance_Monitor/Performance_Monitoring.md).

## Usage
To use the module, enable `PERFORMANCE_MONITORING` in your build (ensure that `BSC_COMPILATION_FLAGS` includes `-D PERFORMANCE_MONITORING` when running make).
Code running on a core with `PERFORMANCE_MONITORING` enabled can now access any of the relevant counter CSRs as specified by the [RISC-V Privileged Specification](https://riscv.org/technical/specifications/) (section __3.1.11 Hardware Performance Monitor__).
The implemented CSRs are:
- `mcycle` and `minstret` (also work without `PERFORMANCE_MONITORING` enabled)
- `mhpmcounter3–mhpmcounter31` event counters (29 total)
- `mcycleh`, `minstreth` & `mhpmcounternh` versions of the above to access the high bits on RV32
- `cycle`, `instret` & `hpmcountern` as read-only shadows
- `mhpmevent3-mhpmevent31` event selectors
- `mcounteren` to enable reads to masked counters in S- and U-mode (Seems that check is implemented in CSR_RegFile, but never used)
- `mcountinhibit` to control which counters increment
- `scounteren` to enable reads to masked counters in U-mode

## Events
Any event happening any number of times per cycle in the core can be counted, using the provided `mhpmcounter<N>` and `mhpmevent<N>` CSRs. Most common events are already provided, though it should be simple to extend and add additional events as needed.
The following events along with corresponding event id (this id should be written to the `mhpmevent<N>` selector CSR) are given:
- No event (0x0)

Core events:
- Redirect &ndash; count PC redirects (0x1)
- Traps &ndash; caused by a dmem exception or failed CHERI check (0x2)
- Branch &ndash; count branch instrs (0x3)
- Jal &ndash; count jal instrs (0x4)
- Jalr &ndash; count jalr instrs (0x5)
- Auipc &ndash; count auipc instrs (0x6)
- Load &ndash; count load instrs (0x7)
- Store &ndash; count store instrs (0x8)
- LR &ndash; count lr instrs (0x9)
- SC &ndash; count sc instrs (0xa)
- AMO &ndash; count (non lr or sc) atomic instrs (0xb)
- Serial shift &ndash; count serial shift (slli, srli, srai) instrs (0xc)
- Integer Mult/Div &ndash; count integer multiply and divide instrs (0xd)
- FP &ndash; count all floating point instrs (0xe)
- SC Success &ndash; count SC successes (0xf)
- Load wait &ndash; count cycles waiting on load (0x10)
- Store wait &ndash; count cycles waiting on store (0x11)
- Fence &ndash; count fence instrs (0x12)

The following events are defined in Toooba, but not implemented (mostly due
to the fact that they are Flute-only events)

-------------------------------

- F Busy No Consume &ndash; count cycles where stage F is busy (0x13)
- D Busy No Consume &ndash; count cycles where stage F is ready to pipe but D is busy (0x14)
- 1 Busy No Consume &ndash; count cycles where stage D is ready to pipe but 1 is busy (0x15)
- 2 Busy No Consume &ndash; count cycles where stage 1 is ready to pipe but 2 is busy (0x16)
- 3 Busy No Consume &ndash; count cycles where stage 2 is ready to pipe but 3 is busy (0x17)
- Imprecise setbounds &ndash; count when a setbounds instr does NOT result in the exact bounds requested (0x18)
- Unrepresentable cap &ndash; count when a capability is out of bounds (due to set offset instr) and is nullified (0x19)

---------------------------------------------------

- Mem cap load &ndash; count when capability wide data are loaded, regardless of tag (0x1a)
- Mem cap store &ndash; count when capability wide data are loaded, regardless of tag (0x1b)
- Mem cap load tag set &ndash; count when a tagged capability is loaded (0x1c)
- Mem cap store tag set &ndash; count when stage a tagged capability is stored (0x1d)

IMem, DMem L1 Cache, and LL Cache (including the respective TLBs); events identical for all three, though some are irrelevant for the respective caches. IDs in format (IMem/DMem/LLC):
- Load &ndash; count loads requested by cpu (0x20/0x30/unimplemented)
- Load miss &ndash; count loads missed (0x21/0x31/0x61)
- Load miss latency &ndash; count cycles waiting on a load miss (0x22/0x32/0x62)
- Store &ndash; count stores requested by cpu (unimplemented/0x33/unimplemented)
- Store miss &ndash; (unimplemented/unimplemented/unimplemented)
- Store miss latency &ndash; (unimplemented/unimplemented/unimplemented)
- Amo &ndash; count atomic ops requested by cpu (unimplemented/0x36/unimplemented)
- Amo miss &ndash; count atomics missed (unimplemented/0x37/unimplemented)
- Amo miss latency &ndash; count cycles waiting on a atomics miss (unimplemented/0x38/unimplemented)
- Tlb &ndash; count tlb accesses (0x29/0x39/0x69)
- Tlb miss &ndash; count tlb missed (0x2a/0x3a/0x6a)
- Tlb miss latency &ndash; count cycles waiting on a tlb miss (0x2b/0x3b/unimplemented)
- Tlb flush &ndash; count tlb flushes (0x2c/0x3c/0x6c)
- Evict &ndash; count cache line evictions (unimplemented/unimplemented/unimplemented)


TagController events:
- Write &ndash; count writes to tag cache (0x40)
- Write miss (0x41)
- Read (0x42)
- Read miss (0x43)
- Evict (0x44)
- Set tag write (0x45)
- Set tag read (0x46)

Transient-execution events
- Renamed Instructions &ndash; count renamed instructions excluding instructions that are already trapping (0x70)

Missing events are:
- L1 WT CHERI events
- L1 WB events
- L2 WB events
- AXI4 events for CHERI-enabled builds
- External events
