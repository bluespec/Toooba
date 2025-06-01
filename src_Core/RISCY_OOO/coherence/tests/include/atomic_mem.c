#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>

uint64_t c_createMem(uint32_t lg_sz_bytes) {
	uint64_t size = 0x01ULL << lg_sz_bytes;
    printf("[C AtomicMem] INFO: try to allocate %lldB\n", (long long)size);

	uint64_t *mem = (uint64_t*)malloc(size); // should be dword-aligned

	if(mem == NULL) {
		fprintf(stderr, "[C AtomicMem] ERROR: fail to malloc %lldB\n", (long long)size);
		return 0;
	}
	// set uninitialized words to 0xAAAAAAAA (default of BSV regfile)
	for(uint64_t i = 0; i < (size >> 3); i++) {
		mem[i] = 0xAAAAAAAAAAAAAAAAULL;
	}
    printf("[C AtomicMem] INFO: allocate %lldB mem\n", (long long)size);
	return (uint64_t)mem;
}

uint64_t c_readMem(uint64_t ptr, uint64_t addr) {
	uint64_t *mem = (uint64_t*)ptr;
	return mem[addr >> 3];
}

void c_writeMem(uint64_t ptr, uint64_t addr, uint64_t data) {
	uint64_t *mem = (uint64_t*)ptr;
	mem[addr >> 3] = data;
}
