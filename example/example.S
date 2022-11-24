#define MEMORY_START  0x10000
#define MEMORY_OFFSET 128

.globl _start
_start:
	# Do something with register A0 (which is tainted).
	addi a0, a0, -5

	# Write contents of register A0 to memory and load it again.
	# To see if taint tracking propogates across the memory.
	li t0, MEMORY_START
	sw a0, MEMORY_OFFSET(t0)
	lw a0, MEMORY_OFFSET(t0)

	# Do some arithmetic operations on the tainted value loaded from memory.
	mv a1, a0
	addi a2, a1, 23

	# Invalid instruction to cause exit
	.word 0xffffffff
