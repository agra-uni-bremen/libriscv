#define MEMORY_START  0x10000
#define MEMORY_OFFSET 128

.globl _start
mydata:
	.word 0xdeadbeef

_start:
	lw a0, mydata
	addi a0, a0, -5

	li t0, MEMORY_START
	sw a0, MEMORY_OFFSET(t0)

	lw a0, MEMORY_OFFSET(t0)

	# Invalid instruction to cause exit
	.word 0xffffffff
