#define MEMORY_START  0x10000

.globl _start
mydata:
	.word 0xdeadbeef

_start:
	lw a0, mydata
	li t0, MEMORY_START
	sw a0, 128(t0)

	lb a0, 128(t0)
	lb a1, 129(t0)
	lb a2, 130(t0)
	lb a3, 131(t0)

	lh a4, 128(t0)
	lh a5, 130(t0)

	# Invalid instruction to cause exit
	.word 0xffffffff
