.globl _start
_start:
	li t0, 23
	slli a1, t0, 1
	srli a2, t0, 1
	srli a3, t0, 31
	srli a4, t0, 0

	li t1, -5
	srai a5, t1, 6

	# Invalid instruction to cause exit
	.word 0xffffffff
