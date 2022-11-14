.globl _start
_start:
	li t0, -1
	slti a0, t0, 0
	sltiu a1, t0, 0
	seqz a2, zero

	li t0, 0xffffffff
	slti a3, t0, 0
	sltiu a4, t0, 0

	li t0, -128
	slti a5, t0, -10
	sltiu a6, t0, -10

	# Invalid instruction to cause exit
	.word 0xffffffff
