.globl _start
_start:
	li a0, 0xdeadbeef
	andi a1, a0, 0x1f
	ori  a2, a0, 0xff
	xori a3, a0, 1

	# Invalid instruction to cause exit
	.word 0xffffffff
