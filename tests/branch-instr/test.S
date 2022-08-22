#define SUCC_PATT 0x2342
#define FAIL_PATT 0xffff

.globl _start
end:
	.word 0xffffffff
_start:
	li a0, 1
	li a1, 2
	// This branch should be taken.
	blt a0, a1, succ
fail:
	li a0, FAIL_PATT
	j end
succ:
	li a0, 2
	li a1, 2
	// This branch should not be taken.
	blt a0, a1, fail
	li a0, SUCC_PATT
	j end
