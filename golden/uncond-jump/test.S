#define TWO 2
#define FOUR 4

.globl _start

_start:
	addi a0, zero, TWO
	jal ra, square
	add a2, a1, zero  

	addi a0, zero, FOUR
	jal ra, square
	add a3, a1, zero

	jal zero, end

square:
	addi t0, zero, 0 
	addi t1, zero, 0
	loop: 
		add t1, t1, a0
		addi t0, t0, 1
	blt t0, a0, loop
	add a1, t1, zero
	jalr zero, 0(ra) 

end:
	add zero, zero, zero
	.word 0xffffffff
