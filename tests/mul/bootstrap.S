.globl _start
.globl main

_start:
jal main

# invalid instruction to cause exit
.word 0xffffffff
