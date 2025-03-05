.globl main

bar:
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
main:
  jalr x1, x6, 0x12c
  nop
  nop
	nop
foo:
  addi x0, x1, 100
	
