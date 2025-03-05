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
  jal x10, bar
  nop
  nop
	nop
foo:
  addi x0, x1, 100
	
