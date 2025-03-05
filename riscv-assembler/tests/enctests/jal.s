.globl main

main:
  jal x10, foo
  nop
  nop
	nop
foo:
  addi x0, x1, 100
	
