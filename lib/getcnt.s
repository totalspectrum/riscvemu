	.globl getcnt
	.text
getcnt:	
	rdcycle a0
	ret

	.globl getcyclespersec
getcyclespersec:	
	li	a0, 80000000
	ret
