	.globl getcnt
	.text
getcnt:	
	rdcycle a0
	ret

	.globl getcyclespersec
getcyclespersec:	
	li	a0, 60000000
	ret
