	.globl getcnt
	.text
getcnt:	
	rdcycle a0
	ret
