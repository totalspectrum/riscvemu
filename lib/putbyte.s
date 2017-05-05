	# putbyte
	# takes one argument (a0), a byte to write
	# returns nothing
	.globl putbyte
	.text
putbyte:
	csrrw	x0, 0xbc0, a0
	ret
