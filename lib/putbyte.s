	# putbyte
	# takes one argument (a0), a byte to write
	# returns nothing
	.globl putbyte
	.text
putbyte:
	slli	a0, a0, 4
	ori	a0, a0, 15
	csrrw	x0, 0xbc0, a0
	ret
