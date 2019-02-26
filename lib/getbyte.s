	# getbyte
	# takes no arguments
	# returns input byte, or -1 if no character is available
	.globl getbyte
	.text
getbyte:
	csrrw	a0, 0xbc0, x0
	ret
