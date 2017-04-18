	.text
start:
	li	x10, 5
	li	x11, 4
	mul	x8, x10, x11
	li	x10, 13
	divu	x8, x10, x11
	remu	x9, x10, x11
	sub	x10, x0, x10
	div	x8, x10, x11
	rem	x9, x10, x11
	.long	0
