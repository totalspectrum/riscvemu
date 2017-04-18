	.text
start:
	li	x10, 17
	li	x11, 18
	divu	x8, x10, x11
	remu	x9, x10, x11
	
	li	x10, 7
	li	x11, 7
	div	x8, x10, x11
	rem	x9, x10, x11

	li	x10, 9
	li	x11, 8
	divu	x8, x10, x11
	remu	x9, x10, x11
	
	.long	0
