	.text
start:
	j	next
val1:	.long	-1
next:	
	addi	x7, x0, 4		# get address of val1 into x7
	lw	x8, 0(x7)
	lb	x9, 0(x7)
	lhu	x10, 2(x7)
	ori	x8, x0, 0x11
	xori	x8, x8, 0x10	# leaves 1 in x8
	ori	x10, x0, 0x123
	jal	x1, subr
	lui	x9, 0x87654
	addi	x9, x9, 0x321  	# leaves 0x87654321 in x9
	sub	x9, x9, x8	# leaves 0x87654320 in x9
	sub	x10, x0, x8	# leaves FFFFFFFF in x10
	slt	x11, x9, x0	# leaves 1 in x11
	sltiu	x10, x9, 0xa	# leaves 0 in x10
	srli	x11, x9, 0x10	# leaves 0x8765 in x11
	addi	x2, x0, 0x8
	sra	x10, x9, x2	# leaves 0xFF876543 in x10
	slli	x10, x10, 4	# leaves 0xF8765430 in x10
	jal	x1, subr
	lui	x8, 0x55555
here:	jal	x0, here
	
subr:
	andi	x10, x10, 0xFF
	sub	x10, x0, x10
	jalr	x0, x1
	
	
