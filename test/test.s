	.text
start:
	la	x8, msg
	li	x10, 0xf0000000
repeat:
	lbu	x9, 0(x8)
	beq	x9, x0, here
	addi	x8, x8, 1
	sll	x3, x9, 4
	ori	x3, x3, 0xf
	sw	x3, 0(x10)
	j	repeat
	
here:	jal	x0, here
	
subr:
	andi	x10, x10, 0xFF
	sub	x10, x0, x10
	jalr	x0, x1
	
	.data
msg:	.ascii "hi\r\n"
	.long	0
