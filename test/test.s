	.text
start:
	li	x10, 17
	li	x11, 18
	bltu	x10, x11, ok1
test2:	
	bltu	x11, x10, ok2
test3:
	beq	x11, x10, ok3
test4:
	bne	x11, x10, ok4
done:
	j	done
ok1:
	j	test2
ok2:
	j	test3
ok3:
	j	test4
ok4:
	j	ok4
	
	.long	0
