	.text
start:
	# perform whatever setup we need

	# zero the bss
	la	t0, _bssstart
	la	t1, _bssend
	beq	t0,t1,.endlp
.lp:
	sw	x0,0(t0)
	addi	t0,t0,4
	bne	t0,t1,.lp
.endlp:
	
	# call main
	call	main

	# now hang
done:
	j	done

	
