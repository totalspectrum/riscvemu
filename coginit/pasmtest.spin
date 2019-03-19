''
'' simple ASM demo
''
''
'' this simple program runs in another COG and adds two numbers together
''
'' it is passed a mailbox pointing to three long values:
''   mbox[0] = command; this is 1 to indicate we should do something
''             we set command to 0 when we are finished
''   mbox[1] = aval; we also write back the final result (aval+bval) here
''   mbox[2] = bval
''

DAT
	org	0
entry
#ifdef __P2__
	mov	tempptr, ptra	' on P2 parameters go in ptra
#else
	mov	tempptr, par	' on P1 they go in the par register
#endif	
	mov	command_ptr, tempptr
	add	tempptr, #4
	mov	aval_ptr, tempptr
	add	tempptr, #4
	mov	bval_ptr, tempptr

	'' wait for a command
wait_for_command
	rdlong	cmd, command_ptr
	cmp	cmd, #0 wz
  if_z	jmp	#wait_for_command

  	'' ignore any commands other than 1
	cmp	cmd, #1 wz
  if_nz	jmp	#command_done

  	'' fetch a and b, and add them together
  	rdlong	aval, aval_ptr
	rdlong	bval, bval_ptr
	add	aval, bval
	wrlong	aval, aval_ptr

command_done
	wrlong	zero, command_ptr
	jmp	#wait_for_command

zero	long	0

tempptr         res 1
command_ptr	res 1
aval_ptr	res 1
bval_ptr	res 1
cmd		res 1
aval		res 1
bval		res 1

	fit $1f0

''
'' and the spin interface code
''

VAR
	long mbox[3]	' mailbox for communicating with COG
	long cogx	' 1 + id of COG running our code (0 if stopped)

PUB start
    cogx := cognew(@entry, @mbox) + 1
    return cogx

PUB stop
    if cogx => 1
      cogstop(cogx-1)
      cogx := 0

PUB sum(a, b)
  ' set up parameters
  ' we must do this before triggering the command
  mbox[1] := a
  mbox[2] := b

  ' now issue the command
  mbox[0] := 1

  ' wait for COG to finish
  repeat while mbox[0] <> 0

  ' and return result
  return mbox[1]

