	# functions to set/clear pins in the OUTA/DIRA registers

	#   INA = 0x1f2
	#   OUTA = 0x1f4
	#   DIRA = 0x1f6
	# those are COG addresses, or with 0x7f0 to make csrs

	# uint32_t pin_dirout(uint32_t x):
	#   bits set in x become outputs
	#   returns old set of outputs
	.global pin_dirout
pin_dirout:
	csrrs	a0, 0x7f6, a0
	ret

	# void pin_dirin(uint32_t x):
	#   bits set in x become inputs
	#   returns old set of outputs
	.global pin_dirin
pin_dirin:
	csrrc	a0, 0x7f6, a0
	ret

	# void pin_outhi(uint32_t x)
	#   bits set in x become high outputs
	#   returns old set of outputs
	.global pin_outhi
pin_outhi:
	csrrs	a0, 0x7f4, a0
	ret

	# void pin_outlo(uint32_t x)
	#   bits set in x become low outputs
	#   returns old set of outputs
	.global pin_outlo
pin_outlo:
	csrrc	a0, 0x7f4, a0
	ret
	
	# uint32_t pin_get(void)
	.global pin_get
pin_get:	
	csrrs	a0, 0x7f2, x0
	ret

	# void waitcnt(x): wait for cycle x to happen
	#  returns x
	.global waitcnt
waitcnt:
	csrrw	a0, 0xbc1, a0
	ret
