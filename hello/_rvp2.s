	# 
	# simple asm file to include the P2 JIT compiler
	#
	.section .emulator, "ax"
	.globl _emustart
	.globl _emuend
_emustart:	
	.incbin "p2trace.bin"
_emuend:	
	
