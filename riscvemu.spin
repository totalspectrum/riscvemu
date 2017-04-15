VAR
  long cog
  
PUB start(params)
  cog := cognew(@enter, params) + 1
  return cog

PUB stop
  if (cog > 0)
    cogstop(cog-1)

DAT
		org 0
enter
		mov	temp, par
		rdlong	cmd_addr, temp
		add	temp, #4
		rdlong	membase, temp
		add	temp, #4
		rdlong	pc, temp
		add	temp, #4
		add	pc, membase
		rdlong	dbgreg_addr, temp

nexti
		rdlong	opcode, pc
		call	#singlestep
		add	pc, #4
		'' check for valid opcodes
		'' the two lower bits must be 11
		'' that means nonzero, even parity on those two bits
		test	opcode, #3 wz,wc
   if_c_or_z	jmp	#illegal_instr
		mov	x0+1, opcode
		jmp	#nexti

illegal_instr
		mov	newcmd, #2	' signal illegal instruction
		call	#sendcmd
		jmp	#nexti
		
singlestep
		call	#dumpregs
		mov	newcmd, #1	' single step command
		call	#sendcmd	' send info
		call	#waitcmdclear	' wait for response
singlestep_ret	ret
		
dumpregs
		mov	cogaddr, #x0
		mov	hubaddr, dbgreg_addr
		mov	hubcnt, #34*4
		
		call	#cogxfr_write
dumpregs_ret
		ret

newcmd		long 0
sendcmd
		call	#waitcmdclear
		wrlong	newcmd, cmd_addr
sendcmd_ret	ret
		
waitcmdclear
		rdlong	temp, cmd_addr
		cmp	temp, #0 wz
	if_nz	jmp	#waitcmdclear	' far end is still processing last command
waitcmdclear_ret
		ret
		
	
'------------------------------------------------------------------------------
' routines for fast transfer of COG memory to/from hub
' "hubaddr"   is the HUB memory address
' "cogaddr"   is the COG memory address
' "hubcnt"    is the number of *bytes* to transfer
' '
' The idea is based on code posted by Kuroneko in the
' "Fastest possible memory transfer" thread on the
' Parallax forums, modified slightly for arbitrary buffers.
' Note that the number of longs must be a multiple of 2
'------------------------------------------------------------------------------

' NOTE: the instructions at lbuf0 and lbuf1 can be destroyed if
' we count down below 0 (where the cache starts) so we have to
' refresh them each time
' we have to set up for read/write anyway, so this isn't too big
' a deal

wrins		wrlong	0-0, hubaddr

cogxfr_write
		mov	lbuf0, wrins
		jmp	#doxfer

rdins	   	rdlong	0-0, hubaddr
cogxfr_read
  		mov	lbuf0, rdins
doxfer
		mov	lbuf1, lbuf0
		add	hubcnt, #7
		andn	hubcnt, #7	' round up
		' point to last byte in HUB buffer
		add	hubaddr, hubcnt
		sub	hubaddr, #1
		' point to last longs in cog memory
		shr	hubcnt, #2      ' convert to longs
		add	cogaddr, hubcnt
		sub	cogaddr, #1
		movd	lbuf0, cogaddr
		sub	cogaddr, #1
		movd	lbuf1, cogaddr
		sub	hubcnt, #2
		movi	hubaddr, hubcnt	' set high bits of hub address

lbuf0		rdlong	0-0, hubaddr
		sub	lbuf0, dst2
		sub	hubaddr, i2s7 wc
lbuf1		rdlong  0-0, hubaddr
		sub	lbuf1, dst2
if_nc		djnz	hubaddr, #lbuf0
cogxfr_read_ret
cogxfr_write_ret
		ret
		'' initialized data and presets
dst2		long	2 << 9
i2s7		long	(2<<23) | 7

temp		long 0
dbgreg_addr	long 0	' address where registers go in HUB during debug
cmd_addr	long 0	' address of HUB command word
membase		long 0	' base of emulated RAM
hubaddr		long 0
cogaddr		long 0
hubcnt		long 0

		'' registers
		'' pc must follow x0-x31
		'' next one after is also displayed in debug
x0		long	0[32]
pc		long	0
opcode		long	0

