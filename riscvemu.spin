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
opcodetab
{00}		jmp	#init
{01}		jmp	#illegalinstr
{02}		jmp	#illegalinstr
{03}		jmp	#illegalinstr
{04}		jmp	#immediateop
{05}		jmp	#illegalinstr
{06}		jmp	#illegalinstr
{07}		jmp	#illegalinstr
{08}		jmp	#illegalinstr
{09}		jmp	#illegalinstr
{0A}		jmp	#illegalinstr
{0B}		jmp	#illegalinstr
{0C}		jmp	#illegalinstr
{0D}		jmp	#illegalinstr
{0E}		jmp	#illegalinstr
{0F}		jmp	#illegalinstr

{10}		jmp	#illegalinstr
{11}		jmp	#illegalinstr
{12}		jmp	#illegalinstr
{13}		jmp	#illegalinstr
{14}		jmp	#illegalinstr
{15}		jmp	#illegalinstr
{16}		jmp	#illegalinstr
{17}		jmp	#illegalinstr
{18}		jmp	#illegalinstr
{19}		jmp	#illegalinstr
{1A}		jmp	#illegalinstr
{1B}		jmp	#illegalinstr
{1C}		jmp	#illegalinstr
{1D}		jmp	#illegalinstr
{1E}		jmp	#illegalinstr
{1F}		jmp	#illegalinstr

opcode0entry
		jmp	#illegalinstr	'' load
		
mathtab
{0}		long	0	'' add
{1}		long	1	'' slli
{2}		long	2	'' slti
{3}		long	3	'' sltiu
{4}		long	4	'' xori
{5}		long	5	'' srli or srai, based on imm 
{6}		long	6	'' ori
{7}		long	7	'' andi

init
		mov	opcodetab, opcode0entry
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
   if_c_or_z	jmp	#illegalinstr
   		mov	rd, opcode
		shr	rd, #7
		and	rd, #$1f wz
		add	rd, #x0
		mov	temp, opcode
		shr	temp, #2
		and	temp, #$1f
		mov	x0+8, temp
		add	temp, #opcodetab
		jmp	temp		'' jump to instruction decode

		'' come here for illegal instructions
illegalinstr
		mov	newcmd, #2	' signal illegal instruction
		call	#sendcmd
		jmp	#nexti
		
immediateop
  if_z		jmp	#nexti			' rd == x0 means nop
		mov	rs2, opcode
		sar	rs2, #20
		mov	rs1, opcode
		shr	rs1, #15
		and	rs1, #$1f
		add	rs1, #x0
		mov	funct3, opcode
		shr	funct3, #12
		and	funct3, #7
		add	funct3, #mathtab	' funct3 pts at opcode
		movs	:fetch, funct3
		movs	:exec1, rs1
		movd	:writeback, rd
:fetch		mov	funct3, 0-0
		'' actually execute the decoded instruction here
:exec1		mov	temp, 0-0
:exec2		add	temp, rs2	' write actual instruction here
:writeback	mov	0-0, temp
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

rd		long	0
rs1		long	0
rs2		long	0
funct3		long	0
