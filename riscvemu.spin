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
{00}		jmp	#init		' load
{01}		jmp	#illegalinstr	' float load
{02}		jmp	#illegalinstr	' custom0
{03}		jmp	#illegalinstr	' fence
{04}		jmp	#immediateop	' math immediate
{05}		jmp	#auipc		' auipc
{06}		jmp	#illegalinstr	' wide math imm
{07}		jmp	#illegalinstr	' ???
{08}		jmp	#illegalinstr	' store
{09}		jmp	#illegalinstr	' float store
{0A}		jmp	#illegalinstr	' custom1
{0B}		jmp	#illegalinstr	' atomics
{0C}		jmp	#regop		' math reg
{0D}		jmp	#lui		' lui
{0E}		jmp	#illegalinstr	' wide math reg
{0F}		jmp	#illegalinstr	' ???

{10}		jmp	#illegalinstr
{11}		jmp	#illegalinstr
{12}		jmp	#illegalinstr
{13}		jmp	#illegalinstr
{14}		jmp	#illegalinstr
{15}		jmp	#illegalinstr
{16}		jmp	#illegalinstr	' custom2
{17}		jmp	#illegalinstr
{18}		jmp	#illegalinstr	' conditional branch
{19}		jmp	#jalr
{1A}		jmp	#illegalinstr
{1B}		jmp	#jal
{1C}		jmp	#illegalinstr	' system
{1D}		jmp	#illegalinstr
{1E}		jmp	#illegalinstr	' custom3
{1F}		jmp	#illegalinstr

opcode0entry
		jmp	#illegalinstr	'' load

'' these generally contain the PROPELLER opcode needed to
'' implement the corresponding RISC-V instruction
'' there are some special circumstances:
''   the cmp based instructions (low bit 0); these need further
''   expansion to implement slt / sltu
'' 
mathtab
{0}		jmp	#imp_add
{1}		jmp	#imp_sll	'' shl
{2}		jmp	#imp_slt	'' set if less than, signed
{3}		jmp	#imp_sltu	'' set if less than, unsigned
{4}		jmp	#imp_xor	'' xori
{5}		jmp	#imp_shr	'' srli or srai, based on imm 
{6}		jmp	#imp_or		'' ori
{7}		jmp	#imp_and	'' andi

init
		mov	opcodetab, opcode0entry
		mov	temp, par
		rdlong	cmd_addr, temp
		add	temp, #4
		rdlong	membase, temp
		add	temp, #4
		rdlong	memsize, temp
		add	temp, #4
		rdlong	pc, temp
		add	temp, #4
		add	pc, membase
		rdlong	dbgreg_addr, temp

		''
		'' main instruction decode loop
		''
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
   if_z		mov	rd, #temp	' write to x0 get ignored
		mov	temp, opcode
		shr	temp, #2
		and	temp, #$1f
		add	temp, #opcodetab
		jmp	temp		'' jump to instruction decode

		'' come here for illegal instructions
illegalinstr
		mov	newcmd, #2	' signal illegal instruction
		call	#sendcmd
		jmp	#nexti


		'' math register operations
regop
		mov	rs2, opcode
		sar	rs2, #20
		and	rs2, #$1f
		add	rs2, #x0
		movs	:fetchrs, rs2
		movs	mathtab, #imp_addsub
:fetchrs	mov	rs2, 0-0
		jmp	#domath
		
		'' math immediate operations
immediateop
		movs	mathtab, #imp_add
		mov	rs2, opcode
		sar	rs2, #20

domath
		mov	rs1, opcode
		shr	rs1, #15
		and	rs1, #$1f
		add	rs1, #x0
		mov	funct3, opcode
		shr	funct3, #12
		and	funct3, #7
		add	funct3, #mathtab	' funct3 pts at instruction
		movs	:exec1, rs1
		movd	:writeback, rd
:exec1		mov	dest, 0-0
		'' actually execute the decoded instruction here

		jmpret	mathret, funct3
:writeback	mov	0-0, dest
		jmp	#nexti
mathret		long	0

		'' execute math instructions
		'' for all of these, rs1 is in temp, rs2 has the needed value
		'' result should go in temp
imp_add
		add	dest, rs2
		jmp	mathret
imp_addsub
		test	opcode, sra_mask wz
	if_z	add	dest, rs2
	if_nz	sub	dest, rs2
		jmp	mathret

imp_sll		shl	dest, rs2
		jmp	mathret
imp_slt		cmps	dest, rs2 wz,wc
		mov	dest, #0
  if_b		mov	dest, #1
  		jmp	mathret
imp_sltu	cmp	dest, rs2 wz,wc
		jmp	#imp_slt+1
imp_xor
		xor	dest, rs2	' FIXME
		jmp	mathret
imp_shr
		'' depending on opcode we do sar or shr
		test	opcode, sra_mask wz
	if_z	shr	dest, rs2
	if_nz	sar	dest, rs2
		jmp	mathret
imp_or		or	dest, rs2
		jmp	mathret
imp_and		and	dest, rs2
		jmp	mathret
		'' for sra 31..26 = 16
		'' so 31..24 = 64 = $40
sra_mask	long	$40000000

'' load upper immediate (20 bits)
lui
		'' set up to write to rd
		movd   :luiwrite, rd
    		'' extract upper immediate
		and	opcode, luimask
:luiwrite	mov	0-0, opcode
		jmp	#nexti
luimask		long	$fffff000

'' load upper 20 bits, added with pc
auipc
		'' set up to write to rd
		movd   :auipcwr, rd
    		'' extract upper immediate
		and	opcode, luimask
		add	opcode, pc
		sub	opcode, #4
:auipcwr	mov	0-0, opcode
		jmp	#nexti

''''''''''''''''''''''''''''''''''''''''''''''''''''
'' jal: jump and link
''''''''''''''''''''''''''''''''''''''''''''''''''''
Jmask		long	$fff00fff
Jbit11		long	(1<<11)
jal
		movd	:jalwr, rd	' set up to write result
		mov	temp, opcode	' extract J-immediate
		sar	temp, #20	' sign extend, get some bits in place
		and	temp, Jmask
		andn	opcode, Jmask
		or	temp, opcode	' set bits 19:12
		test	temp, #1 wc	' check old bit 20
		andn	temp, #1 	' clear low bit
		muxc	temp, JBit11	' set bit 11
		sub	pc, membase	' and for offset
:jalwr		mov	0-0, pc		' save old pc
		sub	pc, #4		' compensate for pc bump
		add	pc, membase
		add	pc, temp
		jmp	#nexti

jalr
		movd	:jalrwr, rd	' set up to write result
		mov	rs1, opcode
		sar	opcode, #20	' get offset
		shr	rs1, #15
		and	rs1, #$1f
		add	rs1, #x0
		movs	:jalfetch, rs1
		sub	pc, membase
:jalrwr		mov	0-0, pc		' save old pc
:jalfetch	mov	pc, 0-0		' fetch rs1 value
		add	pc, membase
		add	pc, opcode
		jmp	#nexti
''''''''''''''''''''''''''''''''''''''''''''''''''''''''

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
		sub	pc, membase	' adjust for VM
		call	#cogxfr_write
		add	pc, membase	' adjust for VM
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
dest		long 0
dbgreg_addr	long 0	' address where registers go in HUB during debug
cmd_addr	long 0	' address of HUB command word
membase		long 0	' base of emulated RAM
memsize		long 0	' size of emulated RAM
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

		fit	$120	'$1F0 is whole thing
