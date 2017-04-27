{{
   RISC-V Emulator for Parallax Propeller
   Copyright 2017 Total Spectrum Software Inc.
   Terms of use: MIT License (see the file LICENSE.txt)

   An emulator for the RISC-V processor architecture, designed to run
   in a single Propeller COG.

   The interface is pretty simple: the params array passed to the start
   method should contain:
   
   params[0] = address of command register
   params[1] = base of emulated memory
   params[2] = size of emulated memory (stack will start at base + size and grow down)
   params[3] = initial pc (usually 0)
   params[4] = address for register dump (36 longs; x0-x31, pc, opcode, dbg1, dbg3)

   The command register is used for communication from the RISC-V back to the host.
   The lowest 4 bits contain a command, as follows:
   $1 = single step (registers have been dumped)
   $2 = illegal instruction encountered (registers have been dumped)
   $F = request to write character in bits 12-8 of command register

   The host should write 0 to the command register as an "ACK" to restart the RISC-V.
  ---------------------------------------------------------------------
  Emulated memory map:
  0000_0000 - 0000_xxxx: RAM (0 is membase)
  F000_0000 - FFFF_FFFF: I/O space:
     F000_0000 = command register; writes here go to the host command register
     F000_0004 - F000_07FC = COG memory space (useful for accessing COG registers like CNT and OUTA)
   
}}

VAR
  long cog			' 1 + the cog id of the cog running the emulator
  
PUB start(params)
  cog := cognew(@enter, params) + 1
  return cog

PUB stop
  if (cog > 0)
    cogstop(cog-1)

DAT
		org 0
enter
init
		mov	temp, ptra
		rdlong	cmd_addr, temp
		add	temp, #4
		rdlong	membase, temp
		add	temp, #4
		rdlong	memsize, temp
		add	temp, #4
		rdlong	shadowpc, temp
		add	temp, #4
		add	shadowpc, membase
		mov	x0+2,membase	' set up stack pointer
		add	x0+2,memsize
		rdlong	dbgreg_addr, temp

		'' set up opcode table in LUT at offset 0
		'' initialize to all 0
		mov    dest, #0
		mov    temp, #$80
.oinitlp
		wrlut	jmpillegalinstr, dest
		add	dest, #1
		djnz	temp, #.oinitlp

		'' now set up some specific opcodes
		wrlut	jmploadop, #3
		wrlut	jmpimmop,  #3+($04<<2)
		wrlut	jmpauipc,  #3+($05<<2)
		wrlut	jmpstoreop,#3+(8<<2)
		wrlut	jmpregop,  #3+($0C<<2)
		wrlut	jmplui,    #3+($0D<<2)
		wrlut	jmpcondbranch, #3+($18<<2)
		wrlut	jmpjalr,   #3+($19<<2)
		wrlut	jmpjal,    #3+($1b<<2)
		wrlut	jmpsys,    #3+($1c<<2)
		
		'' finally set up the shadow pc
		rdfast	x0, shadowpc	        ' should be rdfast #0, pc, but fastspin is buggy
		jmp	#nexti

jmpillegalinstr
		long	illegalinstr
		
jmploadop
{00}		long	loadop	' load
jmpimmop
{04}		long	immediateop	' math immediate
jmpauipc
{05}		long	auipc		' auipc
jmpstoreop
{08}		long	storeop	' store
jmpregop
{0C}		long	regop		' math reg
jmplui
{0D}		long	lui		' lui
jmpcondbranch
{18}		long	condbranch	' conditional branch
jmpjalr
{19}		long	jalr
jmpjal
{1B}		long	jal
jmpsys
{1C}		long	sysinstr	' system

''
'' table for immediate operations
''
mathimmtab
{0}		long	imp_add	'' add or sub, based on imm field
{1}		long	imp_sll	'' shl
{2}		long	imp_slt	'' set if less than, signed
{3}		long	imp_sltu	'' set if less than, unsigned
{4}		long	imp_xor	'' xori
{5}		long	imp_shr	'' srli or srai, based on imm 
{6}		long	imp_or		'' ori
{7}		long	imp_and	'' andi

''
'' table for "regular" math operations
'' note that if bits 31..25 of the opcode == 1, we should use
'' the "mul" table instead
'' 
mathregtab
{0}		long	imp_addsub	'' add or sub, based on imm field
{1}		long	imp_sll	'' shl
{2}		long	imp_slt	'' set if less than, signed
{3}		long	imp_sltu	'' set if less than, unsigned
{4}		long	imp_xor	'' xori
{5}		long	imp_shr	'' srli or srai, based on imm 
{6}		long	imp_or		'' ori
{7}		long	imp_and	'' andi

multab
{0}		long	imp_mul
{1}		long	illegalinstr	'' mulh, not implemented
{2}		long	illegalinstr	'' mulhsu, not implemented
{3}		long	imp_muluh
{4}		long	imp_div
{5}		long	imp_divu
{6}		long	imp_rem
{7}		long	imp_remu

		''
		'' main instruction decode loop
		''

		'' write back last result from "dest" here
write_and_nexti
		mov	0-0, dest
nexti
		rflong	opcode
'''		call	#checkdebug
   		mov	temp, opcode
		shr	temp, #7
		and	temp, #$1f wz
		add	temp, #x0
   if_z		mov	temp, #dest	' writes to x0 get ignored
   		setd	write_and_nexti, temp
		mov	temp, opcode
		and	temp, #$7f
		rdlut	info1, temp
		jmp	info1

		
		'' come here for illegal instructions
illegalinstr
		call	#dumpregs
		mov	newcmd, #2	' signal illegal instruction
		call	#waitcmdclear
		call	#sendcmd
		jmp	#nexti


		''
		'' utility: extract rs1 field from opcode
		'' NOTE: does not actually read value from register
		''
getrs1
		mov	rs1, opcode
		shr	rs1, #15
    _ret_	and	rs1, #$1f


getrs2
		mov	rs2, opcode
		sar	rs2, #20
    _ret_	and	rs2, #$1f


		'' extract funct3 field
getfunct3
		mov	funct3, opcode
		shr	funct3, #12
    _ret_	and	funct3, #7

mulbit		long	(1<<25)

		'' math immediate operations
immediateop
		mov	rs2, opcode
		sar	rs2, #20
		call	#getrs1
		call	#getfunct3
		alts	rs1, #x0
		mov	dest, 0-0		' load rs1 into dest
		alts	funct3, #mathimmtab	' funct3 pts at instruction
		mov	funct3, 0-0
		
		'' actually execute the decoded instruction here
		jmp	funct3

		
		'' math register operations
regop
		call	#getrs2
		alts	rs2, #x0
		mov	rs2, 0-0
		test	opcode, mulbit wz
		mov	desth, #mathregtab
	if_nz	mov	desth, #multab

		'' fall through
		
		'' generic math routine
		'' enter with rs2 having the decoded value of rs2
		'' (register fetched if applicable)
		'' and with desth containing the table to use
		'' (generic mathtab, or multab)
domath
		call	#getrs1
		call	#getfunct3
		alts	rs1, #x0
		mov	dest, 0-0		' load rs1 into dest
		alts	funct3, desth		' funct3 pts at instruction
		mov	funct3, 0-0
		'' actually execute the decoded instruction here
		jmp	funct3

		'' execute math instructions
		'' for all of these, rs1 is in temp, rs2 has the needed value
		'' result should go in temp
imp_add
		add	dest, rs2
		jmp	#write_and_nexti
imp_addsub
		test	opcode, sra_mask wc
		sumc	dest, rs2	 ' if C=1 dest = dest - rs2, otherwise dest = dest + rs2
		jmp	#write_and_nexti

imp_sll		shl	dest, rs2
		jmp	#write_and_nexti
		
imp_slt		cmps	dest, rs2 wz,wc
		mov	dest, #0
		muxc	dest, #1
  		jmp	#write_and_nexti
imp_sltu	cmp	dest, rs2 wz,wc
		mov	dest, #0
		muxc	dest, #1
		jmp	#write_and_nexti
imp_xor
		xor	dest, rs2
		jmp	#write_and_nexti
imp_shr
		'' depending on opcode we do sar or shr
		test	opcode, sra_mask wz
	if_z	shr	dest, rs2
	if_nz	sar	dest, rs2
		jmp	#write_and_nexti
		
imp_or		or	dest, rs2
		jmp	#write_and_nexti
		
imp_and		and	dest, rs2
		jmp	#write_and_nexti
		
		'' for sra 31..26 = 16
		'' so 31..24 = 64 = $40
sra_mask	long	$40000000

'' load upper immediate (20 bits)
lui
    		'' extract upper immediate
		mov	dest, opcode
		and	dest, luimask
		jmp	#write_and_nexti
luimask		long	$fffff000


'' load upper 20 bits, added with pc
auipc
		mov	dest, opcode
		and	dest, luimask
		getptr	shadowpc
		add	dest, shadowpc
		sub	dest, membase
		sub	dest, #4
		jmp	#write_and_nexti

''''''''''''''''''''''''''''''''''''''''''''''''''''
'' jal: jump and link
''''''''''''''''''''''''''''''''''''''''''''''''''''
Jmask		long	$fff00fff
bit11		long	(1<<11)
jal
		mov	temp, opcode	' extract J-immediate
		sar	temp, #20	' sign extend, get some bits in place
		and	temp, Jmask
		andn	opcode, Jmask
		or	temp, opcode	' set bits 19:12
		test	temp, #1 wc	' check old bit 20
		andn	temp, #1 	' clear low bit
		muxc	temp, bit11	' set bit 11
		getptr	dest  		' get old PC
		mov	shadowpc, dest
		sub	dest, membase	' adjust for offset
		sub	shadowpc, #4		' compensate for pc bump
		add	shadowpc, temp
		rdfast	x0, shadowpc		' would use #0 instead of x0 except for fastspin bug
		jmp	#write_and_nexti

jalr
		call	#getrs1
		sar	opcode, #20	' get offset
		getptr	dest		' save old PC
		sub	dest, membase
		alts	rs1, #x0
		mov	shadowpc, 0-0	' fetch rs1 value
		add	shadowpc, membase
		add	shadowpc, opcode
		rdfast	x0, shadowpc		' would use #0 instead of x0 except for fastspin bug
		jmp	#write_and_nexti
''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' implement load and store
''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'
' table of load instructions
' the I field gives the PASM instruction we have to use to implement
' the load; the S field is 0 for unsigned load, 1 for signed

loadtab
		long	do_rdbyte
		long	do_rdword
		long	do_rdlong
		long	illegalinstr
		long	do_rdbytes
		long	do_rdwords
		long	do_rdlong
		long	illegalinstr
loadop
		call	#getrs1
		call	#getfunct3
		alts	rs1, #x0
    		mov	dest, 0-0	' set dest to value of rs1
		sar	opcode, #20	' extract immediate
		add	dest, opcode	' add offset
		alts	funct3, #loadtab
		mov	funct3, 0-0
		jmp	funct3

bytesignextend	long	$FFFFFF00
wordsignextend	long	$FFFF0000

do_rdbyte
		add	dest, membase
		rdbyte	dest, dest
		jmp	#write_and_nexti
		
do_rdbytes
		add	dest, membase
		rdbyte	dest, dest wc
		muxc	dest, bytesignextend
		jmp	#write_and_nexti
do_rdword
		add	dest, membase
		rdword	dest, dest
		jmp	#write_and_nexti
do_rdwords
		add	dest, membase
		rdword	dest, dest wc
		muxc	dest, wordsignextend
		jmp	#write_and_nexti
do_rdlong
		mov	info1, #$aa
		test	dest, iobase wz
	if_nz	jmp	#read_io
		add	dest, membase
		rdlong	dest, dest
		jmp	#write_and_nexti

read_io
		'' read from COG memory
		shr	dest, #2	' convert from bytes to longs
		and	dest, #$1ff	' mask off COG memory
		alts	dest, #0
        	mov	dest, 0-0
		mov	info2, dest
		jmp	#write_and_nexti
		
		''
		'' re-order bits of opcode so that it is
		'' an s-type immediate value
get_s_imm
		mov	temp, opcode
		shr	temp, #7
		and	temp, #$1f
		sar	opcode, #20
		andn	opcode, #$1f
    _ret_	or	opcode, temp	' opcode has offset

storetab
		long	do_wrbyte
		long	do_wrword
		long	do_wrlong
		long	illegalinstr
		
storeop
		call	#getrs2
		call	#getrs1
		call	#getfunct3
		alts	rs2, #x0
		mov	dest, 0-0	' set dest to value of rs2 (value to store)
		alts	rs1, #x0
     		mov	rs1, 0-0	' set rs1 to address of memory
		test	funct3, #4 wz	' check for signed/unsigned; Z is set for signed
	if_nz	jmp	#illegalinstr
		and	funct3, #3

		'' extract s-type immediate
		call	#get_s_imm
		add	rs1, opcode	' find address
		alts	funct3, #storetab
		mov	funct3, 0-0
		jmp	funct3		' go do store

iobase		long	$f0000000
do_wrlong
		test	rs1, iobase wz
    if_nz	jmp	#write_io
		add	rs1, membase
		wrlong	dest, rs1
		jmp	#nexti		' no writeback
		'' handle special IO stuff
write_io
		andn	rs1, iobase
		shr	rs1, #2 wz
    if_nz	jmp	#doiocog
    		mov	newcmd, dest
		call	#sendcmd
		call	#waitcmdclear
		jmp	#nexti
doiocog
		altd	rs1, #0
		mov	0-0, dest
		jmp	#nexti

do_wrword
		add	rs1, membase
		wrword	dest, rs1
		jmp	#nexti		' no writeback
do_wrbyte
		add	rs1, membase
		wrbyte	dest, rs1
		jmp	#nexti		' no writeback

''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' implement csrrw instruction
''''''''''''''''''''''''''''''''''''''''''''''''''''''''
sysinstrtab
		long	illegalinstr
		long	csrrw
		long	csrrs
		long	csrrc
		long	illegalinstr
		long	illegalinstr
		long	illegalinstr
		long	illegalinstr
sysinstr
		call	#getrs1
		call	#getfunct3
		shr	opcode, #20	' extract CSR address
		alts	funct3, #sysinstrtab
		mov	funct3, 0-0
		jmp	funct3

csrrw
csrrs
csrrc
		mov	info1, #$C5
		mov	info2, opcode
		mov	dest, #0
		cmp	opcode, ##$C00 wz, wc
	if_nz	jmp	#illegalinstr
		getct	dest
		jmp	#write_and_nexti

''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' implement conditional branches
''''''''''''''''''''''''''''''''''''''''''''''''''''''''
condbranch_tab
		long	do_beq
		long	do_bne
		long	illegalinstr
		long	illegalinstr
		long	do_blt
		long	do_bge
		long	do_bltu
		long	do_bgeu
condbranch
		call	#getrs1
		call	#getrs2
		call	#getfunct3
		alts	rs1, #x0
		mov	rs1, 0-0
		alts	rs2, #x0
        	mov	rs2, 0-0
		call	#get_s_imm	' opcode now contains s-type immediate
		test	opcode, #1 wc	' get low bit into carry
		muxc	opcode, bit11	' copy up to bit 11
		andn	opcode, #1	' clear low bit
		alts	funct3, #condbranch_tab
		mov	funct3, 0-0
		jmp	funct3

do_beq
		cmp	rs1, rs2 wz
	if_nz	jmp	#nexti
		'' fall through
takebranch
		getptr	shadowpc
		add	opcode, shadowpc
		sub	opcode, #4	' opcode now has desired destination
		rdfast	x0, opcode	' reset pc
		jmp	#nexti

do_bne
		cmp	rs1, rs2 wz
	if_z	jmp	#nexti
		jmp	#takebranch
		
do_bltu
		cmp	rs1, rs2 wc,wz
	if_nc	jmp	#nexti
		jmp	#takebranch
do_blt
		cmps	rs1, rs2 wc,wz
	if_nc	jmp	#nexti
		jmp	#takebranch
		
do_bgeu
		cmp	rs1, rs2 wc,wz
	if_c	jmp	#nexti
		jmp	#takebranch
do_bge
		cmps	rs1, rs2 wc,wz
	if_c	jmp	#nexti
		jmp	#takebranch

''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' unsigned multiply rs1 * rs2 -> (dest, desth)
''''''''''''''''''''''''''''''''''''''''''''''''''''''''
imp_mul
		call	#umul
		jmp	#write_and_nexti
imp_muluh	call	#umul
		mov	dest, desth
		jmp	#write_and_nexti

imp_divu	call	#udiv
		jmp	#write_and_nexti
imp_remu	call	#udiv
		mov	dest, desth
		jmp	#write_and_nexti

imp_rem
		mov	divflags, #4
		jmp	#dodiv
imp_div
		mov	divflags, #0
dodiv
		abs	dest, dest wc
		muxc	divflags, #1
		abs	rs2, rs2 wc
		muxc	divflags, #2
		call	#udiv
		test	divflags, #4 wc	' do we want remainder?
	if_c	jmp	#dorem
		test	divflags, #3 wc	' if both have same sign parity will be even, so c 0
	if_c	neg	dest, dest
		jmp	#write_and_nexti
dorem
		mov	desth, dest
		test	divflags, #1 wc	' remainder has sign of rs1
	if_c	neg	dest, dest
		jmp	#write_and_nexti

umul
		qmul	dest, rs2
		getqx	dest
    _ret_	getqy	desth

		'' calculate dest / rs2; result in dest, remainder in desth
udiv
		tjz	rs2, #div_by_zero
		setq	#0		' set high 64 bits
		qdiv	dest, rs2	' dest/rs2
		getqx	dest  		' quotient
    _ret_	getqy	desth		' remainder

div_by_zero
		neg	dest, #1
		mov	desth, rs1
		ret

''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' debug routines
''''''''''''''''''''''''''''''''''''''''''''''''''''''''

checkdebug
		tjz	stepcount, #checkdebug_ret
		djnz	stepcount, #checkdebug_ret
		call	#dumpregs
		mov	newcmd, #1	' single step command
		call	#sendcmd	' send info
		call	#waitcmdclear	' wait for response
		call	#readregs	' read new registers
checkdebug_ret	ret
		
dumpregs
		mov	cogaddr, #x0
		mov	hubaddr, dbgreg_addr
		mov	hubcnt, #38*4
		getptr	shadowpc
		sub	shadowpc, membase	' adjust for VM
		call	#cogxfr_write
		add	shadowpc, membase	' adjust for VM
dumpregs_ret
		ret

readregs
		mov	cogaddr, #x0
		mov	hubaddr, dbgreg_addr
		mov	hubcnt, #38*4
		call	#cogxfr_read
		add	shadowpc, membase	' adjust for VM
		rdfast	x0, shadowpc
		mov	x0, #0		'
readregs_ret
		ret
newcmd		long 0
sendcmd
		call	#waitcmdclear
		wrlong	newcmd, cmd_addr
sendcmd_ret	ret

t0		long	0
waitcmdclear
		rdlong	t0, cmd_addr wz
	if_nz	jmp	#waitcmdclear	' far end is still processing last command
waitcmdclear_ret
		ret
		
	
'------------------------------------------------------------------------------
' routines for fast transfer of COG memory to/from hub
' "hubaddr"   is the HUB memory address
' "cogaddr"   is the COG memory address
' "hubcnt"    is the number of *bytes* to transfer
'------------------------------------------------------------------------------

cogxfr_read
		setd	.rdins, cogaddr
		add	hubcnt, #3
		shr	hubcnt, #2	' count of longs
		sub	hubcnt, #1
		setq	hubcnt
.rdins		rdlong	0-0, hubaddr
		ret
		
cogxfr_write
		setd	.rdins, cogaddr
		add	hubcnt, #3
		shr	hubcnt, #2	' count of longs
		sub	hubcnt, #1
		setq	hubcnt
.rdins		wrlong	0-0, hubaddr
		ret

temp		long 0
dest		long 0
desth		long 0	' high word of result for mul/div
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
shadowpc	long	0
opcode		long	0
info1		long	0	' debug info
info2		long	0	' debug info
stepcount	long	1	' start in single step mode
rununtil	long	0

rd		long	0
rs1		long	0
rs2		long	0
funct3		long	0
divflags	long	0

		fit	$1e0	'$1F0 is whole thing
