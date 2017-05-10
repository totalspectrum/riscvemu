{{
   RISC-V Emulator for Parallax Propeller
   Copyright 2017 Total Spectrum Software Inc.
   Terms of use: MIT License (see the file LICENSE.txt)

   An emulator for the RISC-V processor architecture, designed to run
   in a single Propeller COG.

   The interface is pretty simple: the params array passed to the start
   method should contain:
   
   params[0] = address of command register
   params[1] = base of emulated memory (must be 4K, and programs must be linked
   	       to run there)
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
  Reads and writes go directly to the host HUB memory. To access COG memory
  or special registers use the CSR instructions. CSRs we know about:
     7Fx - COG registers 1F0-1FF
     BC0 - UART register
     BC1 - wait register
     C00 - cycle counter

  Theory of operation:
  we pre-compile instructions and run them from a cache.
  Each RISC-V instruction maps to up to 4 PASM instructions.
  They run inline until the end of the cache, where we have to
  have a return (probably through a _ret_ prefix). On return
  the emupc register contains the next pc we should execute;
  this is initialized to the next pc after the cache, so if
  we fall through everything is good.

  NOTES:
    During runtime, the actual pc is kept in the ptrb register.
}}

#define DEBUG

CON
  CACHE_LINES = 64	' 1 line per instruction
  WC_BITNUM = 20
  WZ_BITNUM = 19
  IMM_BITNUM = 18
  
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
x0		nop
x1		rdlong	cmd_addr, ptra++
x2		rdlong	x2, ptra++
x3		rdlong	temp, ptra++

x4		rdlong	ptrb, ptra++		
x5		add	x2, temp	' set up stack pointer
x6		rdlong	dbgreg_addr, ptra++
x7		mov	x1, #$1ff	' will count down

		' initialize LUT memory
x8		neg	x3,#1
x9		nop
x10		wrlut	x3,x1
x11		djnz	x1,#x10

x12		nop
x13		nop
x14		nop
x15		jmp	#startup

x16		long	0[16]
		'' these registers must immediately follow x0-x31
x32
shadowpc	long	0
opcode		long	0
info1		long	0	' debug info
info2		long	0	' debug info
info3		long	0
info4		long	0
rununtil	long	0
stepcount	long	1	' start in single step mode
		
		'' now start execution
startup
		mov	temp,#15
.lp		altd	temp, #x2
		mov	0-0, #0
		djnz	temp, #.lp
		
set_pc
		getbyte	cachepc, ptrb, #0	' low 2 bits of ptrb must be 0
		getnib	tagidx, ptrb, #1
#ifdef DEBUG
		mov	info3, ptrb
		mov	info4, cachepc
		call	#checkdebug
#endif
		add	tagidx, #$100		' start of tag data
		andn	ptrb, #$f      	     	' back ptrb up to start of line
		rdlut	temp, tagidx
		cmp	ptrb, temp wz
	if_z	add	ptrb, #16	' skip to start of next line
	if_nz	call	#recompile
		push	#set_pc
		add	cachepc, CACHE_START
		jmp	cachepc


''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' routine to compile the cache line starting at ptrb
''''''''''''''''''''''''''''''''''''''''''''''''''''''''
recompile
		'' update the tag index
		wrlut	ptrb, tagidx

		'' now compile into cachebaseaddr in the LUT
		'' the cache base address is formed from ptrb, which
		'' is now pointing at the start of the HUB address
		getbyte	cacheptr, ptrb, #0
		mov	cachecnt, #4
cachelp
		rdlong	opcode, ptrb++
		test	opcode, #3 wc,wz
  if_z_or_c	jmp	#do_illegalinstr		' low bits must both be 3
  		call	#decodei
		mov	temp, opcode
		shr	temp, #2
		and	temp, #$1f
		alts	temp, #optable
		mov	opdata, 0-0		' fetch long from table
		test	opdata, CONDMASK wz	' are upper bits 0?
	if_nz	jmp	#getinstr
		alts	funct3, opdata		' do table indirection
		mov	opdata, 0-0
getinstr
		mov	temp, opdata
		and	temp, #$1ff
		call	temp			' compile the instruction
pad_instructions
		'' pad the instruction translation to a multiple of 4
		test	cacheptr, #3 wz
	if_nz	call	#emit_nop
	if_nz	jmp	#pad_instructions

		djnz	cachecnt, #cachelp
		'' finish the cache line

		'' now set the last instructions EXEC flag to 0 (_ret_)
finish_cache_line
		mov	cmp_flag, #0
		jmp	#reset_compare_flag
		
do_illegalinstr
		call	#illegalinstr
		jmp	#pad_instructions
''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' table of compilation routines for the various opcodes
' if the upper 4 bits of the entry is 0, the entry is
' actually a pointer to another table (indexed by funct3)
' otherwise, the bottom 9 bits is a jump address and
' the upper 23 bits is a parameter
''''''''''''''''''''''''''''''''''''''''''''''''''''''''
optable
{00}		long	loadtab		' load
{01}		cmp	0, illegalinstr	' float load
{02}		cmp	0, illegalinstr	' custom0
{03}		cmp	0, illegalinstr	' fence
{04}		long	mathtab		' math immediate: points to sub table
{05}		cmp	0,auipc		' auipc
{06}		cmp	0,illegalinstr	' wide math imm
{07}		cmp	0,illegalinstr	' ???

{08}		long	storetab	' store
{09}		cmp	0,illegalinstr	' float store
{0A}		cmp	0,illegalinstr	' custom1
{0B}		cmp	0,illegalinstr	' atomics
{0C}		long	mathtab		' math reg<->reg
{0D}		cmp	0,lui		' lui
{0E}		cmp	0,illegalinstr	' wide math reg
{0F}		cmp	0,illegalinstr	' ???

{10}		cmp	0,illegalinstr
{11}		cmp	0,illegalinstr
{12}		cmp	0,illegalinstr
{13}		cmp	0,illegalinstr
{14}		cmp	0,illegalinstr
{15}		cmp	0,illegalinstr
{16}		cmp	0,illegalinstr	' custom2
{17}		cmp	0,illegalinstr

{18}		cmp	0,condbranch	' conditional branch
{19}		cmp	0,jalr
{1A}		cmp	0,illegalinstr
{1B}		cmp	0,jal
{1C}		long	systab	' system
{1D}		cmp	0,illegalinstr
{1E}		cmp	0,illegalinstr	' custom3
{1F}		cmp	0,illegalinstr


sardata		sar	0,0
subdata		sub	0,0


SIGNOP_BIT	long	$40000000	' RISCV bit for changing shr/sar

		'' code for typical reg-reg functions
		'' such as add r0,r1
		'' needs to handle both immediate and reg rs2
		'' it does this by looking at the opcode:
		'' $13 is immediate
		'' $33 is reg-reg

regfunc
		cmp	rd, #0	wz	' if rd == 0, emit nop
	if_z	jmp	#emit_nop

		testb	opdata, #WC_BITNUM wc 	' check for sar/shr?
	if_nc	jmp	#nosar
		test	opcode, SIGNOP_BIT wc	' want sar instead of shr?
	if_c	mov	opdata, sardata
		and	immval, #$1f		' only low 5 bits of immediate
nosar
		'' check for immediates
		test	opcode, #$20 wz
		andn	opdata, #$1ff	' zero out source in template instruction
	if_nz	jmp	#reg_reg
		bith	opdata, #IMM_BITNUM

		'
		' emit an immediate instruction with optional large prefix
		' and with dest being the result
		'
		mov	dest, rd
		call	#emit_mov_rd_rs1
		jmp	#emit_big_instr

		'
		' register<-> register operation
		'
reg_reg
		'' the multiply instructions are in the same
		'' opcode as the "regular" math ones;
		'' check for them here
		mov	temp, opcode
		shr	temp, #25
		and	temp, #$3f
		cmp	temp, #1 wz
	if_z	jmp	#muldiv
	
		testb	opdata, #WZ_BITNUM wc
	if_nc	jmp	#nosub
		test	opcode, SIGNOP_BIT  wc	' need sub instead of add?
	if_c	mov	opdata, subdata
nosub
		'
		' if rd matches rs2, move rd out of the way
		'
		cmp	rd, rs2 wz
	if_nz	jmp	#notemp
		sets	mov_temp_op, rd
		wrlut	mov_temp_op, cacheptr
		add	cacheptr, #1
		mov	rs2, #temp

notemp
		call	#emit_mov_rd_rs1
		'' now do the operation
		sets	opdata, rs2
		setd  	opdata, rd
emit_opdata_and_ret
		wrlut	opdata, cacheptr
	_ret_	add	cacheptr, #1

mov_temp_op
		mov	temp, 0-0

'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'' for multiply and divide we just generate
''    mov rs1, <rs1>
''    mov rs2, <rs2>
''    call #routine
''    mov <rd>, dest
multab
	call	#\imp_mul
	call	#\illegalinstr
	call	#\illegalinstr
	call	#\imp_muluh
	call	#\imp_div
	call	#\imp_divu
	call	#\imp_rem
	call	#\imp_remu
	
muldiv
	alts	funct3, #multab
	mov	temp, 0-0
	sets	mul_templ, rs1
	sets	mul_templ+1, rs2
	mov	mul_templ+2, temp
	setd	mul_templ+3, rd
	mov	opptr, #mul_templ
	jmp	#emit4
	
mul_templ
	mov	rs1, 0-0
	mov	rs2, 0-0
	call	#\imp_mul
	mov	0-0, rd
	
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'' variants for sltu and slt
'' these should generate something like:
''     cmp	rs1, rs2 wc
''     mov	rd, #0
''     muxc	rd, #1
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
sltfunc
		cmp	rd, #0	wz	' if rd == 0, emit nop
	if_z	jmp	#\emit_nop
	
		andn	opdata, #$1ff	' zero out source
		setd	sltfunc_pat, rd
		setd	sltfunc_pat+1, rd
		'' check for immediate
		test	opcode, #$20 wz
	if_nz	jmp	#slt_reg
	
		'' set up cmp with immediate here	
		bith	opdata, #IMM_BITNUM
		mov	dest, rs1
		call	#emit_big_instr	' cmp rs1, ##immval
		jmp	#slt_fini
slt_reg
		'' for reg<->reg, output cmp rs1, rs2
		sets	opdata, rs2
		setd	opdata, rs1
		wrlut	opdata, cacheptr
		add	cacheptr, #1
slt_fini
		mov	opptr, #sltfunc_pat
		jmp	#emit2		' return from there to our caller
		
sltfunc_pat
		mov	0-0, #0
		muxc	0-0, #1

''''''''''''''''''''''''''''''''''''''''''''''''''''''
'' load/store operations
'' these look like:
''     mov ptra, rs1
''     rdlong rd, ptra[##immval] wc
''     muxc rd, SIGNMASK (optional, only if wc set on prev. instruction)
''
'' the opdata field has:
''   instruction set up for rd/write (load/store share most code)
''   dest field has mask to use for sign extension (or 0 if no mask)
''   src field is address of this routine
''

SIGNBYTE	long	$FFFFFF00
SIGNWORD	long	$FFFF0000

storeop
		'' RISC-V has store value in rs2, we want it in rd
		andn	immval, #$1f
		or	immval, rd
		mov	rd, rs2
		jmp	#ldst_common
loadop
		cmp	rd, #0	wz	' if rd == 0, emit nop
	if_z	jmp	#emit_nop
ldst_common
		sets	movptra, rs1
		wrlut	movptra, cacheptr
		add	cacheptr, #1
		and	immval, AUGPTR_MASK
		bith	immval, #23	' set to force ptra relative

		'' check for sign extend
		'' the dest field of opdata will have the mask we
		'' should use to sign extend, or 0 for no sign extension
		mov	signmask, opdata ' save potential sign mask
		shr	signmask, #9
		and	signmask, #$1ff wz
		mov	dest, rd
		call	#emit_big_instr
		cmp	signmask, #0 wz
	if_z	ret
		setd	signext_instr, rd
		sets	signext_instr, signmask
		wrlut	signext_instr, cacheptr
	_ret_	add	cacheptr, #1

movptra
		mov	ptra, 0-0
signext_instr
		muxc	0-0, 0-0
signmask
		long	0
		
LOC_MASK
AUGPTR_MASK
		long	$000FFFFF

sysinstr
illegalinstr
		mov	opptr, #imp_illegal
		jmp	#emit3

imp_illegal
		call	#\dumpregs
		mov	newcmd, #2
		jmp	#\sendcmd_and_wait

    		'' deconstruct instr
decodei
		mov	immval, opcode
		sar	immval, #20
		mov	rs2, immval
		and	rs2, #$1f
		mov	rs1, opcode
		shr	rs1, #15
		and	rs1, #$1f
		mov	funct3, opcode
		shr	funct3,#12
		and	funct3,#7
		mov	rd, opcode
		shr	rd, #7
	_ret_	and	rd, #$1f
		
auipc
		mov	immval, opcode
		and	immval, LUI_MASK
		add	immval, ptrb
		sub	immval, #4
		jmp	#lui_aui_common
lui
		mov	immval, opcode
		and	immval, LUI_MASK
lui_aui_common
		mov	dest, rd
		jmp	#emit_mvi	'' return from there
		
LUI_MASK	long	$fffff000

{{
   template for jal rd, offset
imp_jal
		loc	ptrb, #newpc   (compile time pc+offset)
    _ret_	mov	rd, ##retaddr  (compile time pc+4)

   template for jalr rd, rs1, offset
imp_jalr
		loc	ptrb, #offset
		add	ptrb, rs1
		mov	rd, ##retaddr

    small offset case (including typical 0 offset) goes:
    		mov	ptrb, rs1
		add/sub	ptrb, #offset
    _ret_	mov	rd, ##retaddr
}}


Jmask		long	$fff00fff

jal
		mov	immval, opcode
		sar	immval, #20	' sign extend, get some bits in place
		and	immval, Jmask
		mov	temp, opcode
		andn	temp, Jmask
		or	immval, temp
		test	immval, #1 wc	' check old bit 20
		andn	immval, #1  	' clear low bit
		bitc	immval, #11	' set bit 11
		add	immval, ptrb	' calculate branch target
		call	#emit_pc_immval_minus_4
emit_save_retaddr
		mov	immval, ptrb	' get return address
		mov	dest, rd
		call	#emit_mvi	' move into rd
		
		wrlut	ret_instr, cacheptr
	_ret_	add	cacheptr, #1

jalr
		' set up offset in ptrb
		and	immval, LOC_MASK
		andn	imp_jalr, LOC_MASK
		or	imp_jalr, immval
		sets	imp_jalr+1, rs1
		mov	opptr, #imp_jalr
		call	#emit2
		jmp	#emit_save_retaddr

imp_jalr
		loc	ptrb, #\(0-0)
		add	ptrb, 0-0
''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'' conditional branch
''   beq rs1, rs2, immval
'' the immediate is encoded a bit oddly, with parts where
'' rd would be
'' rather than using a dispatch table, we decode the funct3
'' bits directly
'' the bits are abc
'' where "a" selects for equal (0) or lt (1) (for us, Z or C flag)
''       "b" selects for signed (0) or unsigned (1) compare
''       "c" inverts the sense of a
'' the output will look like:
''        cmp[s] rs1, rs2 wc,wz
''  if_z  loc ptrb, ##newpc
''  if_z  ret
''''''''''''''''''''''''''''''''''''''''''''''''''''''''
cmps_instr	cmps	rs1, rs2 wc,wz
cmp_instr	cmp	rs1, rs2 wc,wz
cmp_flag	long	0
ret_instr
		ret

loc_instr	loc	ptrb, #\(0-0)

emit_pc_immval_minus_4
		sub	immval, #4
emit_pc_immval
		andn	loc_instr, LOC_MASK
		and	immval, LOC_MASK
		or	loc_instr, immval
		wrlut	loc_instr, cacheptr
	_ret_	add	cacheptr, #1

'' reset the compare flag on the last instruction
'' to whatever is in cmp_flag
reset_compare_flag
		sub	cacheptr, #1
		rdlut	temp, cacheptr
		andn	temp, CONDMASK
		or	temp, cmp_flag
		wrlut	temp, cacheptr
	_ret_	add	cacheptr, #1

condbranch
		test	funct3, #%100 wz
	if_z	mov	cmp_flag, #%1010	' IF_Z
	if_nz	mov	cmp_flag, #%1100	' IF_C
		test	funct3, #%001 wz
	if_nz	xor	cmp_flag, #$f		' flip sense
		test	funct3, #%010 wz
		'' write the compare instruction
	if_z	mov	opdata,cmps_instr
	if_nz	mov	opdata, cmp_instr
		setd	opdata, rs1
		sets	opdata, rs2
		wrlut	opdata, cacheptr
		add	cacheptr, #1

		'' now we need to calculate the new pc
		'' this means re-arranging some bits
		'' in immval
		andn 	immval, #$1f
		or	immval, rd
		test  	immval, #1 wc
		bitc	immval, #11
		andn	immval, #1
		add	immval, ptrb
		call	#emit_pc_immval_minus_4
		
		'' now write a conditional loc and ret
		shl	cmp_flag,#28		' get in high nibble
		call	#reset_compare_flag	' change conditional flag of last instruction
		mov	opdata, ret_instr
		andn	opdata, CONDMASK
		or	opdata, cmp_flag
		wrlut	opdata, cacheptr
	_ret_	add	cacheptr, #1

		
''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' helper routines for compilation
''''''''''''''''''''''''''''''''''''''''''''''''''''''''

' emit 1-4 instructions from opptr to cacheptr
emit4
		altd	opptr, #0
		wrlut	0-0, cacheptr
		add	cacheptr, #1
		add	opptr, #1
emit3
		altd	opptr, #0
		wrlut	0-0, cacheptr
		add	cacheptr, #1
		add	opptr, #1
emit2
		altd	opptr, #0
		wrlut	0-0, cacheptr
		add	cacheptr, #1
		add	opptr, #1
emit1
		altd	opptr, #0
		wrlut	0-0, cacheptr
	_ret_	add	cacheptr, #1

' emit code to copy the 32 bit immediate value in immval into
' the register pointed to by "dest"
mvins 	      	mov     0-0,#0
emit_mvi
		mov	opdata, mvins
emit_big_instr
		mov	big_temp_0+1,opdata
		cmp	dest, #x0 wz
	if_z	ret	' never write to x0
		mov	temp, immval
		shr	temp, #9	wz
		and	big_temp_0, AUG_MASK
		or	big_temp_0, temp
		and	immval, #$1FF
		sets	big_temp_0+1, immval
		setd	big_temp_0+1, dest
		mov	opptr, #big_temp_0
		jmp	#emit2
big_temp_0
		mov	0-0, ##0-0

AUG_MASK	long	$ff800000

'
' emit a no-op (just return)
'
emit_nop
		wrlut	emit_nop_pat,cacheptr
	_ret_	add	cacheptr,#1
emit_nop_pat
		or	0,0	' a real nop won't work because we can't prefix with _ret_

'
' emit a mov of rs1 to rd
'
emit_mov_rd_rs1
#ifdef NEVER
		cmp	rd,rs1 wz
	if_z	ret			' skip the mov if both the same
#endif
		sets	mov_pat,rs1
		setd	mov_pat,rd
		wrlut	mov_pat,cacheptr
	_ret_	add	cacheptr,#1
mov_pat		mov	0,0

CONDMASK	long	$f0000000
''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' debug routines
''''''''''''''''''''''''''''''''''''''''''''''''''''''''

checkdebug
		tjz	stepcount, #checkdebug_ret
		djnz	stepcount, #checkdebug_ret
		call	#dumpregs
		mov	newcmd, #1	' single step command
		call	#sendcmd_and_wait	' send info
		call	#readregs	' read new registers
checkdebug_ret
		ret
		
dumpregs
		mov	shadowpc, ptrb
		setq	#40	' transfer 38 registers
	_ret_	wrlong	x0, dbgreg_addr

readregs
		setq	#40	' transfer 38 registers
		rdlong	x0, dbgreg_addr
		mov	ptrb, shadowpc
	_ret_	mov	x0, #0		' always 0 in x0!

newcmd		long 0
sendcmd_and_wait
		call	#waitcmdclear
		wrlong	newcmd, cmd_addr
		jmp	#waitcmdclear

		'' this command is a little unusual in that it uses x0
		'' as a temporary;
		'' that's OK because at the end x0 is guaranteed
		'' to be 0
waitcmdclear
		rdlong	x0, cmd_addr wz
	if_nz	jmp	#waitcmdclear	' far end is still processing last command
waitcmdclear_ret
		ret
		
'=========================================================================
' MATH ROUTINES
'=========================================================================
imp_mul
		qmul	rs1, rs2
    _ret_	getqx	rd

imp_muluh
		qmul	rs1, rs2
    _ret_	getqy	rd

    		'' calculate rs1 / rs2
imp_divu
		tjz	rs2, #div_by_zero
		setq	#0
		qdiv	rs1, rs2
	_ret_	getqx	rd

div_by_zero
	_ret_	neg	rd, #1

imp_remu
		tjz	rs2, #rem_by_zero
		setq	#0
		qdiv	rs1, rs2
	_ret_	getqy	rd

rem_by_zero
	_ret_	mov	rd, rs1

imp_rem
		abs	rs1, rs1 wc	' remainder should have sign of rs1
		abs	rs2, rs2
		call	#imp_remu
	_ret_	negc	rd
imp_div
		mov	temp, rs1
		xor	temp, rs2
		abs	rs1,rs1
		abs	rs2,rs2
		call	#imp_divu
		testb	temp, #31 wc	' check sign
	_ret_	negc	rd


'=========================================================================
' system instructions
'=========================================================================

''
'' the register read/write routines
'' these basically look like:
''    mov rd, <reg>
''    op  <reg>, rs1
''
csrrw
		getnib	funct3, immval, #2
		and	immval, #$1FF

#ifndef DEBUG
		'' check for COG I/O e.g. 7f4
		cmp	funct3, #7 wz
	if_nz	jmp	#not_cog

		'' first, emit a mov to get the old register value
		cmp	rd, #0 wz
	if_z	jmp	#skip_rd
		setd	mov_pat, rd
		sets	mov_pat, immval
		wrlut	mov_pat, cacheptr
		add	cacheptr, #1
skip_rd
		'' now emit the real operation
		setd	opdata, immval
		sets	opdata, rs1
		jmp	#emit_opdata_and_ret
#endif
not_cog
		'' check for standard read-only regs
		cmp	funct3, #$C wz
	if_nz	jmp	#not_standard

		'' write to 0? that's a no-op
	    	cmp	rd, #0 wz
	if_z	jmp	#emit_nop

		'' $c00 == mcount (cycles counter)
		cmp	immval, #0 wz
	if_nz	jmp	#illegalinstr

		mov	opdata, getct_instr
  		setd	opdata, rd
		jmp	#emit_opdata_and_ret

		'' here's where we do our non-standard registers
		'' BC0 == UART
not_standard
		cmp	#funct3, #$B wz	' is it one of ours?
	if_nz	jmp	#illegalinstr
	
		cmp	immval, #$1C0 wz
	if_nz	jmp	#not_uart

		'' if rs1 is x0, skip any writes
   		cmp	rs1, #0 wz
	if_z	jmp	#emit_nop
	
		'' implement uart
  		sets	wrcmd_instr, rs1
		mov	opptr, #wrcmd_instr
		jmp	#emit4		' return from there to caller
not_uart
		cmp	immval, #$1C1 wz
	if_nz	jmp	#not_wait
		setd	waitcnt_instr, rs1
		mov	opptr, #waitcnt_instr
		jmp	#emit2		' return from there

not_wait
		jmp	#illegalinstr


getct_instr
		getct	0-0
waitcnt_instr
		addct1 0-0, #0
		waitct1
wrcmd_instr
		mov	newcmd, 0-0
		shl	newcmd, #4
		or	newcmd, #$F
		jmp	#\sendcmd_and_wait

		
'=========================================================================
		'' VARIABLES
temp		long 0
dest		long 0
dbgreg_addr	long 0	' address where registers go in HUB during debug
cmd_addr	long 0	' address of HUB command word

rd		long	0
rs1		long	0
rs2		long	0
immval		long	0
funct3		long	0
opdata
divflags	long	0

opptr		long	0
cacheptr	long	0

CACHE_START	long	$200	'start of cache area: $000-$0ff in LUT
cachepc		long	0
tagidx		long	0
cachecnt	long	0

	''
	'' opcode tables
	''
start_of_tables
''''' math indirection table
'' upper bits are acutlly instructions we wish to use
'' dest bits contain flags: 2 -> test for shr/sar 
mathtab
		add	0,regfunc    wz	' wz indicates we want add/sub
		shl	0,regfunc    wc ' wc indicates to regfunct that it's a shift
		cmps	0,sltfunc    wc,wz
		cmp	0,sltfunc    wc,wz
		xor	0,regfunc
		shr	0,regfunc    wc	' wc indicates we want shr/sar
		or	0,regfunc
		and	0,regfunc
loadtab
		rdbyte	0, #loadop
		rdword	0, #loadop
		rdlong	0, #loadop
		cmp	0, illegalinstr
		rdbyte	SIGNBYTE, #loadop wc
		rdword	SIGNWORD, #loadop wc
		rdlong	0, #loadop
		cmp	0, illegalinstr
storetab
		wrbyte	0, #storeop
		wrword	0, #storeop
		wrlong	0, #storeop
		cmp	0, illegalinstr
		cmp	0, illegalinstr
		cmp	0, illegalinstr
		cmp	0, illegalinstr
		cmp	0, illegalinstr

systab		jmp	#\illegalinstr
		mov	0,csrrw
		or	0,csrrw
		andn	0,csrrw
		jmp	#\illegalinstr
		jmp	#\illegalinstr
		jmp	#\illegalinstr
		jmp	#\illegalinstr
end_of_tables

		fit	$1f0

''
'' some lesser used routines we wanted to put in HUB
'' but as presently organized, that's difficult :(
''

		.orgh
		'' initialize serial routines
ser_init
