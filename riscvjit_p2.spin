{{
   RISC-V Emulator for Parallax Propeller
   Copyright 2017-2019 Total Spectrum Software Inc.
   Terms of use: MIT License (see the file LICENSE.txt)

   An emulator for the RISC-V processor architecture, designed to run
   in a single Propeller COG.

   This version is stand alone, i.e. does not communicate with a debug
   COG.

   Reads and writes go directly to the host HUB memory. To access COG memory
   or special registers use the CSR instructions. CSRs we know about:
      7Fx - COG registers 1F0-1FF
      BC0 - UART register
      BC1 - wait register  (writing here causes us to wait until a particular cycle)
      BC2 - debug register; writing here dumps a checksum of memory
      C00 - cycle counter

   Theory of operation:
     We pre-compile instructions and run them from a cache.
     Each RISC-V instruction maps to up to 4 PASM instructions.
     They run inline until the end of the cache, where we have to
     have a return (probably through a _ret_ prefix). On return
     the ptrb register contains the next pc we should execute;
     this is initialized to the next pc after the cache, so if
     we fall through everything is good.
}}

'#define SPECIAL_DEBUG
'#define DEBUG_TRACE
#define ALWAYS

CON
{
  cache configuration
  on the RiscV side, we have 4 instructions per cache line
    this translates to 16 bytes
  we have 64 cache lines
}
' bits per cache line
TOTAL_CACHE_BITS = 9
PC_CACHELINE_BITS = 5
PC_TAGIDX_BITS = (TOTAL_CACHE_BITS-PC_CACHELINE_BITS)

PC_CACHELINE_LEN = (1<<PC_CACHELINE_BITS)

' given a pc, we calculate the offset within a cache line by "pc & PC_CACHEOFFSET_MASK"
PC_CACHEOFFSET_MASK = (PC_CACHELINE_LEN-1)	' finds offset within cache line
' given a pc, we calculate the cache line number (tag index) by "(pc >> PC_CACHELINE_BITS) & PC_TAGIDX_MASK"
PC_NUMTAGS = (1<<PC_TAGIDX_BITS)
PC_TAGIDX_MASK = (PC_NUMTAGS-1)

TOTAL_CACHE_MASK = (1<<TOTAL_CACHE_BITS)-1

CON
  WC_BITNUM = 20
  WZ_BITNUM = 19
  IMM_BITNUM = 18
  BASE_OF_MEM = $2000  ' 8K
  TOP_OF_MEM = $42000   ' 256K + 8K
  RX_PIN = 63
  TX_PIN = 62

  '' smart pin modes
  _txmode       = %0000_0000_000_0000000000000_01_11110_0 'async tx mode, output enabled for smart output
  _rxmode       = %0000_0000_000_0000000000000_00_11111_0 'async rx mode, input  enabled for smart input

  CYCLES_PER_SEC = 160_000_000
  CLOCK_MODE = $010007f8
  BAUD = 230_400
  
  
PUB start(params)
  coginit(0, @enter, 0)

DAT
		org 0
enter
x0		nop
x1		jmp	#x3
x2		long	TOP_OF_MEM
x3		nop

x4		loc	ptrb, #\BASE_OF_MEM
x5		nop
x6		hubset	#0
x7		mov	x1, #$1ff	' will count down

		' initialize LUT memory
x8		neg	x3,#1
x9		nop
x10		wrlut	x3,x1
x11		djnz	x1,#x10

x12		loc	ptra, #\boot_msg
x13		call	#ser_init
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
memstart	long	BASE_OF_MEM
memend		long	TOP_OF_MEM
debug_trace	long	0

cache_line_first_pc long 0
l1tags		long	0[PC_NUMTAGS]

		'' now start execution
startup
		mov	temp,#15
.lp		altd	temp, #x2
		mov	0-0, #0
		djnz	temp, #.lp

		'' set the pc to ptrb
set_pc
		mov	cachepc, ptrb
		and	cachepc, #TOTAL_CACHE_MASK
		mov	cache_offset, ptrb
		and	cache_offset, #PC_CACHEOFFSET_MASK
		mov	tagidx, ptrb
		shr	tagidx, #PC_CACHELINE_BITS
		and	tagidx, #PC_TAGIDX_MASK
		
#ifdef DEBUG_TRACE		
		test	debug_trace, #1	wz
	if_nz	call	#\debug_print
#endif	

		andn	ptrb, #PC_CACHEOFFSET_MASK   	     	' back ptrb up to start of line
		alts	tagidx, #l1tags	
		mov	temp, tagidx
		cmp	ptrb, temp wz
#ifdef SPECIAL_DEBUG
		call	#recompile
#else		
	if_z	add	ptrb, #PC_CACHELINE_LEN	' skip to start of next line
	if_nz	call	#recompile
#endif	
		push	#set_pc
#ifdef SPECIAL_DEBUG
		cmp	ptrb, ##$4770 wz
	if_nz	jmp	#.skip_special
		call	#dump_cache
.skip_special
#endif
		add	cachepc, CACHE_START
		jmp	cachepc


''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' routine to compile the cache line starting at ptrb
''''''''''''''''''''''''''''''''''''''''''''''''''''''''
recompile
		'' update the tag index
		'' ptrb must have been masked back to the start of the line
		altd    tagidx, #l1tags
		mov	0-0, ptrb

		'' now compile into cachebaseaddr in the LUT
		'' the cache base address is formed from ptrb, which
		'' is now pointing at the start of the HUB address
		mov	cache_line_first_pc, ptrb
		mov	cacheptr, ptrb
		and	cacheptr, #TOTAL_CACHE_MASK
		mov	cachecnt, #PC_CACHELINE_LEN/4
cachelp
		rdlong	opcode, ptrb
		add	ptrb, #4
		test	opcode, #3 wcz
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
{01}		and	0, illegalinstr	' float load
{02}		and	0, illegalinstr	' custom0
{03}		and	0, illegalinstr	' fence
{04}		long	mathtab		' math immediate: points to sub table
{05}		and	0,auipc		' auipc
{06}		and	0,illegalinstr	' wide math imm
{07}		and	0,illegalinstr	' ???

{08}		long	storetab	' store
{09}		and	0,illegalinstr	' float store
{0A}		and	0,illegalinstr	' custom1
{0B}		and	0,illegalinstr	' atomics
{0C}		long	mathtab		' math reg<->reg
{0D}		and	0,lui		' lui
{0E}		and	0,illegalinstr	' wide math reg
{0F}		and	0,illegalinstr	' ???

{10}		and	0,illegalinstr
{11}		and	0,illegalinstr
{12}		and	0,illegalinstr
{13}		and	0,illegalinstr
{14}		and	0,illegalinstr
{15}		and	0,illegalinstr
{16}		and	0,illegalinstr	' custom2
{17}		and	0,illegalinstr

{18}		and	0,condbranch	' conditional branch
{19}		and	0,jalr
{1A}		and	0,illegalinstr
{1B}		and	0,jal
{1C}		long	systab	' system
{1D}		and	0,illegalinstr
{1E}		and	0,illegalinstr	' custom3
{1F}		and	0,illegalinstr


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
''     loc ptra, #immval
''     add ptra, rs1
''     rdlong rd, ptra wc
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
		and	immval, AUGPTR_MASK
		andn	locptra, AUGPTR_MASK
		or	locptra, immval
		sets	addptra, rs1
		mov	opptr, #locptra
		call	#emit2

		'' now the actual rd/wr instruction
		'' opdata contains a template like
		''   rdword SIGNWORD, loadop wc
		''
		mov	signmask, opdata ' save potential sign mask
		shr	signmask, #9
		and	signmask, #$1ff wz ' remember if there is a sign mask
		'' now change the opdata to look like
		''   rdword rd, ptra
		sets	opdata, #ptra
		setd	opdata, rd
		wrlut	opdata, cacheptr
		add	cacheptr, #1
		'' see if we need a sign extension instruction
	if_z	ret
		setd	signext_instr, rd
		sets	signext_instr, signmask
		wrlut	signext_instr, cacheptr
	_ret_	add	cacheptr, #1

locptra
		loc	ptra, #\0
addptra
		add	ptra, 0-0
ioptra
		rdlong  0-0, ptra
signext_instr
		muxc	0-0, 0-0
signmask
		long	0
		
LOC_MASK
AUGPTR_MASK
		long	$000FFFFF

sysinstr
illegalinstr
		mov	immval, ptrb
		call	#emit_pc_immval_minus_4
		mov	opptr, #imp_illegal
		jmp	#emit1

imp_illegal
		call	#\illegal_instr_error

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
		loc	ptrb, #\newpc   (compile time pc+offset)
    _ret_	mov	rd, ##retaddr  (compile time pc+4)

   template for jalr rd, rs1, offset
imp_jalr
		loc	ptrb, #\offset
		add	ptrb, rs1
   _ret_	mov	rd, ##retaddr  (compile time pc+4)
}}


Jmask		long	$fff00fff

jal
		mov	immval, opcode
		sar	immval, #20	' sign extend, get some bits in place
		and	immval, Jmask
		test	immval, #1 wc	' check old bit 20
		mov	temp, opcode
		andn	temp, Jmask
		or	immval, temp
		andn	immval, #1  	' clear low bit
		muxc	immval, ##(1<<11)
		add	immval, ptrb	' calculate branch target
		call	#emit_pc_immval_minus_4

		mov	immval, ptrb	' get return address
		mov	dest, rd
		call	#emit_mvi	' move into rd
		
		wrlut	ret_instr, cacheptr
	_ret_	add	cacheptr, #1

jalr
		' set up offset in ptrb
		and	immval, LOC_MASK wz
	if_nz	jmp	#illegalinstr    '' FIXME DEBUG CODE
		andn	imp_jalr, LOC_MASK
		or	imp_jalr, immval
		sets	imp_jalr+1, rs1
		mov	opptr, #imp_jalr
		call	#emit2
		' now emit the final load
		mov	immval, ptrb	' get return address
		mov	dest, rd
		call	#emit_mvi	' move into rd
		' now flag it with a _ret_
		mov   cmp_flag, #0
		jmp   #reset_compare_flag

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
''        cmp[s] rs1, rs2 wcz
''  if_z  loc ptrb, #\newpc
''  if_z  ret
'' ** NOTE: if "newpc" is in the same cache line, we can
'' change this to:
''       cmp[s] rs1, rs2 wcz
''  if_z  jmp #\cachepc
''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''
cmps_instr	cmps	rs1, rs2 wcz
cmp_instr	cmp	rs1, rs2 wcz
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
		shl	cmp_flag,#28		' get in high nibble
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
		'' BEWARE! ptrb has stepped up by 4, so we need to
		sub	immval, #4

		'' see if the new pc is in the same cache line
		'' subtract 4??
		
		mov    temp, immval
		sub    temp, cache_line_first_pc  ' calculate immval - cache_line_start
		cmp    temp, #PC_CACHELINE_LEN wcz
	if_ae	jmp    #normal_branch
		
		'' want to emit a conditional jump here
#ifdef ALWAYS_NOT
		'' debug code
		'' print cache_line_start, oldpc, newpc
		mov	 info1, cache_line_first_pc
		call	 #ser_hex
		mov	 info1, ptrb
		call	 #ser_hex
		mov	 info1, immval
		call	 #ser_hex
#endif		
		mov	opdata, absjump
		and	immval, #TOTAL_CACHE_MASK
		add	immval, CACHE_START
		or	opdata, immval
		andn	opdata, CONDMASK
		or	opdata, cmp_flag
		wrlut	opdata, cacheptr
#ifdef ALWAYS_NOT
		mov	info1, opdata
		call	#ser_hex
		call	#ser_nl
#endif		
	_ret_	add	cacheptr, #1
absjump
		jmp	#\0-0
		
normal_branch
		'' now write a conditional loc and ret
		call	#emit_pc_immval
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
		sets	mov_pat,rs1
		setd	mov_pat,rd
		wrlut	mov_pat,cacheptr
	_ret_	add	cacheptr,#1
mov_pat		mov	0,0

CONDMASK	long	$f0000000

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
		mov	divflags, rs1	' remainder should have sign of rs1
		abs	rs1, rs1
		abs	rs2, rs2
		call	#imp_remu
		testb	divflags, #31 wc
	_ret_	negc	rd
imp_div
		mov	divflags, rs1
		xor	divflags, rs2
		abs	rs1,rs1
		abs	rs2,rs2
		call	#imp_divu
		testb	divflags, #31 wc	' check sign
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
		jmp	#\compile_csrw


getct_instr
		getct	0-0
waitcnt_instr
		addct1 0-0, #0
		waitct1
uart_send_instr
		mov	uartchar, 0-0
		call	#\ser_tx
uart_recv_instr
		call	#\ser_rx
		mov	0-0, uartchar
debugprint_instr
		mov	debug_trace, 0-0
		call	#\debug_print
		
'=========================================================================
		'' VARIABLES
temp		long 0
dest		long 0

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
cache_offset	long	0
tagidx		long	0
cachecnt	long	0

uartchar	long	0
uartcnt		long	0
waitcycles	long	0

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
		cmps	0,sltfunc    wcz
		cmp	0,sltfunc    wcz
		xor	0,regfunc
		shr	0,regfunc    wc	' wc indicates we want shr/sar
		or	0,regfunc
		and	0,regfunc
loadtab
		rdbyte	SIGNBYTE, loadop wc
		rdword	SIGNWORD, loadop wc
		rdlong	0, loadop
		and	0, illegalinstr
		rdbyte	0, loadop
		rdword	0, loadop
		rdlong	0, loadop
		and	0, illegalinstr
storetab
		wrbyte	0, storeop
		wrword	0, storeop
		wrlong	0, storeop
		and	0, illegalinstr
		and	0, illegalinstr
		and	0, illegalinstr
		and	0, illegalinstr
		and	0, illegalinstr

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
'' some lesser used routines that can go in HUB memory
''

		orgh $800

compile_csrw
		getnib	funct3, immval, #2
		and	immval, #$1FF

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
		cmp	funct3, #$B wz	' is it one of ours?
	if_nz	jmp	#illegalinstr
	
		cmp	immval, #$1C0 wz
	if_nz	jmp	#not_uart

		'' we can only actually read *or* write
		'' if rd is x0, then we are writing
		cmp	rd, #0 wz
	if_z	jmp	#skip_uart_read
		setd	uart_recv_instr+1, rd
		mov	opptr, #uart_recv_instr
		jmp	#emit2

skip_uart_read
		'' if rs1 is x0, skip any writes
   		cmp	rs1, #0 wz
	if_z	jmp	#emit_nop
	
		'' implement uart
  		sets	uart_send_instr, rs1
		mov	opptr, #uart_send_instr
		jmp	#emit2		' return from there to caller

not_uart
		cmp	immval, #$1C1 wz
	if_nz	jmp	#not_wait
		setd	waitcnt_instr, rs1
		mov	opptr, #waitcnt_instr
		jmp	#emit2		' return from there

not_wait
		cmp	immval, #$1C2 wz
	if_nz	jmp	#not_debug
		sets	debugprint_instr, rs1
		mov	opptr, #debugprint_instr
		jmp	#emit2
not_debug
		jmp	#illegalinstr

		'' print single character in uartchar
ser_tx
		wypin	uartchar, #TX_PIN
		waitx	#20
.txflush
		testp	#TX_PIN wc
	if_nc	jmp	#.txflush
		ret

		'' receive a single character into uartchar
		'' or set it to -1 if no character available
ser_rx
		neg	uartchar, #1
		testp	#RX_PIN wc
	if_c	rdpin	uartchar, #RX_PIN
	if_c	shr	uartchar, #24
		ret
		
		'' init the serial port
		'' this sets up the RX and TX pins as smart pins
		'' and then falls through to ser_str to print
		'' a startup message
ser_init
		' set system clock
		hubset	##CLOCK_MODE
		waitx	##20_000_000/100  ' longer than necessary
		hubset	##CLOCK_MODE+3

		mov	x4, ##7 + ((CYCLES_PER_SEC / BAUD) << 16) ' bitperiod
		wrpin	##_txmode, #TX_PIN
		wxpin	x4, #TX_PIN
		dirh	#TX_PIN
		wrpin	##_rxmode, #RX_PIN
		wxpin	x4, #RX_PIN
		dirh	#RX_PIN

		waitx	##CYCLES_PER_SEC
		'' fall through
		
		'' print string pointed to by ptra
ser_str
		rdbyte	uartchar, ptra++ wz
	if_z	jmp	#done_str
		call	#ser_tx
		jmp	#ser_str
done_str
		ret

		'' print a hex number
		'' number is in info1
ser_hex
		mov	info2, #8
.hexlp
		getnib	uartchar, info1, #7
		add	uartchar, #"0"
		cmp	uartchar, #"9" wcz
	if_a	add	uartchar, #("A"-"0") - 10
		call	#ser_tx
		rol	info1, #4
		djnz	info2, #.hexlp
		mov	uartchar, #" "
		call	#ser_tx
		ret

		' enter with ptrb holding pc
illegal_instr_error
		loc	ptra, #\illegal_instruction_msg
		call	#ser_str
		mov	info1, ptrb
		call	#ser_hex
die
		jmp	#die

#ifdef DEBUG_CHECKS
badcache
		loc	ptra, #\cache_error_msg
		call	#ser_str
.lp		jmp	#.lp
#endif

		' create a checksum of memory
		' (info1, info2) are checksum
		' pa = start of mem block
		' pb = end of mem block
update_checksum
		rdbyte	temp, pa	' x := word[ptr]
		add	info1, temp	' c0 += x
		add	info2, info1	' c1 += x
		add	pa, #1 		' ptr += 2
		cmp	pa, pb wz
	if_ne	jmp	#update_checksum
		ret
		
debug_print
		mov	info1, #0	' c0 := 0
		mov	info2, #0	' c1 := 0
		mov	pa, memstart	' ptr := PROGBASE
		mov	pb, memend
		call	#update_checksum

		' now merge in x0-x31
		loc	pa, #reg_buf
		loc	pb, #reg_buf_end
		
		setq	#31
		wrlong	x0, pa
		call	#update_checksum

		and	info1, ##$FFFF
		and	info2, ##$FFFF
		shl	info2, #16
		add	info1, info2
		call	#ser_hex

		mov	uartchar, #"@"
		call	#ser_tx
		mov	info1, ptrb
		call	#ser_hex

#ifdef SPECIAL_DEBUG
		' special debug code
		mov	uartchar, #"#"
		call	#ser_tx
		mov	info1, x11	' a1
		call	#ser_hex
		mov	uartchar, #"%"
		call	#ser_tx
		mov	info1, x8	' s0
		call	#ser_hex
#endif
		loc	ptra, #\chksum_msg
		call	#ser_str
		ret
boot_msg
		byte	"RiscV P2 JIT", 10, 0
chksum_msg
		byte    "=memory chksum", 10, 0
illegal_instruction_msg
		byte	"*** ERROR: illegal instruction at: ", 0
cache_error_msg
		byte	"*** JIT ERROR: bad cache line ending", 10, 13, 0
hex_buf
		byte  0[8], 0
reg_buf
		long 0[32]
reg_buf_end

#ifdef SPECIAL_DEBUG
cache_msg
		byte	"cache ptrb cachepc: ", 0
dump_cache
		loc	ptra, #cache_msg
		call	#ser_str
		mov	info1, ptrb
		call	#ser_hex
		mov	info1, cachepc
		call	#ser_hex
		
		mov	info3, #16
		mov	info4, cachepc
		andn	info4, #$f
.dumplp
		test	info4, #3 wz
	if_nz	jmp	#.skip_nl
		call	#ser_nl
		mov	info1, info4
		call	#ser_hex
		mov	uartchar, #":"
		call	#ser_tx
.skip_nl
		rdlut	info1, info4
		call	#ser_hex
		add	info4, #1
		djnz	info3, #.dumplp

		jmp	#ser_nl
#endif
ser_nl
		mov	uartchar, #13
		call	#ser_tx
		mov	uartchar, #10
		call	#ser_tx
		ret
