'#define DEBUG_ENGINE
'#define USE_DISASM
'#define USE_LUT_CACHE

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
      C00 - cycle counter
      C80 - cycle counter high
      
   Theory of operation:
     We pre-compile instructions and run them from a cache.
     Each RISC-V instruction maps to up to 4 PASM instructions.
     They run inline until the end of the cache, where we have to
     have a return (probably through a _ret_ prefix). On return
     the ptrb register contains the next pc we should execute;
     this is initialized to the next pc after the cache, so if
     we fall through everything is good.
}}

#define ALWAYS

CON
  WC_BITNUM = 20
  WZ_BITNUM = 19
  IMM_BITNUM = 18
  BASE_OF_MEM = $4000   ' 16K
  TOP_OF_MEM = $78000   ' leaves 32K free at top for cache and debug
  HIBIT = $80000000

  RV_SIGNOP_BITNUM = 30		' RISCV bit for changing shr/sar
  
DAT
		org 0
		'' initial COG boot code
		cogid	   pa
		setq	   #0
		coginit	   pa, #$20
		' config area
		orgh $10
		long	   0
		long	   23_000_000	' frequency
		long	   0		' clock mode
		long	   230_400	' baud

		orgh $20
		org 0
enter
x0		nop
x1		jmp	#x3
x2		long	TOP_OF_MEM
x3		nop

x4		loc	ptrb, #\BASE_OF_MEM
x5		rdlong	temp, #$18	' get old clock mode
x6		hubset	temp
x7		mov	x1, #$1ff	' will count down

		' initialize LUT memory
x8		neg	x3,#1
x9		nop
x10		wrlut	x3,x1
x11		djnz	x1,#x10

x12		nop
x13		call	#ser_init
x14		call	#jit_init
x15		jmp	#startup

x16		long	0[16]
		'' these registers must immediately follow x0-x31
x32
opcode		long	0

uart_num	long	0
uart_char	long	0
uart_str	long	0
uart_temp	long	0

info2
dis_temp2	long	0
dis_temp1	long	0
dis_ptr		long	0
dis_cnt		long	0
cycleh		long	0
lastcnt		long	0
chfreq		long	$80000000

		'' ISR for CT3 == 0
ct3_isr
		addct3	lastcnt, chfreq
		testb	lastcnt, #31 wc
		addx	cycleh, #0
		reti3
		
		
''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' table of compilation routines for the various opcodes
' the lower 20 bits is generally the address to jump to;
' the exception is that if the upper bit is set then
' it's a pointer to another table indexed by func3
''''''''''''''''''''''''''''''''''''''''''''''''''''''''
optable
{00}		long	HIBIT + loadtab			' TABLE: load instructions
{01}		long	illegalinstr			' float load
{02}		long	HIBIT + custom0tab		' TABLE: custom0 instructions
{03}		long	illegalinstr			' fence
{04}		long	HIBIT + mathtab			' TABLE: math immediate
{05}		long	auipc				' auipc instruction
{06}		long	illegalinstr			' wide math imm
{07}		long	illegalinstr			' ???

{08}		long	HIBIT + storetab		' TABLE: store instructions
{09}		long	illegalinstr			' float store
{0A}		long	HIBIT + custom1tab		' TABLE: custom1
{0B}		long	illegalinstr			' atomics
{0C}		long	HIBIT + mathtab			' TABLE: math reg<->reg
{0D}		long	lui				' lui
{0E}		long	illegalinstr			' wide math reg
{0F}		long	illegalinstr			' ???

{10}		long	illegalinstr
{11}		long	illegalinstr
{12}		long	illegalinstr
{13}		long	illegalinstr
{14}		long	illegalinstr
{15}		long	illegalinstr
{16}		long	illegalinstr	' custom2
{17}		long	illegalinstr

{18}		long	condbranch	' conditional branch
{19}		long	jalr
{1A}		long	illegalinstr
{1B}		long	jal
{1C}		long	HIBIT + systab	' system
{1D}		long	illegalinstr
{1E}		long	illegalinstr	' custom3
{1F}		long	illegalinstr


sardata		sar	0,0
subdata		sub	0,0

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
		testb	opcode, #RV_SIGNOP_BITNUM wc	' want sar instead of shr?
	if_c	mov	opdata, sardata
		and	immval, #$1f		' only low 5 bits of immediate
nosar
		'' check for immediates
		test	opcode, #$20 wz
		andn	opdata, #$1ff	' zero out source in template instruction
	if_nz	jmp	#reg_reg
		bith	opdata, #IMM_BITNUM

		' special case: addi xa, x0, N
		' can be translated as mv x0, N
		' we can tell it's an add because it will have WZ_BITNUM set
		testb	opdata, #WZ_BITNUM wc
	if_c	jmp	#handle_addi
reg_imm
		'
		' emit an immediate instruction with optional large prefix
		' and with dest being the result
		'
		mov	dest, rd
		call	#emit_mov_rd_rs1
		jmp	#emit_big_instr

		' handle addi instruction specially
		' if we get addi R, x0, N
		' we emit mov R, #N instead
		' similarly addi R, N, 0
		' can become mov R, N
handle_addi
		cmp	immval, #0 wcz
	if_z	jmp	#emit_mov_rd_rs1
		cmp	rs1, #x0 wz
	if_z	mov	dest, rd
	if_z	jmp	#emit_mvi
		' convert addi A, B, -N to sub A, B, N
		cmp	immval, #0 wcz
	if_ae	jmp	#reg_imm
		neg	immval
		mov	opdata, subdata
		bith	opdata, #IMM_BITNUM
		jmp	#reg_imm

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
	if_z	jmp	#hub_muldiv
	
		testb	opdata, #WZ_BITNUM wc
	if_nc	jmp	#nosub
		testb	opcode, #RV_SIGNOP_BITNUM  wc	' need sub instead of add?
	if_c	mov	opdata, subdata
nosub
		'
		' if rd is not the same as rs1, we have
		' to issue an ALTR 0, #rd
		'
		cmp	rd, rs1 wz
	if_z	jmp	#noaltr
		sets	altr_op, rd
		mov	jit_instrptr, #altr_op
		call	#emit1
noaltr
		'' now do the operation
		sets	opdata, rs2
		setd  	opdata, rs1
emit_opdata_and_ret
		mov	jit_instrptr, #opdata
		jmp	#emit1

altr_op
		altr	x0, #0-0

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
	
mul_templ
	mov	rs1, 0-0
	mov	rs2, 0-0
	call	#\imp_mul
	mov	0-0, rd
	
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'' variants for sltu and slt
'' these should generate something like:
''     cmp	rs1, rs2 wc
''     wrc	rd
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
sltfunc
		jmp	#\hub_slt_func

sltfunc_pat
		wrc	0-0

''''''''''''''''''''''''''''''''''''''''''''''''''''''
'' load/store operations
'' these look like:
''     loc ptra, #immval
''     add ptra, rs1
''     rdlong rd, ptra wc
''     muxc rd, SIGNMASK (optional, only if wc set on prev. instruction)
''
'' there are some special cases if the immediate value is small:
''    if immval == 0, just do rdlong rd, rs1 wc
''    if immval < 64 and we are a long word read, do:
''      mov ptra, rs1
''      rdlong rd, ptra[immval/4]
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
		cmp	immval, #0 wz
	if_nz	jmp	#maybe_full_ldst_imm
		mov	dest, rs1
		jmp	#final_ldst
maybe_full_ldst_imm
		mov	temp, #15
		' note: low bits of func3 == 0 for byte, 1 for word, 2 for long
		' which is what we want
		and	func3, #3
		shl	temp, func3
		cmp	immval, temp wcz
	if_a	jmp	#full_ldst_imm
		shr	immval, func3	' now immval is between 0 and 15
		'
		' OK, we can emit
		'
		' mov ptra, rs1
		' wrbyte rd, ptra[immval]
		sets	mov_to_ptra, rs1
		mov	jit_instrptr, #mov_to_ptra
		call	#emit1
		
		mov	signmask, opdata
		shr	signmask, #9
		and	signmask, #$1ff wz	' check for sign mask
		or	immval, #%1000_00000	' SUP mode for ptra[immval]
		sets	opdata, immval
		bith	opdata, #IMM_BITNUM	' change to imm mode
		
		jmp	#do_opdata_and_sign
full_ldst_imm
		and	immval, LOC_MASK
		andn	locptra, LOC_MASK
		or	locptra, immval
		sets	addptra, rs1
		mov	jit_instrptr, #locptra
		call	#emit2

		mov	dest, #ptra	' use ptra for address
final_ldst
		'' now the actual rd/wr instruction
		'' opdata contains a template like
		''   rdword SIGNWORD, loadop wc
		''
		mov	signmask, opdata ' save potential sign mask
		shr	signmask, #9
		and	signmask, #$1ff wz ' remember if there is a sign mask
		'' now change the opdata to look like
		''   rdword rd, ptra
		sets	opdata, dest
do_opdata_and_sign		
		setd	opdata, rd
		mov	jit_instrptr, #opdata
		call	#emit1

		'' see if we need a sign extension instruction
	if_z	ret
		setd	signext_instr, rd
		sets	signext_instr, signmask
		mov	jit_instrptr, #signext_instr
		mov	pb, #1
		jmp	#jit_emit

locptra
		loc	ptra, #\0
addptra
		add	ptra, 0-0
mov_to_ptra
		mov	ptra, 0-0
ioptra
		rdlong  0-0, ptra
signext_instr
		muxc	0-0, 0-0
signmask
		long	0

wrpin_table
		wrpin	0, 0
		wxpin	0, 0
		wypin	0, 0
		jmp	#\illegalinstr

rdpin_table
		wrc	0		' NOTE: S is nonzero, only D is used
		rdpin	0, 0
		rqpin	0, 0
		akpin	0		' NOTE: D is 1, only S is used
		
LOC_MASK
		long	$000FFFFF
dirinstr
		dirl	0
testbit_instr
		test	0-0, #1 wc
testpin_instr
		testp	0-0 wc
		
sysinstr
illegalinstr
		jmp	#hub_illegalinstr

imp_illegal
		call	#\illegal_instr_error

		
auipc
		jmp	#\hub_compile_auipc
lui
		jmp	#\hub_compile_lui
		
LUI_MASK	long	$fffff000

{{
   template for jal rd, offset
imp_jal
		mov	rd, ##retaddr  (compile time pc+4)
	_ret_	loc	ptrb, #\newpc   (compile time pc+offset)

   template for jalr rd, rs1, offset
imp_jalr
		mov	rd, ##retaddr  (compile time pc+4)
		loc	ptrb, #\offset
	_ret_	add	ptrb, rs1

   if offset == 0 (common for ret) use
               mov      rd, ##retaddr
	_ret_  mov      ptrb, rs1
}}


Jmask		long	$fff00fff

jal
		cmp	rd, #0 wz 		    ' if no writeback, just do the jump
	if_e	jmp	#skip_write
		mov	immval, ptrb	' get return address
		mov	dest, rd
		call	#emit_mvi	' move into rd
skip_write
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
		mov	jit_condition, #$F	    ' unconditional jump 		
		jmp	#issue_branch_cond

jalr
		' set up offset in ptrb
		and	immval, LOC_MASK wz
	if_nz	jmp	#.need_offset
		sets	imp_jalr_nooff, rs1
		mov	jit_instrptr, #imp_jalr_nooff
		call	#emit1
		jmp	#.load_retaddr
.need_offset
		andn	imp_jalr, LOC_MASK
		or	imp_jalr, immval
		sets	imp_jalr+1, rs1
		mov	jit_instrptr, #imp_jalr
		call	#emit2
.load_retaddr
		' now emit the final load
		mov	immval, ptrb	' get return address
		mov	dest, rd
		call	#emit_mvi	' move into rd

		' and emit the indirect branch code
		mov	jit_condition, #$f
		jmp	#jit_emit_indirect_branch

imp_jalr
		loc	ptrb, #\(0-0)
		add	ptrb, 0-0
imp_jalr_nooff
		mov	ptrb, 0-0
''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'' conditional branch
''   beq rs1, rs2, immval
'' the immediate is encoded a bit oddly, with parts where
'' rd would be
'' rather than using a dispatch table, we decode the func3
'' bits directly
'' the bits are abc
'' where "a" selects for equal (0) or lt (1) (for us, Z or C flag)
''       "b" selects for signed (0) or unsigned (1) compare
''       "c" inverts the sense of a
'' the output will look like:
''        cmp[s] rs1, rs2 wcz
'' then we use the JIT engine to emit a conditional branch to "newpc"
''
''''''''''''''''''''''''''''''''''''''''''''''''''''''''
cmps_instr	cmps	rs1, rs2 wcz
cmp_instr	cmp	rs1, rs2 wcz
loc_instr	loc	ptrb, #\0

emit_pc_immval_minus_4
		sub	immval, #4
emit_pc_immval
		andn	loc_instr, LOC_MASK
		and	immval, LOC_MASK
		or	loc_instr, immval
		mov	jit_instrptr, #loc_instr
		jmp	#emit1

condbranch
		jmp	#hub_condbranch

''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' helper routines for compilation
''''''''''''''''''''''''''''''''''''''''''''''''''''''''

mvins 	      	mov     0-0,#0
negins		neg	0-0,#0

big_temp_0
		mov	0-0, ##0-0

AUG_MASK	long	$ff800000

'
' emit a no-op (just return)
'
emit_nop
		mov	jit_instrptr, #emit_nop_pat
		jmp	#emit1

emit_nop_pat
		or	0,0	' use this for a nop so we can conditionalize it if necessary

'
' emit a mov of rs1 to rd
'
emit_mov_rd_rs1
		cmp	rd, rs1 wz
	if_z	ret	    	' nothing to do if rd is rs1 already
		sets	mov_pat,rs1
		setd	mov_pat,rd
		mov	jit_instrptr, #mov_pat
		jmp	#emit1
mov_pat		mov	0,0

CONDMASK	long	$f0000000

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
		jmp	#\hub_compile_csrw


coginit_pattern
		mov	temp, 0-0	' rs1
		setq	0-0   		' rs3
		coginit temp,0-0 wc	' rs2
		negc	0-0,temp 	' rd
		
calldebug
		call	#\debug_print
getct_pat
		getct	0-0
getcth_pat
		mov	0-0, cycleh
waitcnt_instr
		addct1 0-0, #0
		waitct1
uart_send_instr
		mov	uart_char, 0-0
		call	#\ser_tx
		or	x0, x0
uart_recv_instr
		call	#\ser_rx
		mov	0-0, uart_char
		
'=========================================================================
' custom instructions
'=========================================================================
pinsetinstr
		jmp	#\hub_pinsetinstr
wrpininstr
		jmp	#\hub_wrpininstr
rdpininstr
		jmp	#\hub_rdpininstr
coginitinstr
		jmp	#\hub_coginitinstr
singledestinstr
		jmp	#\hub_singledestinstr
'=========================================================================
		'' VARIABLES
temp		long 0
dest		long 0

rd		long	0
rs1		long	0
rs2		long	0
immval		long	0
func3		long	0
func2		long	0
opdata
divflags	long	0

jmptabptr	long	0

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

custom0tab
		and	0, illegalinstr
		and	0, illegalinstr
		'' dirl, drvl, etc. only have dest fields, so
		'' we cannot do the usual trick of putting the
		'' address in the source field;
		'' instead, we use AND and put the instruction bits
		'' in the dest field
		and	%001011000, pinsetinstr		' drvl
		and	%001010000, pinsetinstr		' fltl
		and	%001001000, pinsetinstr		' outl
		and	%001000000, pinsetinstr		' dirl
		and	0, wrpininstr
		and	0, rdpininstr
custom1tab
		and	0, coginitinstr
		and	0, singledestinstr
		and	0, illegalinstr
		and	0, illegalinstr
		and	0, illegalinstr
		and	0, illegalinstr
		and	0, illegalinstr
		and	0, illegalinstr
end_of_tables

'' utility routines for emitting 1-4 words
emit1
		mov	pb, #1
		jmp	#jit_emit
emit2
		mov	pb, #2
		jmp	#jit_emit
emit3
		mov	pb, #3
		jmp	#jit_emit
emit4
		mov	pb, #4
		jmp	#jit_emit
		
		''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
		'' code for doing compilation
		'' called from the JIT engine loop
		''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
compile_bytecode
		' fetch the actual RISC-V opcode
		rdlong	opcode, ptrb++
		test	opcode, #3 wcz
  if_z_or_c	jmp	#illegalinstr		' low bits must both be 3
  
    		'' decode instruction
		mov	immval, opcode
		sar	immval, #20
		mov	rs2, immval
		and	rs2, #$1f
		mov	rs1, opcode
		shr	rs1, #15
		and	rs1, #$1f
		mov	func3, opcode
		shr	func3,#12
		and	func3,#7
		mov	rd, opcode
		shr	rd, #7
		and	rd, #$1f

		'' now look up in table
		mov	temp, opcode
		shr	temp, #2
		and	temp, #$1f
		alts	temp, #optable
		mov	opdata, 0-0		' fetch long from table; set C if upper bit set

		testb	opdata, #31 wc
	if_nc	jmp	#getinstr
		' need to do a table indirection
		and	opdata, #$1ff		' clear upper bits
		alts	func3, opdata		' do table indirection
		mov	opdata, 0-0

getinstr
		mov	temp, opdata
		and	temp, #$1ff
		jmp	temp			' compile the instruction, return to JIT loop


#include "jit/jit_engine.spinh"

jit_instrptr	long	0
jit_instr	long	0
jit_cacheptr	long	0
jit_cachepc	long	0
jit_orig_cachepc
		long	0
jit_temp	long	0
jit_temp2	long	0
jit_condition	long	0

dis_instr	long	0

		fit	$1ec

''
'' some lesser used routines that can go in HUB memory
''
		orgh
		'' now start execution
startup
		mov	uart_str, ##@boot_msg
		call	#ser_str
		
		'' set up interrupt for CT3 == 0
		'' to measure cycle rollover
		getct	lastcnt
		and	lastcnt, chfreq
		addct3	lastcnt, chfreq
		mov   IJMP3, #ct3_isr
		setint3	#3   '' ct3

		'' run the JIT loop forever
		jmp	#jit_set_pc

#include "jit/util_serial.spin2"

#ifdef USE_DISASM
#include "jit/util_disasm.spin2"
#endif

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


emit_mvi
		cmp	immval, #0 wcz
	if_b	mov	opdata, negins
	if_b	neg	immval
	if_ae	mov	opdata, mvins
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
		mov	jit_instrptr, #big_temp_0
		'' if the augment bits are nonzero, emit augment
	if_nz	mov	pb, #2
	if_nz	jmp	#jit_emit
		'' otherwise skip the augment part
		add	jit_instrptr, #1
		jmp	#emit1

'
hub_muldiv
	alts	func3, #multab
	mov	temp, 0-0
	sets	mul_templ, rs1
	sets	mul_templ+1, rs2
	mov	mul_templ+2, temp
	setd	mul_templ+3, rd
	mov	jit_instrptr, #mul_templ
	jmp	#emit4
	
hub_condbranch		
		test	func3, #%100 wz
	if_z	mov	jit_condition, #%1010	' IF_Z
	if_nz	mov	jit_condition, #%1100	' IF_C
		test	func3, #%001 wz
	if_nz	xor	jit_condition, #$f	' flip sense
		test	func3, #%010 wz
		'' write the compare instruction
	if_z	mov	opdata,cmps_instr
	if_nz	mov	opdata, cmp_instr
		setd	opdata, rs1
		sets	opdata, rs2
		mov	jit_instrptr, #opdata
		call	#emit1

		'' now we need to calculate the new pc
		'' this means re-arranging some bits
		'' in immval
		andn 	immval, #$1f
		or	immval, rd
		test  	immval, #1 wc
		bitc	immval, #11
		andn	immval, #1
		add	immval, ptrb

		''
		'' issue a conditional branch to the value in
		'' "immval"
		'' jit_condition has the P2 flags to use for the condition
		'' ($F for unconditional branch)
		''
issue_branch_cond		
		'' BEWARE! ptrb has stepped up by 4, so we need to
		'' adjust accordingly
		sub	immval, #4

		' and go create the branch
		mov	jit_instrptr, immval
		jmp	#jit_emit_direct_branch

hub_illegalinstr
		mov	immval, ptrb
		call	#emit_pc_immval_minus_4
		mov	jit_instrptr, #imp_illegal
		mov	pb, #1
		jmp	#jit_emit

hub_compile_auipc
		mov	immval, opcode
		and	immval, LUI_MASK
		add	immval, ptrb
		sub	immval, #4
		jmp	#lui_aui_common
hub_compile_lui
		mov	immval, opcode
		and	immval, LUI_MASK
lui_aui_common
		mov	dest, rd
		jmp	#emit_mvi	'' return from there
		
hub_slt_func
		cmp	rd, #0	wz	' if rd == 0, emit nop
	if_z	jmp	#\emit_nop
	
		andn	opdata, #$1ff	' zero out source
		setd	sltfunc_pat, rd
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
		mov	jit_instrptr, #opdata
		call	#emit1
slt_fini
		mov	jit_instrptr, #sltfunc_pat
		jmp	#emit1	' return from there to our caller
		

hub_compile_csrw
		getnib	func3, immval, #2
		and	immval, #$1FF

		'' check for COG I/O e.g. 7f4
		cmp	func3, #7 wz
	if_nz	jmp	#not_cog

		'' first, emit a mov to get the old register value
		cmp	rd, #0 wz
	if_z	jmp	#skip_rd
		setd	mov_pat, rd
		sets	mov_pat, immval
		mov	jit_instrptr, #mov_pat
		call	#emit1
skip_rd
		'' now emit the real operation
		setd	opdata, immval
		sets	opdata, rs1
		jmp	#emit_opdata_and_ret
not_cog
		'' check for standard read-only regs
		cmp	func3, #$C wz
	if_nz	jmp	#not_standard

		'' write to 0? that's a no-op
	    	cmp	rd, #0 wz
	if_z	jmp	#emit_nop

		'' $c00 == mcount (cycles counter)
		cmp	immval, #0 wz
	if_nz	jmp	#not_mcount

		mov	opdata, getct_pat
  		setd	opdata, rd
		jmp	#emit_opdata_and_ret
not_mcount
		'' $c80 == cycleh (high cycle counter)
		cmp	immval, #$80 wz
	if_nz	jmp	#illegalinstr
		mov	opdata, getcth_pat
		setd	opdata, rd
		jmp	#emit_opdata_and_ret
		
		'' here's where we do our non-standard registers
		'' BC0 == UART
not_standard
		cmp	func3, #$B wz	' is it one of ours?
	if_nz	jmp	#illegalinstr
	
		cmp	immval, #$1C0 wz
	if_nz	jmp	#not_uart

		'' we can only actually read *or* write
		'' if rd is x0, then we are writing
		cmp	rd, #0 wz
	if_z	jmp	#skip_uart_read
		setd	uart_recv_instr+1, rd
		mov	jit_instrptr, #uart_recv_instr
		jmp	#emit2

skip_uart_read
		'' if rs1 is x0, skip any writes
   		cmp	rs1, #0 wz
	if_z	jmp	#emit_nop
	
		'' implement uart
  		sets	uart_send_instr, rs1
		mov	jit_instrptr, #uart_send_instr
		jmp	#emit3		' return from there to caller

not_uart
		cmp	immval, #$1C1 wz
	if_nz	jmp	#not_wait
		setd	waitcnt_instr, rs1
		mov	jit_instrptr, #waitcnt_instr
		jmp	#emit2		' return from there

not_wait
		cmp	immval, #$1C2 wz
	if_nz	jmp	#not_debug
		mov	jit_instrptr, #calldebug
		jmp	#emit1
not_debug
		jmp	#illegalinstr

		' enter with ptrb holding pc
illegal_instr_error
		mov	uart_str, ##@illegal_instruction_msg
		call	#ser_str
		mov	uart_num, ptrb
		call	#ser_hex
die
		jmp	#die

		' create a checksum of memory
		' (uart_num, info2) are checksum
		' pa = start of mem block
		' pb = end of mem block
update_checksum
		rdbyte	temp, pa	' x := word[ptr]
		add	uart_num, temp	' c0 += x
		add	info2, uart_num	' c1 += x
		add	pa, #1 		' ptr += 2
		cmp	pa, pb wz
	if_ne	jmp	#update_checksum
		ret

		alignl
reg_buf
		long	0[32]
reg_buf_end

debug_print
		mov	uart_num, #0	' c0 := 0
		mov	info2, #0	' c1 := 0
		loc	pa, #\BASE_OF_MEM	' ptr := PROGBASE
		loc	pb, #\TOP_OF_MEM
		call	#update_checksum

		' now merge in x0-x31
		loc	pa, #reg_buf
		loc	pb, #reg_buf_end
		
		setq	#31
		wrlong	x0, pa
		call	#update_checksum

		and	uart_num, ##$FFFF
		and	info2, ##$FFFF
		shl	info2, #16
		add	uart_num, info2
		call	#ser_hex

		mov	uart_char, #"@"
		call	#ser_tx
		mov	uart_num, ptrb
		call	#ser_hex

		mov	uart_str, ##@chksum_msg
		jmp	#ser_str

boot_msg
		byte	"RiscV P2 JIT (trace cache version)", 13, 10, 0
chksum_msg
		byte    "=memory chksum", 13, 10, 0
illegal_instruction_msg
		byte	"*** ERROR: illegal instruction at: ", 0
hex_buf
		byte  0[8], 0


hub_pinsetinstr
		'' RISC-V has store value in rs2
		'' adjust immediate for instruction format
		andn	immval, #$1f
		or	immval, rd
		'' isolate the precise function
		mov	func2, immval
		shr	func2, #10
		and	func2, #3
		and	immval, #$1ff wz
		'' do we have to output an immediate value?
	if_z	mov	dest, rs1	' use rs1 as the pin value
	if_z	jmp   	#.do_op
		andn	locptra, LOC_MASK
		or	locptra, immval
		sets	addptra, rs1  
		mov	jit_instrptr, #locptra
		call	#emit1
		cmp	rs1, #0 wz    		' if rs1 is x0 we do not need the add
	if_nz	mov	jit_instrptr, #addptra
	if_nz	call	#emit1
		mov	dest, #ptra	' use ptra as the pin value	
.do_op
		' now the actual pin instruction
		' rs2 contains the value to write to the pin
		' dest contains the pin number
		' opdata contains a template like and yy, xx
		shr	 opdata, #9 	     ' get instruction pattern into the src bits
		and	 opdata, #$1ff
		or	 opdata, dirinstr    ' set bits for dir
		setd	 opdata, dest	     ' set pin to affect
		
		'' depending on func2 we have some different operations
		'' 00 = store value 01 == store !value
		'' 10 = store rnd   11 == invert existing value
		'' low bit goes directly into instruction
		test   func2, #%01 wc
		muxc   opdata, #%01
		'' check for using value; if we do use it then we may need to do a bit test
		test	func2, #%10 wc     ' do we use the value
	if_c	or	opdata, #%110	    ' if not, emit dirrnd/dirnot
	if_c	jmp	#emit_opdata_and_ret
		cmp	rs2, #0 wz	     ' is the value known to be 0?
	if_z	jmp	#emit_opdata_and_ret

		'' if the value isn't known, emit a test #1 wc to get the value into c
		setd	testbit_instr, rs2	' test the bit in the rs2 register
		mov	jit_instrptr, #testbit_instr
		call	#emit1
		or	opdata, #%10		' emit dirc/dirnc
		jmp	#emit_opdata_and_ret

hub_wrpininstr
		'' RISC-V has store value in rs2
		'' adjust immediate for instruction format
		andn	immval, #$1f
		or	immval, rd
		'' upper 2 bits of immval control function
		mov	func2, immval
		shr	func2, #10
		and	func2, #3
		alts	func2, #wrpin_table
		mov	opdata, 0-0
		test	opdata, ##$3ffff wz	' if it's a jmp #illegalinstr
	if_nz	jmp	#emit_opdata_and_ret
		and	immval, #$1ff wz
	if_z	mov	dest, rs1     ' use rs1 as the pin value directly
	if_z	jmp	#.skip_imm
		andn	locptra, LOC_MASK
		or	locptra, immval
		sets	addptra, rs1
		mov	dest, #ptra		' use ptra as the pin value
.skip_imm
		setd	opdata, rs2
		sets	opdata, dest
		jmp	#emit_opdata_and_ret

		''
		'' read pin data instructions
		''
hub_rdpininstr
		'' upper 2 bits of immval control function
		mov	func2, immval
		shr	func2, #10
		and	func2, #3
		alts	func2, #rdpin_table
		mov	opdata, 0-0
		and	immval, #$1ff wz
		'' check for nonzero pin offset
	if_z	jmp	#.skip_imm
		andn	locptra, LOC_MASK
		or	locptra, immval
		sets	addptra, rs1
		mov	rs1, #ptra		' use ptra as the pin value
.skip_imm
		' never write to x0
		cmp	rd, #0 wz
	if_z	mov	rd, #temp
		' for func2 == 0 we need the testp instruction
		cmp	func2, #0 wz
	if_nz	jmp	#.skip_testp
		setd	testpin_instr, rs1
		mov	jit_instrptr, #testpin_instr
		call	#emit1
.skip_testp
		test	opdata, #$1ff wz
	if_z	sets	opdata, rs1
		test	opdata, ##($1ff<<9) wz	' check dest field
	if_z	setd	opdata, rd
		jmp	#emit_opdata_and_ret
		
hub_coginitinstr
#ifdef FIXME
		mov	uart_num, immval
		call	#ser_hex
		mov	uart_num, rs2
		call	#ser_hex
		mov	uart_num, rs1
		call	#ser_hex
		mov	uart_num, rd
		call	#ser_hex
		call	#ser_nl
#endif
		shr	immval, #5	' skip over rs2
		mov	func2, immval
		and	func2, #3 wz
	if_nz	jmp	#illegalinstr

		' immval is actually rs3, which will go into setq
		shr	immval, #2

		' if rd is 0, then use "temp" instead
		cmp	rd, #0 wz
	if_z	mov	rd, #temp

		sets	coginit_pattern, rs1
		setd	coginit_pattern+1, immval
		sets	coginit_pattern+2, rs2
		setd	coginit_pattern+3, rd
		
		mov	jit_instrptr, #coginit_pattern
		mov	pb, #4
		jmp	#jit_emit

hub_singledestinstr
		jmp	#illegalinstr
		
		orgh	$4000
		