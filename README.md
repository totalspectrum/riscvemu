RISC-V Emulators for Parallax Propeller and Propeller 2
Copyright 2017-2019 Total Spectrum Software Inc.
Terms of use: MIT License (see the file LICENSE.txt)

An emulator for the RISC-V processor architecture, designed to run
in a single Propeller COG. There's also a version for the Propeller 2.

The instruction set emulated is RV32IM, mostly, except that the 64 bit
multiplies are not yet implemented. The only standard CSR implemented is
the low 32 bits of the cycle counter (so rdcycle works, but rdcycleh does
not). Some non-standard CSRs are used to access a UART emulation.

riscvemu.spin is the actual emulator (the guts are in PASM, of course)
riscvemu_p2.spin is a P2 version of the emulator
riscvjit_p2.spin is an advanced P2 version that caches the results of
    compilation.
debug.spin is a top level test harness

Generally we build debug.spin, pad it out to an 8K boundary, and then
postpend a RISC-V binary. On the Propeller 1 we then have to fix up
the binary checksum; this is done with the p1_chksum tool found in
the tools/ directory. See for example the Makefiles
in the fftbench/, fibo/, and xxtea/ subdirectories.

The RISC-V binary should be linked to start at address 8192 (0x2000).
See the Makefiles for examples of how to do this.

To build, I suggest using the Makefiles in the various subdirectories.
These produce two binaries, p1.binary and p2.binary, which are the
downloadables for Propeller1 and Propeller2 respectively. You can use
propeller-load or the Propeller Tool to load the p1.binary. On
P2 I use Dave Hein's loadp2 program.

The interface is pretty simple: the params array passed to the start
method should contain:
```   
   params[0] = address of command register
   params[1] = base of emulated memory
   params[2] = size of emulated memory (stack will start at base + size and grow down)
   params[3] = initial pc (usually 0)
   params[4] = address for register dump (40 longs; x0-x31, pc, opcode, dbg1, dbg2, dbg3, dbg4, reserved, stepcount)
```

The register dump should have 40 longs. The first 32 are the RISC-V general
purpose registers x0-x31. The others are:
```
  reg[32]: program counter
  reg[33]: current opcode
  reg[34]: debug info1
  reg[35]: debug info2
  reg[36]: debug info3
  reg[37]: debug info4
  reg[38]: reserved
  reg[39]: stepcount (for single stepping; 0 = run continuously)
```

The command register is used for communication from the RISC-V back to the host.
The lowest 4 bits contain a command, as follows:
```
   $1 = single step (registers have been dumped)
   $2 = illegal instruction encountered (registers have been dumped)
   $F = request to write character in bits 12-8 of command register
```

The host should write 0 to the command register as an "ACK" to restart
the RISC-V. When it does so, the registers will be re-read. If the
`stepcount` field is non-zero then the emulated processor will step
this many times before stopping again. If it is 0 it will run continuously.
   
---------------------------------------------------------------------
Emulated CSRs:
```
  C00 - cycle counter
  BC0 - uart register; bytes written here go to the serial port
        you can also read from it to read a byte (it returns -1 if
	no byte is available yet)
  BC1 - wait register; if a value is written here we wait until the
        cycle counter matches it
  7Fx - COG registers 1F0 - 1FF
```
----------------------------------------------------------------------
Custom instructions

we use the custom0 opcode space:

instructions:

We add new instructions to the CUSTOM_0 and CUSTOM_1 name spaces

CUSTOM_0 is used for s or sb format (2 registers, 1 imm) (like many others in 00-07)
CUSTOM_1 is used for r format (3 registers)

CUSTOM_0, 0 and CUSTOM_0, 1 are reserved

drv val, offset_mode(pin)
   .insn sb  CUSTOM_0, 2, val, offset_mode(pin)
   offset_mode is %zz_xxxx_pppppp, where:
        pppppp is offset added to pin
	xxxx is reserved (used for selecting groups of pins in future?)
	zz
           zz = 00 => store val to pin (use val=x0 for drvl)     
		01 => store !val to pin (use val=x0 for drvh)
	        10 => store random data to pin (drvrnd etc.)
		11 => invert pin (drvnot etc.)
		
flt val, offset_mode(pin)
   .insn sb  CUSTOM_0, 3, val, offset_mode(pin)
   offset_mode is %zz_xxxx_pppppp, where:
        pppppp is offset added to pin
	xxxx is reserved (used for selecting groups of pins in future?)
	zz
           zz = 00 => store val to pin (use val=x0 for fltl)     
		01 => store !val to pin (use val=x0 for flth)
	        10 => store random data to pin (fltrnd)
		11 => invert pin (fltnot)
		
out val, offset_mode(pin)
   .insn sb  CUSTOM_0, 4, val, offset_mode(pin)
   offset_mode is %zz_xxxx_pppppp, where:
        pppppp is offset added to pin
	xxxx is reserved (used for selecting groups of pins in future?)
	zz
           zz = 00 => store val to pin (use val=x0 for outl)     
		01 => store !val to pin (use val=x0 for outh)
	        10 => store random data to pin (outrnd)
		11 => invert pin (outnot)
		
dir val, offset_mode(pin)
   .insn sb  CUSTOM_0, 5, val, offset_mode(pin)
   offset_mode is %zz_xxxx_pppppp, where:
        pppppp is offset added to pin
	xxxx is reserved (used for selecting groups of pins in future?)
	zz
           zz = 00 => store val to pin (use val=x0 for dirl)     
		01 => store !val to pin (use val=x0 for dirh)
	        10 => store random data to pin (dirrnd)
		11 => invert pin (dirnot)
		
wrpin  mode, offset_mode(pin)
   .insn sb CUSTOM_0, 6, mode, offset(pin)
   does wrpin/wxpin/wypin mode, pin+offset; leaves mode unchanged
   offset_mode is %zz_xxxx_pppppp where:
       pppppp is offset to add to pin
       xxxx is reserved (set to 0)
       zz is: 00 for wrpin, 01 for wxpin, 10 for wypin, 11 is reserved
       
getpin res, offset_mode(pin)
   .insn s CUSTOM_0, 7, res, offset_mode(pin)
   gets value of pin at pin+offset into res
   offset_mode is %zz_xxxx_pppppp where:
       pppppp is offset to add to pin
       xxxx is reserved (set to 0)
       zz is:
         00: read pin value (0 or 1) into res
	 01: do rdpin into res
	 10: do rqpin into res
	 11: do akpin

   
CUSTOM_1:

'' 4 operand
coginit res, dval, addr, param
   .insn r CUSTOM_1, 0, 0, res, dval, addr, param
   addr is address of COG code to run, or 0 for RISC-V kernel
   param is parameter to pass (stack pointer for RISC-V kernel)
   dval is the D register value for coginit


 ==========================================
 STREAMER:
 setdacs d0
 setxfrq d0
 xinit d, s
 xzero d, s
 xcont d, s
 getxacc result1, result2
 rdfast count, base
 rflong res
 rfword res
 rfbyte res
 