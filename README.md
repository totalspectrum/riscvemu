# RISC-V Emulators for Parallax Propeller and Propeller 2

Copyright 2017-2019 Total Spectrum Software Inc.
Terms of use: MIT License (see the file LICENSE.txt)

## Overview

An emulator for the RISC-V processor architecture, designed to run in
a single Propeller COG. There's also a version for the Propeller 2.
In fact the P2 version is the more up to date, and this document
mostly discusses that.

The instruction set emulated is RV32IM, mostly, except that some 64
bit multiplies are not yet implemented. The only standard CSR
implemented on P1 is the low 32 bits of the cycle counter (so rdcycle
works, but rdcycleh does not). On P2 the full 64 bit cycle counter is
available.

Some non-standard CSRs are used to access a UART emulation and to directly access
propeller registers. See below for details.

### Directories and Files

```
coginit - demo of using COGINIT on P2
dhry - dhrystone benchmark
fftbench - FFT benchmark
fibo - recursive fibonacci
hello - hello world using Newlib library, see Readme.txt
toggle - LED toggle code
vga - P2 VGA demo
xxtea - xxtea benchmark

riscvemu.spin is the P1 emulator (the guts are in PASM, of course)
riscvemu_p2.spin is a P2 version of the emulator
riscvjit_p2.spin is an advanced P2 version that compiles to P2 code
debug.spin is a top level test harness for the emulators
```

## Usage

You'll need a RISC-V toolchain; I used the standard GCC toolchain from
https://github.com/riscv/riscv-gnu-toolchain. Make sure to build for
the rv32im architecture (many embedded toolchains build for rv32imc,
but riscvemu does not support compressed instructions yet).

The RISC-V binary should be linked to start at address 8192 (0x2000).
See the Makefiles for examples of how to do this.

To build, I suggest using the Makefiles in the various subdirectories.
These produce three binaries, p1.binary, p2.binary, and p2emu.binary.
p1.binary is the P1 emulator. p2.binary is the P2 JIT version, and
p2emu.binary is the P2 emulator version.

## P2 Support

See lib/riscv.h for some useful utility macros.

### Emulated CSRs
```
  C00 - cycle counter (low 32 bits)
  C80 - cycle counter (high 32 bits) (P2 only)
  BC0 - uart register; bytes written here go to the serial port
        you can also read from it to read a byte (it returns -1 if
	no byte is available yet)
  BC1 - wait register; if a value is written here we wait until the
        cycle counter matches it
  BC2 - debug register, used for internal debug purposes
  7Fx - COG registers 1F0 - 1FF
```

### Custom instructions

On the P2, we add new instructions to the CUSTOM_0 and CUSTOM_1 opcode spaces (these are P2 only,
the P1 emulator does not have them).  See lib/riscv.h for some macros for using these.

CUSTOM_0 is used for pin manipulation instructions
   
CUSTOM_1 is used for COGINIT and other miscellaneous instructions

#### CUSTOM_0: Pin instructions

CUSTOM_0, 0 and CUSTOM_0, 1 are reserved
```
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

  example: the P2 instruction
     drvh  #2
  is achieved via the RISC-V instruction
     .insn sb CUSTOM_0, 2, x0, 0x402(x0)
  to set pin 56 to the value in register x10, do:
     .insn sb CUSTOM_0, 2, x10, 56(x0)
     
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
```
   
#### CUSTOM_1: Misc control instructions

##### 4 operand instructions

```
coginit res, dval, addr, param
   .insn r CUSTOM_1, 0, 0, res, dval, addr, param
   addr is address of COG code to run, or 0 for RISC-V kernel
   param is parameter to pass (stack pointer for RISC-V kernel)
   dval is the D register value for coginit

Other values for func2 are reserved.
```

##### Direct immediate instructions

These look like:
```
.insn i CUSTOM_1, 1, res, imm(dval)
   generates P2 opcode 1101010 with D=res (initialized to dval) and S=imm
   That is, the output register is "res", input register is "dval", and
   "imm" selects the actual opcode to use. Note that in P2 the output and
   input will always be the same for these instructions (the instruction
   field uses D for both) unless you use ALTR.
   
   for example to get current COG id into a0
     .insn i CUSTOM_1, 1, a0, 0x001(a0)
   (this translates to a P2 COGID instruction)
   to stop a COG whose id is in a0 (P2 COGSTOP):
     .insn i CUSTOM_1, 1, a0, 0x03(a0)
```

This allows access to the whole block of P2 instructions from HUBSET
to MODZ, although not all of these will be useful.  If the high bit of
the immediate is set, the WC flag is set on the generated
instruction. If the second highest bit of the immediate is also set,
then the final destination is written as -1 if C is set after the
instruction executes. This is useful for generating LOCKTRY and similar
instructions.

## Spin interfaces

### JIT interface

Not much of an interface here; the runtime code initializes the UART
and then just jumps directly to the RISC-V interpreter. In theory
riscvjit_p2.spin could be built with any P2 assembler, but you may
have to preprocess it first.

### Emulator interface

The emulator can actually act as a Spin object.  The interface to
start up the emulator is pretty simple: the params array passed to the
start method should contain:

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

The JIT compiler is much simpler, and does not have the debug interface. It's
a pure PASM program and hence only requires one COG (the other emulators use
two COGs, one for the debug stub and one for the Risc-V emulation).




