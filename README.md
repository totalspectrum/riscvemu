RISC-V Emulator for Parallax Propeller
Copyright 2017 Total Spectrum Software Inc.
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
  BC1 - wait register; if a value is written here we wait until the
        cycle counter matches it
  7Fx - COG registers 1F0 - 1FF
```