RISC-V Emulator for Parallax Propeller
Copyright 2017 Total Spectrum Software Inc.
Terms of use: MIT License (see the file LICENSE.txt)

An emulator for the RISC-V processor architecture, designed to run
in a single Propeller COG. There's also a version for the Propeller 2.

The instruction set emulated is RV32IM, mostly, except that the 64 bit
multiplies are not yet implemented. The only CSR implemented is
the low 32 bits of the cycle counter (so rdcycle works, but rdcycleh does
not).

riscvemu.spin is the actual emulator (the guts are in PASM, of course)
riscvemu_p2.spin is the P2 version of the emulator
debug.spin is a top level test harness

debug.spin includes test.bin, which should be the actual test program.
This may be built with regular riscv tools; see for example the Makefiles
in the test/, fibo/, and xxtea/ subdirectories.

To build, use openspin or fastspin for the Propeller1, and fastspin -2 for
the Propeller 2. You'll need fastspin version 3.6.2 or later for P2
support; earlier versions had some bugs that cause the emulator not to
work properly. My workflow on P2 looks something like this:

   # recompile fibo
   cd fibo
   make
   cp fibo.bin ../test.bin
   cd ..
   fastspin -2 debug.spin
   loadp2 /dev/ttyUSB0 debug.binary -t

The interface is pretty simple: the params array passed to the start
method should contain:
   
   params[0] = address of command register
   params[1] = base of emulated memory
   params[2] = size of emulated memory (stack will start at base + size and grow down)
   params[3] = initial pc (usually 0)
   params[4] = address for register dump (38 longs; x0-x31, pc, opcode, dbg1, dbg2, stepcount, reserved)

The command register is used for communication from the RISC-V back to the host.
The lowest 4 bits contain a command, as follows:
   $1 = single step (registers have been dumped)
   $2 = illegal instruction encountered (registers have been dumped)
   $F = request to write character in bits 12-8 of command register

The host should write 0 to the command register as an "ACK" to restart
the RISC-V. When it does so, the registers will be re-read. If the
"stepcount" field is non-zero then the emulated processor will step
this many times before stopping again.
   
---------------------------------------------------------------------
Emulated memory map:
  0000_0000 - 0000_xxxx: RAM (0 is membase)
  F000_0000 - FFFF_FFFF: I/O space:
     F000_0000 = command register; writes here go to the host command register
     F000_0004 - F000_07FC = COG memory space (useful for accessing COG registers like CNT and OUTA)
