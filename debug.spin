{{
   RISC-V Emulator test harness
   Copyright 2017 Total Spectrum Software Inc.
   Terms of use: MIT License (see the file LICENSE.txt)
}} 

CON
  _clkfreq = 80_000_000
  _clkmode = xtal1 + pll16x
   
OBJ
#ifdef __P2__
  ser: "SimpleSerial"
  proc: "riscvemu_p2"
#else
  ser: "FullDuplexSerial"
  proc: "riscvemu"
#endif

CON
  memsize = 16*1024		' size of RAM given to the RISC-V chip
  
VAR
  ' regs are 32 general purpose registers, followed by pc and debug
  long regs[36]
  ' parameter layout
  ' 0 = command register address
  ' 1 = base of memory
  ' 2 = size of memory
  ' 3 = initial pc
  ' 4 = debug register address
  '
  long params[5]
  long cmdreg

DAT

dummy
	long 1	'' force long alignment
progmem
	file	"test.bin"
padding
	byte	0[memsize - (@padding - @progmem)]
	
PUB demo | cmd, arg
  ser.start(31, 30, 0, 115200)
  ser.str(string("Processor emulation", 13, 10))
  ser.str(string("starting emulation; base="))
  ser.hex(@progmem, 8)
  params[0] := @cmdreg
  params[1] := @progmem ' base of memory
  params[2] := memsize
  params[3] := 0	' initial PC (relative to base)
  params[4] := @regs	' debug address
  proc.start(@params)
  ser.str(string(" ok"))
  nl
  repeat
    repeat
    while cmdreg == 0
    cmd := cmdreg & $f
    arg := cmdreg >> 4
    if (cmd == 1)	' single step
      dumpregs
      waitforkey
    elseif (cmd == 2)	' illegal instruction
      ser.str(string("*** illegal instruction ***", 13, 10))
      dumpregs
      waitforkey
    elseif (cmd == $f) ' write a byte
      ser.tx(arg)
    cmdreg := 0

PRI nl
  ser.tx(13)
''  ser.tx(10)

'' print 8 registers starting at n
PRI printregs(msg, n) | j
  ser.str(msg)
  repeat j from 0 to 7
    ser.tx(" ")
    ser.hex(regs[n+j], 8)
  nl
  
PRI dumpregs | i,j
  printregs(string(" x0:"), 0)
  printregs(string(" x8:"), 8)
  printregs(string("x16:"), 16)
  printregs(string("x24:"), 24)
  ser.str(string("pc="))
  ser.hex(regs[32], 8)
  ser.str(string(" dbg="))
  ser.hex(regs[33], 8)
  ser.str(string(" info1="))
  ser.hex(regs[34], 8)
  ser.str(string(" info2="))
  ser.hex(regs[35], 8)
  nl

PRI waitforkey | c
   ser.str(string("*** press a key to continue ***"))
   c := ser.rx
   nl

