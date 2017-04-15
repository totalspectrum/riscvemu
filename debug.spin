''
'' serial port demo
''
CON
  _clkfreq = 80_000_000
  _clkmode = xtal1 + pll16x
   
OBJ
  ser: "FullDuplexSerial"
  proc: "riscvemu"
  
VAR
  ' regs are 32 general purpose registers, followed by pc and debug
  long regs[34]
  ' parameter layout
  ' 0 = command register address
  ' 1 = base of memory
  ' 2 = initial pc
  ' 3 = debug register address
  '
  long params[4]
  long cmdreg

DAT

progmem
	long	1
	long	2
	long	3
	long	4
	
PUB demo
  ser.start(31, 30, 0, 115200)
  ser.str(string("Processor emulation", 13, 10))
  ser.str(string("starting emulation..."))
  params[0] := @cmdreg
  params[1] := @progmem ' base of memory
  params[2] := 0	' initial PC (relative to base)
  params[3] := @regs	' debug address
  proc.start(@params)
  ser.str(string("ok"))
  nl
  repeat
    repeat
    while cmdreg == 0
    if (cmdreg == 1)
      dumpregs
      waitforkey
    cmdreg := 0

PRI nl
  ser.tx(13)
  ser.tx(10)

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
  nl

PRI waitforkey | c
   ser.str(string("*** press a key to continue ***"))
   c := ser.rx
   ser.tx(13)

