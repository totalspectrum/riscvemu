{{
   RISC-V Emulator test harness
   Copyright 2017 Total Spectrum Software Inc.
   Terms of use: MIT License (see the file LICENSE.txt)
}} 

CON
#ifdef __P2__
  _clkfreq = 160_000_000
  _clkmode = $010007f8
#else  
  _clkfreq = 80_000_000
  _clkmode = xtal1 + pll16x
#endif  
  PROGBASE = $2000
  BUFSIZ = 80
  
OBJ
#ifdef __P2__
  ser: "SimpleSerial.spin"
  proc: "riscvemu_p2.spin"
#else
  ser: "FullDuplexSerial.spin"
  proc: "riscvemu.spin"
#endif

CON
'  memsize = 16*1024		' size of RAM given to the RISC-V chip
 memsize = 24*1024
 
VAR
  ' regs are 32 general purpose registers, followed by pc and debug
  ' debug info is:
  ' 32 = pc
  ' 33 = opcode
  ' 34 = info1
  ' 35 = info2
  ' 36 = info3
  ' 37 = info4
  ' 38 = reserved
  ' 39 = step count
  
  long regs[40]
  
  ' parameter layout
  ' 0 = command register address
  ' 1 = base of memory
  ' 2 = size of memory
  ' 3 = initial pc
  ' 4 = debug register address
  '
  long params[5]
  long cmdreg

  ' scratch buffer
  byte buf[BUFSIZ]

PUB demo | cmd, arg, c, x
#ifdef __P2__
  clkset(_clkmode, _clkfreq)
  ser.start(63, 62, 0, 230400) ' wrong, but works :(
#else  
  ser.start(31, 30, 0, 115200)
#endif  
  ser.str(string("Processor emulation", 13, 10))
  ser.str(string("starting emulation; base="))
  ser.hex(PROGBASE, 8)
  params[0] := @cmdreg
  params[1] := PROGBASE ' base of memory
  params[2] := memsize
  params[3] := PROGBASE	' initial PC
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
      ser.str(string("*** step ***", 13, 10))
      dumpregs
      c := waitforkey
      if (c == "b")
        nl
	ser.str(string("Number of steps: "))
	x := readNum
	nl
	ser.str(string("Stepping 0x"))
	ser.hex(x, 8)
	ser.str(string(" times"))
	nl
	if (x == 0)
	  x := 1
        regs[39] := x '' big step
      elseif (c == "c")
        regs[39] := 0   ' continue, no stepping
      else
        regs[39] := 1
    elseif (cmd == 2)	' illegal instruction
      ser.str(string("*** illegal instruction ***", 13, 10))
      dumpregs
      regs[38] := 0
      regs[39] := 1
      waitforkey
    elseif (cmd == $f) ' write a byte
        ser.tx(arg)
    cmdreg := 0

PUB pause
  waitcnt(CNT + 20_000_000)
  
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
  ser.str(string(" info: "))
  ser.hex(regs[34], 8)
  ser.str(string(" "))
  ser.hex(regs[35], 8)
  ser.str(string(" "))
  ser.hex(regs[36], 8)
  ser.str(string(" "))
  ser.hex(regs[37], 8)
  ser.str(string(" step "))
  ser.hex(regs[39], 8)
  nl

PRI waitforkey | c
   ser.str(string("*** press a key to continue ***"))
   c := ser.rx
   nl
   return c

PUB readNum | c,x,i,scale
  x := 0
  i := 0
  repeat while (i < BUFSIZ)
    c := ser.rx
    ser.tx(c)
    if (c == 10 OR c == 13)
        quit
    elseif (c == 8 OR c == 127)
        ser.tx(" ")
	ser.tx(c)
	if (i > 0)
	  --i
    else
        buf[i++] := c
  nl
  scale := 1
  repeat while (i > 0)
    c := buf[--i]
    if (c => "0" AND c =< "9")
      x := x + scale*(c-"0")
    elseif (c => "a" AND c =< "f")
      x := x + scale*(10+c-"a")
    else
      ser.str(string("Bad character: "))
      ser.tx(c)
      nl
    scale := scale * 16

  return x
  