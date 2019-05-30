OPT=-Os
SPIN2CPP=/home/ersmith/Parallax/spin2cpp/build/spin2cpp

PROG=kbtest
PROGOBJS=kbtest.o OneCogKbM.o BufferSerial.o
LIBOBJS=$(LIB)putbyte.o $(LIB)simple_printf.o $(LIB)memset.o $(LIB)memcpy.o

include ../lib/Makefile.incl

OneCogKbM.c: OneCogKbM.spin2
	$(SPIN2CPP) --p2 --ccode OneCogKbM.spin2
	sed -i 's@<propeller.h>@"../lib/riscv.h"@g' OneCogKbM.c

BufferSerial.c: BufferSerial.spin2
	$(SPIN2CPP) --p2 --ccode BufferSerial.spin2
	sed -i 's@<propeller.h>@"../lib/riscv.h"@g' BufferSerial.c
