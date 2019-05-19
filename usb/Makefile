OPT=-Os

PROG=kbtest
PROGOBJS=kbtest.o OneCogKbM.o
LIBOBJS=$(LIB)putbyte.o $(LIB)simple_printf.o $(LIB)memset.o $(LIB)memcpy.o

include ../lib/Makefile.incl

OneCogKbM.c: OneCogKbM.spin2
	/home/ersmith/Parallax/spin2cpp/build/spin2cpp --p2 --ccode OneCogKbM.spin2
	sed -i 's@<propeller.h>@"../lib/riscv.h"@g' OneCogKbM.c
