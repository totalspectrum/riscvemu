CDEFS=-DGETMS -DCOUNT=32768
#OPT=-O3
OPT=-Os

PROG=dhry
PROGOBJS=dhry_1.o dhry_2.o dummymalloc.o
LIBOBJS=$(LIB)getcnt.o $(LIB)putbyte.o $(LIB)simple_printf.o $(LIB)strcpy.o $(LIB)memcpy.o $(LIB)getms.o $(LIB)strcmp.o

include ../lib/Makefile.incl
