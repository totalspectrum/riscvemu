COUNT ?= 50000

CDEFS=-DGETMS -DCOUNT=$(COUNT)
#CDEFS=-DGETMS -DCOUNT=50000
#OPT=-O3
OPT=-Os

PROG=dhry
PROGOBJS=dhry_1.o dhry_2.o dummymalloc.o getms.o
LIBOBJS=$(LIB)getcnt.o $(LIB)putbyte.o $(LIB)simple_printf.o $(LIB)strcpy.o $(LIB)memcpy.o $(LIB)strcmp.o

include ../lib/Makefile.incl
