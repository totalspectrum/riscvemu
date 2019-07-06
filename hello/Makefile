#
# Makefile for linking with newlib
# This is a test harness for trying to make the whole
# linking experience much more seamless, as well as a
# proof of concept that newlib can be used in P2
# development.
#
TOOLROOT=/opt/riscv/bin/riscv32-unknown-elf-
FASTSPIN=/home/ersmith/Parallax/spin2cpp/build/fastspin

CC=$(TOOLROOT)gcc
CFLAGS=-Os -march=rv32im
LD=$(TOOLROOT)ld
LINKFILE=./rvemu_p2.ld

PROGNAME=hello
EMULATOR=p2trace.bin
EMUOBJ=_rvp2.o

default: $(PROGNAME).binary

LIBOBJS=write.o sbrk.o
OBJS=$(PROGNAME).o $(LIBOBJS)

$(PROGNAME).binary: $(PROGNAME).elf
	$(TOOLROOT)objdump -M numeric,no-aliases -d $(PROGNAME).elf > $(PROGNAME).lst
	$(TOOLROOT)objcopy -O binary $(PROGNAME).elf $(PROGNAME).binary


$(PROGNAME).elf: $(OBJS) $(EMUOBJ) $(LINKFILE)
	$(CC) -Wl,--print-map -Wl,-T -Wl,$(LINKFILE) -o $(PROGNAME).elf $(OBJS) -lc -lgcc > $(PROGNAME).map

_rvp2.o: _rvp2.s $(EMULATOR)
	$(CC) -o _rvp2.o -c _rvp2.s

STUBSRC=../debug.spin ../riscvemu.spin ../riscvemu_p2.spin ../riscvtrace_p2.spin ../jit/jit_engine.spinh

# HDRSIZE=8192
HDRSIZE=16384

p2emuheader.bin: $(STUBSRC)
	$(FASTSPIN) -2 -o p2emuheader.bin ../debug.spin
	dd if=/dev/null of=p2emuheader.bin bs=1 count=1 seek=$(HDRSIZE)

p2trace.bin: $(STUBSRC)
	$(FASTSPIN) -2  -o p2trace.bin ../riscvtrace_p2.spin
#	dd if=/dev/null of=p2trace.bin bs=1 count=1 seek=$(HDRSIZE)

clean:
	rm -f *~ *.o *.elf *.lst *.binary *.bin *.map *.p2asm *.pasm
