TOOLROOT=/opt/riscv/bin/riscv32-unknown-elf-
FASTSPIN=/home/ersmith/Parallax/spin2cpp/build/fastspin

CC=$(TOOLROOT)gcc
CFLAGS=-Os -march=rv32im
LD=$(TOOLROOT)ld
LINKFILE=./rvemu_p2.ld

PROGNAME=hello

default: p2emu.binary p2.binary

default: $(PROGNAME).bin

LIBOBJS=write.o sbrk.o
OBJS=$(PROGNAME).o $(LIBOBJS)

$(PROGNAME).elf: $(OBJS)
	$(CC) -Wl,--print-map -Wl,-T -Wl,$(LINKFILE) -o $(PROGNAME).elf $(OBJS) -lc -lgcc > $(PROGNAME).map

$(PROGNAME).bin: $(PROGNAME).elf
	$(TOOLROOT)objdump -M numeric,no-aliases -d $(PROGNAME).elf > $(PROGNAME).lst
	$(TOOLROOT)objcopy -O binary $(PROGNAME).elf $(PROGNAME).bin


p2.binary: p2header.bin $(PROGNAME).bin
	cat p2header.bin $(PROGNAME).bin > p2.binary

p2emu.binary: p2emuheader.bin $(PROGNAME).bin
	cat p2emuheader.bin $(PROGNAME).bin > p2emu.binary


STUBSRC=../debug.spin ../riscvemu.spin ../riscvemu_p2.spin ../riscvjit_p2.spin

p1header.bin: $(STUBSRC)
	$(FASTSPIN) -o p1header.bin ../debug.spin
	dd if=/dev/null of=p1header.bin bs=1 count=1 seek=8192

p2emuheader.bin: $(STUBSRC)
	$(FASTSPIN) -2 -o p2emuheader.bin ../debug.spin
	dd if=/dev/null of=p2emuheader.bin bs=1 count=1 seek=8192

p2header.bin: $(STUBSRC)
	$(FASTSPIN) -2  -o p2header.bin ../riscvjit_p2.spin
	dd if=/dev/null of=p2header.bin bs=1 count=1 seek=8192

clean:
	rm -f *~ *.o *.elf *.lst *.binary *.bin *.map *.p2asm *.pasm
