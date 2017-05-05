TOOLS=/opt/riscv/bin/
AS=$(TOOLS)riscv32-unknown-elf-as
ASFLAGS=-march=rv32im
LIB=../lib/
LD=$(TOOLS)riscv32-unknown-elf-ld

CC=$(TOOLS)riscv32-unknown-elf-gcc
CFLAGS=-Os -march=rv32im

PROG=fftbench
PROGOBJS=$(PROG).o
LIBOBJS=$(LIB)getcnt.o $(LIB)putbyte.o $(LIB)simple_printf.o
START=$(LIB)start.o

LINKFILE=$(LIB)flat.ld
SPIN2CPP=/home/ersmith/Parallax/spin2cpp/build/spin2cpp

STUBSRC=../debug.spin ../riscvemu.spin ../riscvemu_p2.spin ../riscvjit_p2.spin

OBJS=$(START) $(PROGOBJS) $(LIBOBJS)

all: p1.binary p2.binary

$(PROG).bin: $(OBJS)
	$(TOOLS)riscv32-unknown-elf-ld -T $(LINKFILE) -o $(PROG).elf $(OBJS)
	$(TOOLS)riscv32-unknown-elf-objdump -M numeric,no-aliases -d $(PROG).elf > $(PROG).lst
	$(TOOLS)riscv32-unknown-elf-objcopy -O binary $(PROG).elf $(PROG).bin

p1.binary: $(PROG).bin p1header.bin
	cat p1header.bin $(PROG).bin > p1.binary
	../tools/p1_chksum p1.binary

p2.binary: $(PROG).bin p2header.bin
	cat p2header.bin $(PROG).bin > p2.binary

clean:
	rm -rf *.o *.elf *.lst *.bin *.binary

p1header.bin: $(STUBSRC)
	$(SPIN2CPP) --code=hub --data=hub --asm --binary -o p1header.bin ../debug.spin
	dd if=/dev/null of=p1header.bin bs=1 count=1 seek=8192

p2header.bin: $(STUBSRC)
	$(SPIN2CPP) --p2 --code=hub --data=hub --asm --binary -o p2header.bin ../debug.spin
	dd if=/dev/null of=p2header.bin bs=1 count=1 seek=8192
