TOOLS=/opt/riscv/bin/
AS=$(TOOLS)riscv32-unknown-elf-as
ASFLAGS=-march=rv32im
LIB=../lib/
LD=$(TOOLS)riscv32-unknown-elf-ld

CC=$(TOOLS)riscv32-unknown-elf-gcc
CFLAGS=-Os -march=rv32im

PROG=fibo
PROGOBJS=$(PROG).o
LIBOBJS=$(LIB)getcnt.o $(LIB)putbyte.o $(LIB)simple_printf.o
START=$(LIB)start.o
HDR=header.bin

LINKFILE=$(LIB)flat.ld
SPIN2CPP=/home/ersmith/Parallax/spin2cpp/build/spin2cpp

OBJS=$(START) $(PROGOBJS) $(LIBOBJS)

$(PROG).binary: $(OBJS) $(HDR)
	$(TOOLS)riscv32-unknown-elf-ld -T $(LINKFILE) -o $(PROG).elf $(OBJS)
	$(TOOLS)riscv32-unknown-elf-objdump -M numeric,no-aliases -d $(PROG).elf > $(PROG).lst
	$(TOOLS)riscv32-unknown-elf-objcopy -O binary $(PROG).elf tmp.bin
	cat $(HDR) tmp.bin > $(PROG).binary
	rm -f tmp.bin

clean:
	rm -rf *.o *.elf *.lst *.bin *.binary

STUBSRC=../debug.spin ../riscvemu.spin ../riscvemu_p2.spin

header.bin: $(STUBSRC)
	$(SPIN2CPP) --p2 --code=hub --data=hub --asm --binary -o header.bin ../debug.spin
	dd if=/dev/null of=header.bin bs=1 count=1 seek=8192
