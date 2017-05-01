TOOLS=/opt/riscv/bin/
AS=$(TOOLS)riscv32-unknown-elf-as
ASFLAGS=-fpic -march=rv32im
LD=$(TOOLS)riscv32-unknown-elf-ld

CC=$(TOOLS)riscv32-unknown-elf-gcc
CFLAGS=-Os -march=rv32im

PROG=xxtea

OBJS=start.o main.o xxtea.o simple_printf.o getcnt.o
HDR=header.bin

$(PROG).bin: $(OBJS) $(HDR)
	$(TOOLS)riscv32-unknown-elf-ld -T flat.ld -o $(PROG).elf $(OBJS)
	$(TOOLS)riscv32-unknown-elf-objdump -M numeric,no-aliases -d $(PROG).elf > $(PROG).lst
	$(TOOLS)riscv32-unknown-elf-objcopy -O binary $(PROG).elf tmp.bin
	cat $(HDR) tmp.bin > $(PROG).bin
	rm -f tmp.bin

clean:
	rm -rf *.o *.elf *.lst *.bin

STUB=../debug.binary

header.bin: $(STUB)
	cp $(STUB) header.bin
	dd if=/dev/null of=header.bin bs=1 count=1 seek=8192
