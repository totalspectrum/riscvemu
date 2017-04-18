TOOLS=/opt/riscv/bin/
AS=$(TOOLS)riscv32-unknown-elf-as
ASFLAGS=-fpic -march=rv32im
LD=$(TOOLS)riscv32-unknown-elf-ld

CC=$(TOOLS)riscv32-unknown-elf-gcc
CFLAGS=-Os -march=rv32im

PROG=xxtea

OBJS=start.o main.o xxtea.o simple_printf.o getcnt.o


$(PROG).bin: $(OBJS)
	$(TOOLS)riscv32-unknown-elf-ld -T flat.ld -o $(PROG).elf $(OBJS)
	$(TOOLS)riscv32-unknown-elf-objdump -M numeric,no-aliases -d $(PROG).elf > $(PROG).lst
	$(TOOLS)riscv32-unknown-elf-objcopy -O binary $(PROG).elf $(PROG).bin

clean:
	rm -rf *.o *.elf *.lst *.bin

