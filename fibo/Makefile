TOOLS=/opt/riscv/bin/
AS=$(TOOLS)riscv32-unknown-elf-as
ASFLAGS=-fpic -march=rv32im
LD=$(TOOLS)riscv32-unknown-elf-ld

CC=$(TOOLS)riscv32-unknown-elf-gcc
CFLAGS=-Os -march=rv32im

OBJS=start.o fibo.o simple_printf.o getcnt.o

fibo.bin: $(OBJS)
	$(TOOLS)riscv32-unknown-elf-ld -T flat.ld -o fibo.elf $(OBJS)
	$(TOOLS)riscv32-unknown-elf-objdump -M numeric,no-aliases -d fibo.elf > fibo.lst
	$(TOOLS)riscv32-unknown-elf-objcopy -O binary fibo.elf fibo.bin

clean:
	rm -rf *.o *.elf *.lst *.bin

