TOOLS=/opt/riscv/bin/

test.bin: test.s
	$(TOOLS)riscv32-unknown-elf-as -o test.o test.s
	$(TOOLS)riscv32-unknown-elf-objdump -M numeric,no-aliases -d test.o > test.lst
	$(TOOLS)riscv32-unknown-elf-objcopy -O binary test.o test.bin

