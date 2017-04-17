TOOLS=/opt/riscv/bin/

test.bin: test.s
	$(TOOLS)riscv32-unknown-elf-as -R -fpic -o test.o test.s
	$(TOOLS)riscv32-unknown-elf-ld -T flat.ld -o test.elf test.o
	$(TOOLS)riscv32-unknown-elf-objdump -M numeric,no-aliases -d test.elf > test.lst
	$(TOOLS)riscv32-unknown-elf-objcopy -O binary test.elf test.bin

clean:
	rm -rf *.o *.elf *.lst *.bin

