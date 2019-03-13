/*
 * pin toggle test program
 * designed for P2 version of RiscV emulator
 */

#include <stdint.h>
#include "riscv.h"

extern void iprintf(const char *, ...);

void main()
{
    uint32_t cycles;
    uint32_t freq = 160000000;
    uint32_t pin = 57;
    uint32_t x;
    
    iprintf("pin is: %08x\n", pin);
    cycles = getcnt();
    for(;;) {
        setpin(pin, 0);
        x = csr_read(OUTB);
        iprintf("ping: %08x\n", x);
        cycles += freq;
        waitcnt(cycles);
        setpin(pin, 1);
        x = csr_read(OUTB);
        iprintf(" pong: %08x\n", x);
        cycles += freq;
        waitcnt(cycles);
    }
}

