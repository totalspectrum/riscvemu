/*
 * pin toggle test program
 * designed for P2 version of RiscV emulator
 * define USE_SMARTPIN to toggle using the smartpin NCO mode
 */

#define USE_SMARTPIN

#ifdef USE_SMARTPIN
#define ncomode 0b01001100
#endif

#include <stdint.h>
#include "../lib/riscv.h"

extern void iprintf(const char *, ...);

uint64_t getcycles()
{
    uint32_t hi, lo, h2;
again:
    h2 = getcnth();
    lo = getcnt();
    hi = getcnth();
    if (hi != h2) goto again;
    return ((uint64_t)hi) << 32 | lo;
}

void main()
{
    uint32_t cycles;
    uint32_t freq = 160000000;
    uint32_t pin = 57;
    uint32_t inpin = 4;
    uint32_t x, y;
    uint64_t llc;
    
    iprintf("pin is: %08x\n", pin);
    cycles = getcnt();
#ifdef USE_SMARTPIN
    uint32_t bitperiod = 16000;
    uint32_t incr = 0x80000000U / (freq / (4*bitperiod));
    dirl_(pin);
    pinwr(pin, ncomode);
    pinwx(pin, bitperiod);
    pinwy(pin, incr);
    dirh_(pin);
    for(;;) {
        llc = getcycles();
        
        // set button low before reading
        pinlow(inpin);
        waitcnt(160000000 / 30 + getcnt());
        dirl_(inpin);
        waitcnt(160000000 / 10000 + getcnt());
        y = getpin(inpin);
        x = csr_read(INA);
        iprintf("cycles = %x:%08x x = %08x, y = %d\n", (uint32_t)(llc >> 32), (uint32_t)llc, x, y);
        waitcnt(getcnt() + 80000000);
    }
#else    
    for(;;) {
        pinlow(pin); //setpin(pin, 0);
        x = csr_read(OUTB);
        iprintf("ping: %08x\n", x);
        cycles += freq;
        waitcnt(cycles);
        //setpin(pin, 1);
        pinhigh(pin);
        x = csr_read(OUTB);
        iprintf(" pong: %08x\n", x);
        cycles += freq;
        waitcnt(cycles);
    }
#endif    
}

