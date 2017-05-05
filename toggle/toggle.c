#include <stdint.h>

extern void iprintf(const char *, ...);
extern uint32_t getcnt(void);
extern uint32_t waitcnt(uint32_t tim);
extern uint32_t pin_dirout(uint32_t mask);
extern uint32_t pin_outhi(uint32_t mask);
extern uint32_t pin_outlo(uint32_t mask);

void main()
{
    uint32_t cycles;
    uint32_t freq = 80000000;
    uint32_t mask = 0x1ffff;
    uint32_t x;
    
    x = pin_dirout(mask);
    iprintf("mask was: %08x\n", x);
    x = pin_dirout(mask);
    iprintf("mask is: %08x\n", x);
    cycles = getcnt();
    for(;;) {
        x = pin_outlo(mask);
        iprintf("ping: %08x\n", x);
        cycles += freq;
        waitcnt(cycles);
        x = pin_outhi(mask);
        iprintf(" pong: %08x\n", x);
        cycles += freq;
        waitcnt(cycles);
    }
}
           
