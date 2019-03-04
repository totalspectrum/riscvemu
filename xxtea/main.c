#include <stdio.h>
#include <time.h>
#include "xxtea.h"

#ifdef __riscv
extern int iprintf(const char *, ...);
extern unsigned int getcnt();
extern int getbyte();
extern unsigned int getcyclespersec();
extern void waitcnt(unsigned x);
#endif
#ifdef __propeller__
#include <propeller.h>
#define iprintf printf
#define getcyclespersec() 80000000
#endif

uint32_t testVector[] = {0x9d204ce7, 0xc03677f6, 0x320f0555, 0x499c703c,
                         0x8b8af399, 0x061b6314, 0x7d410085, 0xe65b712c,
                         0xb7d23609, 0x0270cd59, 0xa23dc8d1};

#ifdef __FLEXC__
char *key = "0123456789ABCDEF";
#else
char key[16] = "0123456789ABCDEF";
#endif
int blockSize = sizeof(testVector) / 4;

void sleep(unsigned n)
{
    clock_t end = n * getcyclespersec() + getcnt();
    waitcnt(end);
}

int main(int argc, char* argv[])
{
    clock_t start, end;
    int i;
#ifdef __FLEXC__
    clkset(0x010007f8, 160000000);
    _setbaud(230400);
#endif
#if 0    
    iprintf("xxtea test: press 'a' to begin\n");
    for(;;) {
        i = getbyte();
        if (i < 0) continue;
        iprintf("got: `%c'\n", i);
        if (i == 'a') break;
    }
#endif    
    iprintf("starting...\n");
    start = getcnt();
    btea (testVector, -blockSize, (uint32_t*) key);
    end = getcnt();

    iprintf("%s\n", (char*)testVector);
    iprintf("done in %lu cycles\n", (unsigned long)(end - start));

    return(0);
} 
