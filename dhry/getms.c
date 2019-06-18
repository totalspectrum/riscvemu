#ifdef __riscv
#include "../lib/riscv.h"
#define CNT getcnt()
#else
#include <propeller.h>
#endif

unsigned int
getms()
{
    return CNT / 80000;
}
