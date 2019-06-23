#include <propeller.h>
#ifdef __riscv
#define CNT getcnt()
#endif

unsigned int
getms()
{
    return CNT / 80000;
}
