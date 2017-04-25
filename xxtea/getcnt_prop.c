#include <propeller.h>
#undef getcnt

unsigned int getcnt(void)
{
    return CNT;
}
