#include <propeller.h>
#define CNT _cnt()

unsigned int
getms()
{
    return CNT / 80000;
}
