#include <stdlib.h>

extern char _bssend[];
static char *sbrk_ptr = _bssend;

void *
malloc(size_t sz)
{
    void *r;

    r = sbrk_ptr;
    sz = (sz + 15) & ~15; // round up
    sbrk_ptr += sz;
    return r;
}
