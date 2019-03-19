// C demo for coginit

#include <stdio.h>
#include <string.h>

// include the Spin object
#include "pasmtest.h"

pasmtest P;

// main routine
void main()
{
    int i;
    int n;
    
    i = pasmtest_start(&P);
    iprintf("start returned %d\n", i);

    n = pasmtest_sum(&P, 100, 200);
    iprintf("result = %d (expect 300)\n", n);
    n = pasmtest_sum(&P, -5, 7);
    iprintf("result = %d (expect 2)\n", n);

    pasmtest_stop(&P);
}
