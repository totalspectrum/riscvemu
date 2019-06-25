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
    int j;
    
    for(j = 0; j < 3; j++) {
        i = pasmtest_start(&P);
        iprintf("start returned %d\n", i);

        n = pasmtest_sum(&P, 100, 200);
        iprintf("result = %d (expect 300)\n", n);
        n = pasmtest_sum(&P, i, j);
        iprintf("result = %d (expect %d)\n", n, i+j);

        pasmtest_stop(&P);
        iprintf("stopped cog...\n");
    }
}
