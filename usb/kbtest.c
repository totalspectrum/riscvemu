#include "../lib/riscv.h"
#include "OneCogKbM.h"

OneCogKbM usb1;

static char usb1_status[4];
static int usb1_eventa;

int main()
{
    int data;
    int err;
    volatile int *errptr;
    OneCogKbM_start(&usb1, (int32_t)&usb1_status);
    iprintf("test program: cog=%d\n", usb1_status[0]);
    errptr = (volatile int *)OneCogKbM_geterrorcode();
    iprintf("errptr=0x%x\n", (int32_t)errptr);
    for(;;) {
        data = OneCogKbM_key();
        if (data != 0) {
            iprintf("data=0x%x\n", data);
        }
        err = *errptr;
        if (err) {
            iprintf("error=0x%x\n", err);
        }
    }
}
