#include "../lib/riscv.h"
#include "BufferSerial.h"
#include "OneCogKbM.h"

OneCogKbM usb1;
BufferSerial ser1;

static char usb1_status[4];
static int usb1_eventa;

int main()
{
    int data;
    int err;
    int sercog;
    volatile int *errptr;
    OneCogKbM_start(&usb1, (int32_t)&usb1_status);
    sercog = BufferSerial_start(&ser1);
    iprintf("test program: usb cog=%d ser cog=%d\n", usb1_status[0], sercog);
    errptr = (volatile int *)OneCogKbM_geterrorcode();
    iprintf("errptr=0x%x\n", (int32_t)errptr);
    for(;;) {
        data = OneCogKbM_key();
        if (data != 0) {
            iprintf("usb data=0x%x\n", data);
        }
        data = BufferSerial_rx(&ser1);
        if (data >= 0) {
            iprintf("ser data=0x%x\n", data);
        }
        err = *errptr;
        if (err) {
            iprintf("error=0x%x\n", err);
        }
    }
}
