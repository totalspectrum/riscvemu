#define TIMER_ADDRESS   0xF00007C4
unsigned int getcnt(void)
{
    volatile unsigned int* timer = (volatile unsigned int *)TIMER_ADDRESS;
    return *timer;
}
