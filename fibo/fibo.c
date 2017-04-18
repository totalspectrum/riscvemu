#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>

//#define iprintf __simple_printf

#define TIMER_ADDRESS   0xF00007C4
#define CLOCK_FREQUENCY  (80000000)

unsigned int fibo (unsigned int n)
{
        if (n <= 1)
        {
                return (n);
        }
        else
        {
                return fibo(n - 1) + fibo(n - 2);
        }
}

int main (int argc,  char* argv[])
{
	int n;
	int result;
	unsigned int* timer = TIMER_ADDRESS;
	unsigned int startTime;
	unsigned int endTime;
        unsigned int executionTime;

/*
        startTime = *timer;
	while (1)
        {
                endTime = *timer;
                executionTime = (endTime - startTime) / (CLOCK_FREQUENCY / 1000);
		iprintf("Timer = %u\n", executionTime);
        }
*/
        for (n = 0; n <= 26; n++)
	{
                iprintf("fibo(%02d) = ", n);
		startTime = *timer;
		result = fibo(n);
		endTime = *timer - startTime;
                executionTime = endTime / (CLOCK_FREQUENCY / 1000);
		iprintf ("%06d %8d cycles (%05ums)\n", result, endTime, executionTime);
	}
	return(0);
}
