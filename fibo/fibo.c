#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>

//#define iprintf __simple_printf

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

extern unsigned int getcnt(void);

int main (int argc,  char* argv[])
{
	int n;
	int result;
	unsigned int startTime;
	unsigned int endTime;
        unsigned int executionTime;

        iprintf("fibo test version 10 (dec %u)(hex %x)\n", 10, 10, 10);
        
        for (n = 0; n <= 26; n++)
	{
                iprintf("fibo(%02d) = ", n);
		startTime = getcnt();
		result = fibo(n);
		endTime = getcnt() - startTime;
                executionTime = endTime / (CLOCK_FREQUENCY / 1000);
		iprintf ("%06d %8d cycles (%05ums)\n", result, endTime, executionTime);
	}
	return(0);
}
