#include "../lib/propeller.h"
#include <sys/stat.h>

int
_fstat(int fd, struct stat *buf)
{
    buf->st_mode = S_IFCHR;
    buf->st_blksize = 0;
    return 0;
}

int
_write(int fd, unsigned char *data, int count)
{
    int c;
    int orig_count = count;
    while (count-- > 0) {
        c = *data++;
        csr_write(UART_CSR, c);
    }
    return orig_count;
}
