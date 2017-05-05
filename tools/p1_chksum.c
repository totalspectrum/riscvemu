#include <stdio.h>

int main(int argc, char **argv)
{
    unsigned char buffer[32768];
    FILE *infile = fopen(argv[1], "r+b");
    int i, num, wrote;
    unsigned char checksum = 0;

    if (!infile) {
        perror(argv[1]);
        return 1;
    }
    num = fread(buffer, 1, 32768, infile);
    if (num < 16 || num > 32768) {
        fprintf(stderr, "read error\n");
        return 1;
    }
    buffer[5] = 0;
    for (i = 0; i < num; i++) checksum += buffer[i];
    buffer[5] = 0x14 - checksum;
    printf("Checksum was %x\n", checksum);
    rewind(infile);
    wrote = fwrite(buffer, 1, num, infile);
    if (wrote != num) {
        fprintf(stderr, "write error\n");
        return 1;
    }
    return 0;
}
