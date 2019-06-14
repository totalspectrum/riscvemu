/*
 * SD Card test program
 * designed for P2 version of RiscV emulator
 * define USE_SMARTPIN to toggle using the smartpin NCO mode
 */

#include <stdint.h>
#include <stdbool.h>
#include "../lib/riscv.h"
#define drvl_(x) pinlow(x)
#define drvh_(x) pinhigh(x)

#define PIN_SS   60
#define PIN_MOSI 59
#define PIN_MISO 58
#define PIN_CLK  61
#define PIN_TEST (PIN_MOSI)

struct cardstate {
    bool isSDHC;
} card;

extern void iprintf(const char *, ...);

uint64_t getcycles()
{
    uint32_t hi, lo, h2;
again:
    h2 = getcnth();
    lo = getcnt();
    hi = getcnth();
    if (hi != h2) goto again;
    return ((uint64_t)hi) << 32 | lo;
}

void nap(uint64_t cycles)
{
    uint64_t now;
    uint64_t end;
    now = getcycles();
    end = now + cycles;
    do {
        now = getcycles();
    } while (now < end);
}

#define spi_short_delay() waitcnt(getcnt() + 10)

static int spi_read()
{
    int i, r;
    r = 0;
    for (i = 0; i < 8; i++) {
        drvl_(PIN_CLK);
        drvh_(PIN_CLK);
        spi_short_delay();
        r = (r << 1) | getpin(PIN_MISO);
    }
    return r;
}

// timeout is 100ms if we use a 160 MHz clock
#define TIMEOUT 16000000

static int spi_chktimeout(uint64_t endtime) {
    uint64_t now = getcycles();
    if (now > endtime) {
        return 1;
    }
    return 0;
}

static int spi_readresp(void)
{
    int r;
    uint64_t endtime = getcycles() + TIMEOUT;
    
    while (1) {
        r = spi_read();
        if (r != 0xff) break;
        if (spi_chktimeout(endtime)) {
            return -1;
        }
    }
    return r;
}

static void spi_send(int outv)
{
    int i;
    for (i = 0; i < 8; i++) {
        drvl_(PIN_CLK);
        if (outv & 0x80) {
            drvh_(PIN_MOSI);
        } else {
            drvl_(PIN_MOSI);
        }
        outv <<= 1;
        drvh_(PIN_CLK);
    }
    drvh_(PIN_MOSI);
}

static int crc7(int crc, int val)
{
    int i;
    for (i = 0; i < 8; i++) {
        crc = crc << 1;
        if ( (crc^val) & 0x80 ) {
            crc ^= 0x09;
        }
        val = val<<1;
    }
    return crc & 0x7f;
}

static int crc_send(int crc, int byte)
{
    crc = crc7(crc, byte);
    spi_send(byte);
    return crc;
}

static int spi_cmd(int index, int arg)
{
    int crc = 0;
    drvl_(PIN_SS);
    (void)spi_read();
    crc = crc_send(crc, 0x40+index);
    crc = crc_send(crc, (arg >> 24) & 0xff);
    crc = crc_send(crc, (arg >> 16) & 0xff);
    crc = crc_send(crc, (arg >> 8) & 0xff);
    crc = crc_send(crc, arg & 0xff);

    // send the CRC
    crc = (crc<<1) | 1;
    spi_send(crc);
    return spi_readresp();
}

static void spi_endcmd(void)
{
    drvh_(PIN_SS);
}

int sdcard_wait_for_status(void)
{
    int tries = 0;
    int i;
    while (tries++ < 100) {
        spi_cmd(13, 0);
        i = spi_readresp();
        spi_endcmd();
        if (i == 0) {
//            iprintf("wait worked on try %d\n", tries);
            return 0;
        }
    }
    return -1;
}

bool sdcard_init(void)
{
    int i;
    int x;
    int retries;

    dirl_(PIN_MISO);
    dirh_(PIN_CLK);
    dirh_(PIN_MOSI);
    drvh_(PIN_SS);
    drvh_(PIN_MOSI);
    for (i = 0; i < 600; i++) {
        (void) spi_read();
    }

    i = spi_cmd(0, 0);
    spi_endcmd();
    if (i < 0) {
        iprintf("timeout 1 in init\n");
        return false;
    }
    i = spi_cmd(8, 0x1aa);
    spi_endcmd();
    if (i < 0) {
        iprintf("timeout 2 in init\n");
        return false;
    }
    while (1) {
        spi_cmd(55, 0);
        i = spi_cmd(41, 0x40000000);
        if (i == 0) break;
        if (i < 0) {
            iprintf("timeout in init loop\n");
            return false;
        }
    }
    i = spi_cmd(58, 0);
    x = spi_read();
    spi_endcmd();
    card.isSDHC = (x >> 6) & 1;
    if (i) {
        iprintf("could not init card\n");
        return false;
    } else {
        iprintf("cmd58 returned 0x%x\n", x);
    }
    i = sdcard_wait_for_status();
    if (i < 0) {
        iprintf("timeout waiting for card\n");
        return false;
    }
    return true;
}

bool sdcard_is_present(void)
{
    int i = getpin(PIN_TEST);
    return (i == 0);
}

static void sdcard_print_status(void)
{
    int i;
    spi_cmd(13, 0);
    i = spi_readresp();
    spi_endcmd();
    iprintf("send status returned: 0x%x\n", i);
}

uint64_t sdcard_get_capacity_in_bytes(void)
{
    uint64_t c_size;
    uint8_t csd[16]; // 16 bytes
    int i;

    sdcard_print_status();
    i = spi_cmd(9, 0);
    i = spi_readresp();
    if (i < 0) {
        iprintf("csd read timed out\n");
        spi_endcmd();
        return 0;
    }
    iprintf("csd: ");
    for (i = 0; i < 16; i++) {
        csd[i] = spi_read();
        iprintf("%02x ", csd[i]);
    }
    (void)spi_read();
    (void)spi_read();
    iprintf("\n");
    spi_endcmd();

    c_size = (((uint32_t)csd[7] & 0x3f) << 16) | ((uint32_t)csd[8] << 8) | csd[9];
    c_size = (c_size+1) * (512ULL * 1024ULL);
    return c_size;
}

// read one block
static int sdcard_read_one_block(uint8_t *dest, uint32_t offset)
{
    int r, i;
    if (!card.isSDHC) {
        offset = offset << 9; // multiply by 512 for non SDHC cards
    }
    spi_cmd(17, offset);
    r = spi_readresp();
    if (r < 0) return r;
    for (i = 0; i < 512; i++) {
        *dest++ = spi_read();
    }
    spi_read(); spi_read();
    spi_endcmd();
    return 0;
}

// these return 0 on success, non-zero on error
uint32_t sdcard_read_blocks(uint8_t *dest, uint32_t start_block, uint32_t num_blocks)
{
    uint32_t i;
    int r;
    for (i = 0; i < num_blocks; i++) {
        r = sdcard_read_one_block(dest, start_block);
        if (r) return r;
        dest += 512;
        start_block++;
    }
    return 0;
}

uint32_t sdcard_write_blocks(uint8_t *dest, uint32_t start_block, uint32_t num_blocks)
{
    return -1;
}

uint8_t ibuf[1024];

static void print_buf(uint8_t *buf, int siz)
{
    int i;
    for (i = 0; i < siz; i++) {
        if (0 == (i % 16)) {
            iprintf("%04x: ", i);
        }
        iprintf("%02x", buf[i]);
        if (15 == (i % 16)) {
            iprintf("\n");
        } else {
            iprintf(" ");
        }
    }
    iprintf("\n");
}

void main()
{
    bool ok;
    uint64_t bytes;
    int i;
    
    iprintf("sdcard test program\n");

    for(;;) {
        ok = sdcard_is_present();
        iprintf("sdcard is %spresent\n", ok ? "" : "NOT ");
        nap(160000000);
        if (ok) {
            break;
        }
    }
    ok = sdcard_init();
    if (ok) {
        iprintf("sdcard OK: SDHC = %u\n", (int)card.isSDHC);
        bytes = sdcard_get_capacity_in_bytes();
        iprintf("capacity=%lu MB\n", (uint32_t)(bytes / (1024*1024ULL)));
    } else {
        iprintf("error\n");
    }
    i = sdcard_read_blocks(ibuf, 0, 2);
    if (i) {
        iprintf("error %d from read\n", i);
    } else {
        print_buf(ibuf, 512);
    }
}
