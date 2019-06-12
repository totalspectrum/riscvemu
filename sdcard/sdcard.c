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

//
// assume 160 MHz; then delay 160 -> 1 MHz, 1600 -> 10 KHz
//
static void delay_us(uint32_t us)
{
    us = 160 * us;
    waitcnt(getcnt() + us);
}

static void spi_delay(void)
{
    delay_us(5);
}

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

#define TIMEOUT 160000000

static int spi_chktimeout(uint32_t starttime) {
    uint32_t now = getcnt();
    if (now - starttime > TIMEOUT) {
        return 1;
    }
    return 0;
}

static int spi_readresp(void)
{
    int r;
    uint32_t starttime = getcnt();
    
    while (1) {
        r = spi_read();
        if (r != 0xff) break;
        if (spi_chktimeout(starttime)) {
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

int crc7(int crc, int val)
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

int crc_send(int crc, int byte)
{
    crc = crc7(crc, byte);
    spi_send(byte);
    return crc;
}

int spi_cmd(int index, int arg)
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

void spi_endcmd(void)
{
    drvh_(PIN_SS);
}

bool sdcard_init(void)
{
    int i;
    int x;
    int retries;

    delay_us(1000);
    
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
        
    return true;
}

bool sdcard_is_present(void)
{
    int i = getpin(PIN_TEST);
    return (i == 0);
}

uint64_t sdcard_get_capacity_in_bytes(void)
{
    uint64_t c_size;
    uint8_t csd[16]; // 16 bytes
    int i;
    i = spi_cmd(9, 0);
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
    c_size = (c_size+1) * 512ULL * 1024ULL;
    return c_size;
}

// these return 0 on success, non-zero on error
uint32_t sdcard_read_blocks(uint8_t *dest, uint32_t block_num, uint32_t num_blocks)
{
    return -1;
}
uint32_t sdcard_write_blocks(uint8_t *dest, uint32_t block_num, uint32_t num_blocks)
{
    return -1;
}

void main()
{
    bool ok;
    uint64_t bytes;

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
        iprintf("capacity= %llu bytes (%lu MB)\n", bytes, (uint32_t)(bytes / (1024*1024ULL)));
    } else {
        iprintf("error\n");
    }
}
