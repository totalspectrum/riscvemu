// C demo for VGA text objects
#define  VGA_BASEPIN 48

#include <string.h>

// include the Spin object
#include "vgatext.h"

#ifdef __riscv
#include <stdio.h>
#endif

vgatext vga;
#define vga_rows VGATEXT_ROWS
#define vga_cols VGATEXT_COLS
#define vga_start(pin) vgatext_start(&vga, pin)
#define vga_tx(c) vgatext_tx(&vga, c)
#define vga_dec(c) vgatext_dec(&vga, c)
#define vga_str(s) vgatext_str(&vga, s)

#define ESC 27
//#define ESC 'E' // for debugging

// print a line of stars
static void starline() {
    int i;
    for (i = 0; i < vga_cols; i++) {
        vga_tx('*');
    }
}
static void blankline() {
    int i;
    vga_tx('*');
    for (i = 1; i < vga_cols-1; i++) {
        vga_tx(' ');
    }
    vga_tx('*');
}
static void gotoxy(int x, int y) {

    //iprintf("(E%d;%dH)", y, x);
    vga_tx(ESC);
    vga_tx('[');
    vga_dec(y);
    vga_tx(';');
    vga_dec(x);
    vga_tx('H');
}
    
static void center(const char *msg) {
    int n = strlen(msg);
    int y = vga_rows/2;
    int x = (vga_cols - n) / 2;
    gotoxy(x, y);
    vga_str(msg);
}

// main routine
void main()
{
    int i;
#ifndef __riscv    
    clkset(_SETFREQ, _CLOCKFREQ);
#endif    
    i = vga_start(VGA_BASEPIN);
#ifdef __riscv
    iprintf("vga_start returned %d\n", i);
#endif

    starline();
    for (i = 1; i < vga_rows-1; i++) {
        blankline();
    }
    starline();
    center("Hello, world!");
    for(;;)
        ;
}
