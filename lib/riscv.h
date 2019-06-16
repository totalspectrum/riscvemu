//
// definitions for Control Status Registers for the P2 Risc-V platform
//

// these are the CSR numbers for the P2 hardware registers
#define DIRA 0x7fa
#define DIRB 0x7fb
#define OUTA 0x7fc
#define OUTB 0x7fd
#define INA  0x7fe
#define INB  0x7ff

#define UART 0xBC0
#define WAITCYC 0xBC1
#define DBGPRNT 0xBC2
#define MILLIS_CSR 0xBC3
#define CNT  0xC00
#define CNTH 0xC80

#define X__(x) #x
#define X_(x) X__(x)

// read data from a CSR
#define csr_read(csr)						\
({								\
	register unsigned long __v;				\
	__asm__ __volatile__ ("csrr %0, " X_(csr)             \
			      : "=r" (__v));			\
	__v;							\
})

// write val to the CSR (csr = val)
#define csr_write(csr, val)					\
({								\
	unsigned long __v = (unsigned long)(val);		\
	__asm__ __volatile__ ("csrw " X_(csr) ", %0"		\
			      : : "rK" (__v));			\
})

// read and then write the CSR (csr = val, return old csr)
#define csr_read_write(csr, val)				\
({								\
	unsigned long __v = (unsigned long)(val);		\
	__asm__ __volatile__ ("csrrw %0, " X_(csr) ", %1"      \
			      : "=r" (__v) : "rK" (__v));	\
	__v;							\
})

// set bits in a CSR (does "csr |= val")
#define csr_set(csr, val)					\
({								\
	unsigned long __v = (unsigned long)(val);		\
	__asm__ __volatile__ ("csrs " X_(csr) ", %0"		\
			      : : "rK" (__v));			\
})

// clear bits in a CSR (does "csr &= ~val"; that is, 1 bits in val
// indicate where we want to clear bits in the csr)
#define csr_clear(csr, val)					\
({								\
	unsigned long __v = (unsigned long)(val);		\
	__asm__ __volatile__ ("csrc " X_(csr) ", %0"		\
			      : : "rK" (__v));			\
})

#define getcnt() csr_read(CNT)
#define getcnth() csr_read(CNTH)
#define waitcnt(tim) csr_write(WAITCYC, tim)
#define getmillis() csr_read(MILLIS_CSR)
#define trigger_debug() csr_read(DBGPRNT)

// NOTE:
// CUSTOM0 opcode is 0x0b (2<<2)+3
// CUSTOM1 opcode is 0x2b (10<<2)+3
#define getpin(pin)                                     \
    ({                                                  \
        unsigned long v;                                \
        __asm__ __volatile__ (".insn s 0x0b, 7, %0, 0(%1)" \
                              : "=r"(v) : "r"(pin) );        \
        v;                                                  \
    })

#define setpin(pin, value)                             \
    ({                                                  \
        unsigned long v = value;                         \
        __asm__ __volatile__ (".insn sb CUSTOM_0, 2, %0, 0x000(%1)" \
                              : : "r"(v), "r"(pin) );             \
        v;                                                  \
    })

#define togglepin(pin) \
    ({                                                  \
        __asm__ __volatile__ (".insn sb CUSTOM_0, 2, x0, -0x400(%0)" \
                              : : "r"(pin) );             \
    })

#define pinlow(pin) \
    ({                                                  \
        __asm__ __volatile__ (".insn sb CUSTOM_0, 2, x0, 0x000(%0)" \
                              : : "r"(pin) );             \
    })
#define pinhigh(pin) \
    ({                                                  \
        __asm__ __volatile__ (".insn sb CUSTOM_0, 2, x0, 0x400(%0)" \
                              : : "r"(pin) );             \
    })

#define dirl_(pin) \
    ({                                                  \
        __asm__ __volatile__ (".insn sb CUSTOM_0, 5, x0, 0x000(%0)" \
                              : : "r"(pin) );             \
    })
#define dirh_(pin) \
    ({                                                  \
        __asm__ __volatile__ (".insn sb CUSTOM_0, 5, x0, 0x400(%0)" \
                              : : "r"(pin) );             \
    })

#define pinwr(pin, value)                               \
    ({                                                  \
        unsigned long v = value;                         \
        __asm__ __volatile__ (".insn sb CUSTOM_0, 6, %0, 0x000(%1)" \
                              : : "r"(v), "r"(pin) );             \
        v;                                                  \
    })
#define pinwx(pin, value)                             \
    ({                                                  \
        unsigned long v = value;                         \
        __asm__ __volatile__ (".insn sb CUSTOM_0, 6, %0, 0x400(%1)" \
                              : : "r"(v), "r"(pin) );             \
        v;                                                  \
    })
#define pinwy(pin, value)                             \
    ({                                                  \
        unsigned long v = value;                         \
        __asm__ __volatile__ (".insn sb CUSTOM_0, 6, %0, -0x800(%1)" \
                              : : "r"(v), "r"(pin) );             \
        v;                                                  \
    })

#define pinrdr(pin)                                     \
    ({                                                  \
        unsigned long v;                                \
        __asm__ __volatile__ (".insn s 0x0b, 7, %0, 0x400(%1)" \
                              : "=r"(v) : "r"(pin) );        \
        v;                                                  \
    })

#define coginit(a, b, c)                                  \
    ({                                                  \
        unsigned long v;                                \
        __asm__ __volatile__ (".insn r CUSTOM_1, 0, 0, %0, %1, %2, %3" \
                              : "=r"(v) : "r"(a), "r"(b), "r"(c)  );    \
        v;                                                  \
    })

#define cognew(a, b) coginit(0x10, a, b)

#define cogstop(a)                                     \
    ({                                                  \
        unsigned long v;                                \
        __asm__ __volatile__ (".insn r CUSTOM_1, 1, 0, %0, %1, x3" \
                              : "=r"(v) : "r"(a)  );    \
        v;                                                  \
    })

#define getrnd()                                        \
    ({                                                  \
        unsigned long v;                                \
        __asm__ __volatile__ (".insn r CUSTOM_1, 1, 0, %0, %0, x27" \
                              : "=r"(v)  );    \
        v;                                              \
    })

#define waitx(a)                                     \
    ({                                                  \
        unsigned long v;                                \
        __asm__ __volatile__ (".insn r CUSTOM_1, 1, 0, %0, %1, x31" \
                              : "=r"(v) : "r"(a)  );    \
        v;                                                  \
    })

#define waitx(a)                                     \
    ({                                                  \
        unsigned long v;                                \
        __asm__ __volatile__ (".insn r CUSTOM_1, 1, 0, %0, %1, x31" \
                              : "=r"(v) : "r"(a)  );    \
        v;                                                  \
    })
