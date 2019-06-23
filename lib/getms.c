#include "propeller.h"

#if 0
extern unsigned int getcyclespersec();

unsigned int getms() {
    return getcnt() / (getcyclespersec()/1000);
}
#else
unsigned int getms() {
    return getmillis() * 2;
}
#endif
