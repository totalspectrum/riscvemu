extern unsigned int getcnt();

extern unsigned int getcyclespersec();

unsigned int getms() {
    return getcnt() / (getcyclespersec()/1000);
}
