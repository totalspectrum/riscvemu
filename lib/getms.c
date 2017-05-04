extern unsigned int getcnt();

unsigned int getms() {
    return getcnt() / 80000;
}
