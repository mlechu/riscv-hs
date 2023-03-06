int mul(int a, int b) {
    return  a * b;
}

// multiply the first character digit of two arguments
int main(int argc, char **argv) {
    if (argc != 3) {
        return -1;
    } 
    char i1 = argv[1][0] - 0x30;
    char i2 = argv[2][0] - 0x30;

    // non-digit provided
    if (i1 < 0 | i1 > 9) {
        return -2;
    } else if (i2 < 0 | i2 > 9) {
        return -3;
    }

    int c = mul(i1, i2);
    return c; 
}
