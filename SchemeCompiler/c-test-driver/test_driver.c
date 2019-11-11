#include <stdio.h>
#define fixnum_mask     3
#define fixnum_tag      0
#define fixnum_shift    2

#define char_mask       255
#define char_tag        15
#define char_shift      8

#define bool_mask       127 
#define bool_tag        31
#define bool_shift      7

#define empty_list      47

extern int scheme_entry();

char* bool_rep(int b) {
    if (b) {
        return "#t";
    }
    return "#f";
}

int main(int argc, char** argv) {
    int val = scheme_entry();
    
    printf("---------------------------\n");

    if ((val & fixnum_mask) == fixnum_tag) {
        printf("%d\n", val >> fixnum_shift);
    }

    else if ((val & char_mask) == char_tag) {
        printf("%c\n", val >> char_shift);
    }

    else if ((val & bool_mask) == bool_tag) {
        printf("%s\n", bool_rep(val >> bool_shift));
    }

    else if (val == empty_list) {
        printf("()\n");
    }

    else {
        printf("Unknown: %d\n", val);
    }
}
