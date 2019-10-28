#include <stdio.h>
extern scheme_entry();

int main(int argc, char** argv) {
    int val = scheme_entry();
    printf("%d\n", val);
}