#include <stdio.h>

/* Program to count blanks, tabs and newlines from given input characters */

int main() {

    int c, t, nl, bl;

    t = 0;
    nl = 0;
    bl = 0;
    while ((c = getchar()) != EOF) {
        if (c == '\t') {
            ++t;
        } else if (c == '\n') {
            ++nl;
        } else if (c == ' ')
            ++bl;
    }
    printf("Tabs: %d\nLines: %d\nBlanks: %d\n", t, nl, bl);
}
