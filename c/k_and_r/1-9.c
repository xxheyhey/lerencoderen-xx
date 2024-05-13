#include <stdio.h>

int main() {

    int c;

    while ((c = getchar()) != EOF) {
        putchar(c);

        while (c == ' ') {
           if ((c = getchar()) != ' ') {
               putchar(c);
               break;
           }
        }
    }
}
