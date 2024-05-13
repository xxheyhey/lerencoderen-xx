#include <stdio.h>

int main() {
    printf("Enter a character: ");

    int test = (getchar() != EOF);

    printf("(getchar() != EOF) == %d\n", test);

    int c;

    while ((c = getchar()) != EOF) {
        putchar(c);
    }
}
