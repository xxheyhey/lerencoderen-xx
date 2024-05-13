#include <stdio.h>

int main() {

    double cel, fahr;
    int lower, upper, step;

    lower = -100;
    upper = 200;
    step = 20;

    printf("Cel\tFahr\n");
    
    cel = lower;
    while(cel <= upper) {
        fahr = cel * (9.0/5.0) + 32.0;
        printf("%4.0lf\t%7.2lf\n", cel, fahr);
        cel += step;
    }
    return(0);
}

