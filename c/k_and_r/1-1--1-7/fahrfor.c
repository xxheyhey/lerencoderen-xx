#include <stdio.h>

#define LOWER 0
#define UPPER 300
#define STEP 20

/* A different way to make the F-C conversion table using a for loop. */
int main() {
    
    int fahr;

    printf("Fahr\tCel\n");

    for (fahr = UPPER; fahr >= LOWER; fahr -= STEP) { 
        printf("%3d\t%6.1lf\n", fahr, (5.0 / 9.0)*(fahr - 32));
    }
}
