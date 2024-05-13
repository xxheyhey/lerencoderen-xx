#include <stdio.h>

/* This is a program that shows the Fahrenheit-Celsius conversion table
 * from 0 to 300 degrees Fahrenheit. */

int main() {

    double fahr, celsius;
    int lower, upper, step;

    lower = 0; /* lower limit of the temperature table */
    upper = 300; /* upper limit of the temperature table */
    step = 20; /* steps of the table in Fahrenheit */
    
    printf("Fahrenheit-Celsius conversion table\n");
    printf("Fahr\t   Cel\n");
    
    fahr = lower;
    while (fahr <= upper) {
                celsius = (fahr-32.0) * 5.0/9.0;
                printf("%4.0lf\t%6.2lf\n", fahr, celsius);
                fahr += step;
    }
}
