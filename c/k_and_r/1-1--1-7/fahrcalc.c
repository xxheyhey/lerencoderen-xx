#include <stdio.h>

/* This is a program that takes a degrees Celsius input from the user and
 * outputs the corresponding Fahrenheit value. */

int main() {
    
    int input; 
    printf("Do you want to convert:\nC -> F (1), or\nF -> C (2)?\nEnter 1 or 2: ");
    scanf("%d", &input);

    if (input == 1) {
        double celsius, fahr_out;
        printf("Enter Celsius: ");
        scanf("%lf", &celsius);
        
        fahr_out = celsius * (9.0/5.0) +32;
        
        printf("\nFahrenheit: %3.2lf\n", fahr_out);
        return(0);

    } else if (input == 2) {
        double fahr, celsius_out;
        printf("Enter Fahrenheit: ");
        scanf("%lf", &fahr);
        
        celsius_out = (fahr - 32) * (5.0/9.0);

        printf("\nCelsius: %3.2lf\n", celsius_out);
        return(0);
    }


}
