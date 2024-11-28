#include <stdio.h>
#include <stdlib.h>

void clearScreen() {
    system("cls || clear");
}

void displayMenu() {
    printf("Simple Calculator\n");
    printf("1. Add\n");
    printf("2. Subtract\n");
    printf("3. Multiply\n");
    printf("4. Divide\n");
    printf("5. Clear Screen\n");
    printf("6. Exit\n");
    printf("Choose an operation: ");
}

int main() {
    int choice;
    double num1, num2, result;

    while (1) {
        clearScreen();
        displayMenu();
        if (scanf("%d", &choice) != 1) {
            printf("Invalid input. Please enter a valid option.\n");
            while(getchar() != '\n');
            continue;
        }

        if (choice == 6) {
            break;
        }

        if (choice == 5) {
            clearScreen();
            continue;
        }

        printf("Enter first number: ");
        if (scanf("%lf", &num1) != 1) {
            printf("Invalid input. Please enter a valid number.\n");
            while(getchar() != '\n');
            continue;
        }

        printf("Enter second number: ");
        if (scanf("%lf", &num2) != 1) {
            printf("Invalid input. Please enter a valid number.\n");
            while(getchar() != '\n');
            continue;
        }

        switch (choice) {
            case 1:
                result = num1 + num2;
                printf("Result: %.2lf\n", result);
                break;
            case 2:
                result = num1 - num2;
                printf("Result: %.2lf\n", result);
                break;
            case 3:
                result = num1 * num2;
                printf("Result: %.2lf\n", result);
                break;
            case 4:
                if (num2 != 0) {
                    result = num1 / num2;
                    printf("Result: %.2lf\n", result);
                } else {
                    printf("Error: Division by zero!\n");
                }
                break;
            default:
                printf("Invalid choice! Please choose a valid operation.\n");
                break;
        }

        printf("Press Enter to continue...");
        while(getchar() != '\n');
        getchar();
    }

    return 0;
}