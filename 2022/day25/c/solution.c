#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINE 64
#define MAX_LINES 256

/* Convert a SNAFU digit to its decimal value */
int snafu_digit_value(char c) {
    switch (c) {
        case '2': return 2;
        case '1': return 1;
        case '0': return 0;
        case '-': return -1;
        case '=': return -2;
        default: return 0;
    }
}

/* Convert SNAFU string to decimal */
long long snafu_to_decimal(const char *s) {
    long long result = 0;
    for (int i = 0; s[i] != '\0' && s[i] != '\n'; i++) {
        result = result * 5 + snafu_digit_value(s[i]);
    }
    return result;
}

/* Convert decimal to SNAFU string (stores result in buffer) */
void decimal_to_snafu(long long n, char *result) {
    if (n == 0) {
        strcpy(result, "0");
        return;
    }

    char digits[MAX_LINE];
    int idx = 0;

    while (n > 0) {
        int remainder = n % 5;
        if (remainder <= 2) {
            digits[idx++] = '0' + remainder;
            n /= 5;
        } else if (remainder == 3) {
            digits[idx++] = '=';
            n = n / 5 + 1;
        } else { /* remainder == 4 */
            digits[idx++] = '-';
            n = n / 5 + 1;
        }
    }

    /* Reverse the digits into result */
    for (int i = 0; i < idx; i++) {
        result[i] = digits[idx - 1 - i];
    }
    result[idx] = '\0';
}

int main(void) {
    FILE *fp = fopen("../input.txt", "r");
    if (!fp) {
        perror("Failed to open input.txt");
        return 1;
    }

    char line[MAX_LINE];
    long long total = 0;

    while (fgets(line, sizeof(line), fp)) {
        if (line[0] == '\n' || line[0] == '\0') continue;
        total += snafu_to_decimal(line);
    }

    fclose(fp);

    char snafu_result[MAX_LINE];
    decimal_to_snafu(total, snafu_result);

    printf("Part 1: %s\n", snafu_result);
    printf("Part 2: No Part 2 on Day 25!\n");

    return 0;
}
