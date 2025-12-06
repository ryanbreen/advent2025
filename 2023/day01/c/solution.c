#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>
#include <errno.h>

#define MAX_LINE_LENGTH 256
#define NUM_WORD_DIGITS 9

/* Mapping of spelled-out digits to their numeric value */
typedef struct {
    const char *word;
    size_t length;
    char digit;
} WordDigit;

static const WordDigit WORD_DIGITS[NUM_WORD_DIGITS] = {
    {"one",   3, '1'},
    {"two",   3, '2'},
    {"three", 5, '3'},
    {"four",  4, '4'},
    {"five",  4, '5'},
    {"six",   3, '6'},
    {"seven", 5, '7'},
    {"eight", 5, '8'},
    {"nine",  4, '9'}
};

/* Check if a spelled-out digit starts at the given position
 * Uses memcmp instead of strncmp for performance (no null-byte check needed)
 */
static char find_word_digit(const char *str, size_t remaining_len) {
    for (size_t i = 0; i < NUM_WORD_DIGITS; i++) {
        if (remaining_len >= WORD_DIGITS[i].length &&
            memcmp(str, WORD_DIGITS[i].word, WORD_DIGITS[i].length) == 0) {
            return WORD_DIGITS[i].digit;
        }
    }
    return '\0';
}

/* Extract calibration value from a line (first digit * 10 + last digit)
 * Algorithm: Single pass through the line, tracking first and last digits found.
 * Cache strlen() result to avoid O(n) call in loop condition.
 */
static int extract_calibration_value(const char *line, bool include_words) {
    char first = '\0';
    char last = '\0';
    const size_t line_len = strlen(line);  /* Cache length - don't call in loop */

    for (size_t i = 0; i < line_len; i++) {
        char digit = '\0';

        if (isdigit((unsigned char)line[i])) {
            digit = line[i];
        } else if (include_words) {
            digit = find_word_digit(&line[i], line_len - i);
        }

        if (digit != '\0') {
            if (first == '\0') {
                first = digit;
            }
            last = digit;
        }
    }

    if (first != '\0' && last != '\0') {
        return (first - '0') * 10 + (last - '0');
    }

    return 0;
}

/* Process input file and sum calibration values */
static int solve(const char *filename, bool include_words) {
    FILE *file = fopen(filename, "r");
    if (!file) {
        fprintf(stderr, "Error: Unable to open file '%s': %s\n",
                filename, strerror(errno));
        return -1;
    }

    char line[MAX_LINE_LENGTH];
    int total = 0;

    while (fgets(line, sizeof(line), file)) {
        /* Remove trailing newline if present */
        line[strcspn(line, "\n")] = '\0';

        total += extract_calibration_value(line, include_words);
    }

    fclose(file);
    return total;
}

static int part1(const char *filename) {
    return solve(filename, false);
}

static int part2(const char *filename) {
    return solve(filename, true);
}

int main(void) {
    const char *input_file = "../input.txt";

    int result1 = part1(input_file);
    if (result1 < 0) {
        return EXIT_FAILURE;
    }
    printf("Part 1: %d\n", result1);

    int result2 = part2(input_file);
    if (result2 < 0) {
        return EXIT_FAILURE;
    }
    printf("Part 2: %d\n", result2);

    return EXIT_SUCCESS;
}
