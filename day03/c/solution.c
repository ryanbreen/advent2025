#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdint.h>

#define MAX_LINE_LENGTH 1024

// Find the maximum two-digit number from a bank of batteries
int max_joltage(const char *line) {
    int len = strlen(line);
    if (len < 2) return 0;

    // Allocate array for max suffix values
    int *max_suffix = (int *)malloc(len * sizeof(int));
    if (!max_suffix) {
        fprintf(stderr, "Memory allocation failed\n");
        exit(1);
    }

    // Build max suffix array - max_suffix[i] = max digit from position i to end
    max_suffix[len - 1] = line[len - 1] - '0';
    for (int i = len - 2; i >= 0; i--) {
        int digit = line[i] - '0';
        max_suffix[i] = (digit > max_suffix[i + 1]) ? digit : max_suffix[i + 1];
    }

    // Find maximum two-digit number
    int max_value = 0;
    for (int i = 0; i < len - 1; i++) {
        int first_digit = line[i] - '0';
        int second_digit = max_suffix[i + 1];
        int value = first_digit * 10 + second_digit;
        if (value > max_value) {
            max_value = value;
        }
    }

    free(max_suffix);
    return max_value;
}

int part1(const char *filename) {
    FILE *fp = fopen(filename, "r");
    if (!fp) {
        fprintf(stderr, "Error opening file: %s\n", filename);
        exit(1);
    }

    char line[MAX_LINE_LENGTH];
    int total = 0;

    while (fgets(line, sizeof(line), fp)) {
        // Remove trailing newline
        size_t len = strlen(line);
        if (len > 0 && line[len - 1] == '\n') {
            line[len - 1] = '\0';
            len--;
        }

        // Skip empty lines
        if (len == 0) continue;

        int joltage = max_joltage(line);
        total += joltage;
    }

    fclose(fp);
    return total;
}

// Find the maximum k-digit number from a bank of batteries using greedy algorithm
uint64_t max_k_digits(const char *line, int k) {
    int len = strlen(line);
    if (len < k) return 0;

    char result[13]; // Max 12 digits + null terminator
    int result_idx = 0;
    int current_pos = 0;

    // For each position in the result
    for (int i = 0; i < k; i++) {
        // We need to pick (k - i) more digits total
        // We can search from current_pos to (len - (k - i)) inclusive
        int search_end = len - (k - i);

        // Find the maximum digit in this range
        char max_digit = line[current_pos];
        int max_pos = current_pos;

        for (int j = current_pos + 1; j <= search_end; j++) {
            if (line[j] > max_digit) {
                max_digit = line[j];
                max_pos = j;
            }
        }

        // Add the maximum digit to result
        result[result_idx++] = max_digit;
        // Move past this position for next iteration
        current_pos = max_pos + 1;
    }

    result[result_idx] = '\0';

    // Convert to uint64_t
    uint64_t value = 0;
    for (int i = 0; i < k; i++) {
        value = value * 10 + (result[i] - '0');
    }

    return value;
}

uint64_t part2(const char *filename) {
    FILE *fp = fopen(filename, "r");
    if (!fp) {
        fprintf(stderr, "Error opening file: %s\n", filename);
        exit(1);
    }

    char line[MAX_LINE_LENGTH];
    uint64_t total = 0;

    while (fgets(line, sizeof(line), fp)) {
        // Remove trailing newline
        size_t len = strlen(line);
        if (len > 0 && line[len - 1] == '\n') {
            line[len - 1] = '\0';
            len--;
        }

        // Skip empty lines
        if (len == 0) continue;

        uint64_t joltage = max_k_digits(line, 12);
        total += joltage;
    }

    fclose(fp);
    return total;
}

int main(int argc, char *argv[]) {
    const char *input_file = "../input.txt";

    // Allow command-line override of input file
    if (argc > 1) {
        input_file = argv[1];
    }

    printf("Part 1: %d\n", part1(input_file));
    printf("Part 2: %llu\n", part2(input_file));

    return 0;
}
