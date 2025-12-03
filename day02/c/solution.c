#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

// Part 1: Check if a number is "invalid" - made of a pattern repeated EXACTLY twice
bool is_invalid_id_part1(long long num) {
    char str[32];
    sprintf(str, "%lld", num);
    int len = strlen(str);

    // Must be even length to be repeated exactly twice
    if (len % 2 != 0) {
        return false;
    }

    int half = len / 2;
    // Check if first half equals second half
    for (int i = 0; i < half; i++) {
        if (str[i] != str[half + i]) {
            return false;
        }
    }

    return true;
}

// Part 2: Check if a number is "invalid" - made of a pattern repeated at least twice
bool is_invalid_id_part2(long long num) {
    char str[32];
    sprintf(str, "%lld", num);
    int len = strlen(str);

    // Try all possible pattern lengths (from 1 to len/2)
    // The pattern must repeat at least twice, so max pattern length is len/2
    for (int pattern_len = 1; pattern_len <= len / 2; pattern_len++) {
        // Check if the string length is divisible by pattern length
        if (len % pattern_len != 0) {
            continue;
        }

        // Check if the entire string is made of this pattern repeated
        bool is_repeated = true;
        for (int i = pattern_len; i < len; i++) {
            if (str[i] != str[i % pattern_len]) {
                is_repeated = false;
                break;
            }
        }

        if (is_repeated) {
            return true;
        }
    }

    return false;
}

int main() {
    FILE *fp = fopen("../input.txt", "r");
    if (!fp) {
        fprintf(stderr, "Error opening input file\n");
        return 1;
    }

    // Read the entire input line
    char *line = NULL;
    size_t len = 0;
    ssize_t read = getline(&line, &len, fp);
    fclose(fp);

    if (read == -1) {
        fprintf(stderr, "Error reading input\n");
        return 1;
    }

    // Make a copy of the line for the second pass
    char *line_copy = strdup(line);

    long long part1_sum = 0;
    long long part2_sum = 0;

    // Parse ranges separated by commas
    char *token = strtok(line, ",");
    while (token != NULL) {
        // Parse range (start-end)
        long long start, end;
        if (sscanf(token, "%lld-%lld", &start, &end) == 2) {
            // Check each number in the range
            for (long long num = start; num <= end; num++) {
                if (is_invalid_id_part1(num)) {
                    part1_sum += num;
                }
                if (is_invalid_id_part2(num)) {
                    part2_sum += num;
                }
            }
        }
        token = strtok(NULL, ",");
    }

    printf("Part 1: %lld\n", part1_sum);
    printf("Part 2: %lld\n", part2_sum);

    free(line);
    free(line_copy);
    return 0;
}
