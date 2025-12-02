#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

// Check if a number is "invalid" - made of a sequence of digits repeated at least twice
bool is_invalid_id(long long num) {
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
    FILE *fp = fopen("/Users/wrb/fun/code/advent2025/day02/input.txt", "r");
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

    long long total_sum = 0;

    // Parse ranges separated by commas
    char *token = strtok(line, ",");
    while (token != NULL) {
        // Parse range (start-end)
        long long start, end;
        if (sscanf(token, "%lld-%lld", &start, &end) == 2) {
            // Check each number in the range
            for (long long num = start; num <= end; num++) {
                if (is_invalid_id(num)) {
                    total_sum += num;
                }
            }
        }
        token = strtok(NULL, ",");
    }

    printf("Total sum of invalid IDs: %lld\n", total_sum);

    free(line);
    return 0;
}
