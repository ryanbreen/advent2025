/*
 * Advent of Code 2023 - Day 9: Mirage Maintenance
 * Extrapolate OASIS sequence values using difference pyramids.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_LINE_LEN 1024
#define MAX_DEPTH 64
#define MAX_VALUES 128

static long pyramid[MAX_DEPTH][MAX_VALUES];
static int pyramid_lengths[MAX_DEPTH];

static bool all_zeros(long *seq, int len) {
    for (int i = 0; i < len; i++) {
        if (seq[i] != 0) {
            return false;
        }
    }
    return true;
}

static int build_difference_pyramid(long *seq, int len) {
    memcpy(pyramid[0], seq, len * sizeof(long));
    pyramid_lengths[0] = len;

    int depth = 1;
    long *current = pyramid[0];
    int current_len = len;

    while (!all_zeros(current, current_len)) {
        int next_len = current_len - 1;
        for (int i = 0; i < next_len; i++) {
            pyramid[depth][i] = current[i + 1] - current[i];
        }
        pyramid_lengths[depth] = next_len;
        current = pyramid[depth];
        current_len = next_len;
        depth++;
    }

    return depth;
}

static long extrapolate_next(long *seq, int len) {
    int depth = build_difference_pyramid(seq, len);

    for (int i = depth - 2; i >= 0; i--) {
        int this_len = pyramid_lengths[i];
        int next_len = pyramid_lengths[i + 1];
        pyramid[i][this_len] = pyramid[i][this_len - 1] + pyramid[i + 1][next_len - 1];
        pyramid_lengths[i]++;
    }

    return pyramid[0][pyramid_lengths[0] - 1];
}

static void reverse_array(long *arr, int len) {
    for (int i = 0; i < len / 2; i++) {
        long tmp = arr[i];
        arr[i] = arr[len - 1 - i];
        arr[len - 1 - i] = tmp;
    }
}

int main(void) {
    FILE *fp = fopen("../input.txt", "r");
    if (!fp) {
        perror("Failed to open input.txt");
        return 1;
    }

    long part1 = 0;
    long part2 = 0;

    char line[MAX_LINE_LEN];
    while (fgets(line, sizeof(line), fp)) {
        long values[MAX_VALUES];
        int count = 0;

        char *token = strtok(line, " \t\n");
        while (token != NULL && count < MAX_VALUES) {
            values[count++] = strtol(token, NULL, 10);
            token = strtok(NULL, " \t\n");
        }

        if (count > 0) {
            part1 += extrapolate_next(values, count);
            reverse_array(values, count);
            part2 += extrapolate_next(values, count);
        }
    }

    fclose(fp);

    printf("Part 1: %ld\n", part1);
    printf("Part 2: %ld\n", part2);

    return 0;
}
