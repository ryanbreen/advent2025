#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINES 1024
#define MAX_BITS 16

static char numbers[MAX_LINES][MAX_BITS + 1];
static int num_count = 0;
static int num_bits = 0;

void parse_input(void) {
    FILE *f = fopen("../input.txt", "r");
    if (!f) {
        perror("Failed to open input.txt");
        exit(1);
    }

    char line[MAX_BITS + 2];
    while (fgets(line, sizeof(line), f) && num_count < MAX_LINES) {
        size_t len = strlen(line);
        while (len > 0 && (line[len - 1] == '\n' || line[len - 1] == '\r')) {
            line[--len] = '\0';
        }
        if (len > 0) {
            strcpy(numbers[num_count], line);
            if (num_bits == 0) {
                num_bits = (int)len;
            }
            num_count++;
        }
    }
    fclose(f);
}

int part1(void) {
    int gamma = 0;

    for (int pos = 0; pos < num_bits; pos++) {
        int ones = 0;
        for (int i = 0; i < num_count; i++) {
            if (numbers[i][pos] == '1') {
                ones++;
            }
        }
        int zeros = num_count - ones;

        if (ones >= zeros) {
            gamma |= (1 << (num_bits - 1 - pos));
        }
    }

    /* epsilon is bitwise NOT of gamma (within num_bits) */
    int epsilon = gamma ^ ((1 << num_bits) - 1);

    return gamma * epsilon;
}

int find_rating(int use_most_common) {
    /* Track which candidates are still active */
    int active[MAX_LINES];
    int active_count = num_count;

    for (int i = 0; i < num_count; i++) {
        active[i] = 1;
    }

    for (int pos = 0; pos < num_bits; pos++) {
        if (active_count == 1) {
            break;
        }

        int ones = 0;
        for (int i = 0; i < num_count; i++) {
            if (active[i] && numbers[i][pos] == '1') {
                ones++;
            }
        }
        int zeros = active_count - ones;

        char target;
        if (use_most_common) {
            target = (ones >= zeros) ? '1' : '0';
        } else {
            target = (zeros <= ones) ? '0' : '1';
        }

        for (int i = 0; i < num_count; i++) {
            if (active[i] && numbers[i][pos] != target) {
                active[i] = 0;
                active_count--;
            }
        }
    }

    /* Find the remaining number and convert to int */
    for (int i = 0; i < num_count; i++) {
        if (active[i]) {
            int value = 0;
            for (int j = 0; j < num_bits; j++) {
                if (numbers[i][j] == '1') {
                    value |= (1 << (num_bits - 1 - j));
                }
            }
            return value;
        }
    }

    return 0;
}

int part2(void) {
    int oxygen = find_rating(1);
    int co2 = find_rating(0);
    return oxygen * co2;
}

int main(void) {
    parse_input();
    printf("Part 1: %d\n", part1());
    printf("Part 2: %d\n", part2());
    return 0;
}
