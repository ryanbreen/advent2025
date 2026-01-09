#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define MAX_LINE 256
#define MAX_LINES 512

// Convert character to priority (a-z = 1-26, A-Z = 27-52)
static int priority(char c) {
    if (c >= 'a' && c <= 'z') {
        return c - 'a' + 1;
    } else {
        return c - 'A' + 27;
    }
}

// Convert character to bitmask position (0-51)
static int char_to_bit(char c) {
    if (c >= 'a' && c <= 'z') {
        return c - 'a';
    } else {
        return c - 'A' + 26;
    }
}

// Create a bitmask representing all characters in a string
static uint64_t string_to_bitmask(const char *s, size_t len) {
    uint64_t mask = 0;
    for (size_t i = 0; i < len; i++) {
        mask |= (1ULL << char_to_bit(s[i]));
    }
    return mask;
}

// Find the character represented by a bitmask (assumes exactly one bit set)
static char bitmask_to_char(uint64_t mask) {
    for (int i = 0; i < 52; i++) {
        if (mask & (1ULL << i)) {
            if (i < 26) {
                return 'a' + i;
            } else {
                return 'A' + (i - 26);
            }
        }
    }
    return 0;
}

static int part1(char lines[][MAX_LINE], int line_count) {
    int total = 0;

    for (int i = 0; i < line_count; i++) {
        size_t len = strlen(lines[i]);
        size_t mid = len / 2;

        uint64_t first_half = string_to_bitmask(lines[i], mid);
        uint64_t second_half = string_to_bitmask(lines[i] + mid, len - mid);

        uint64_t common = first_half & second_half;
        char c = bitmask_to_char(common);
        total += priority(c);
    }

    return total;
}

static int part2(char lines[][MAX_LINE], int line_count) {
    int total = 0;

    for (int i = 0; i < line_count; i += 3) {
        uint64_t mask1 = string_to_bitmask(lines[i], strlen(lines[i]));
        uint64_t mask2 = string_to_bitmask(lines[i + 1], strlen(lines[i + 1]));
        uint64_t mask3 = string_to_bitmask(lines[i + 2], strlen(lines[i + 2]));

        uint64_t common = mask1 & mask2 & mask3;
        char c = bitmask_to_char(common);
        total += priority(c);
    }

    return total;
}

int main(void) {
    FILE *f = fopen("../input.txt", "r");
    if (!f) {
        perror("Failed to open input file");
        return 1;
    }

    char lines[MAX_LINES][MAX_LINE];
    int line_count = 0;

    while (fgets(lines[line_count], MAX_LINE, f) && line_count < MAX_LINES) {
        // Remove trailing newline
        size_t len = strlen(lines[line_count]);
        if (len > 0 && lines[line_count][len - 1] == '\n') {
            lines[line_count][len - 1] = '\0';
        }
        // Skip empty lines
        if (strlen(lines[line_count]) > 0) {
            line_count++;
        }
    }

    fclose(f);

    printf("Part 1: %d\n", part1(lines, line_count));
    printf("Part 2: %d\n", part2(lines, line_count));

    return 0;
}
