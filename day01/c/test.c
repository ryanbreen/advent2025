#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINE 20

int main() {
    FILE *fp = fopen("test_example.txt", "r");
    if (!fp) {
        fprintf(stderr, "Error opening input file\n");
        return 1;
    }

    char line[MAX_LINE];
    int position = 50;  // Starting position
    int part1_count = 0;  // Count of times dial ends at 0
    int part2_count = 0;  // Count of all times dial points at 0 (including during rotation)

    printf("Starting at position %d\n", position);

    while (fgets(line, sizeof(line), fp)) {
        // Skip empty lines
        if (line[0] == '\n' || line[0] == '\0') {
            continue;
        }

        char direction = line[0];
        int distance = atoi(&line[1]);

        int new_position;
        int zeros_during = 0;

        if (direction == 'L') {
            // Left rotation (toward lower numbers)
            new_position = position - distance;
            // Count how many times we cross 0 during this rotation
            while (new_position < 0) {
                new_position += 100;
                zeros_during++;
                part2_count++;
            }
            position = new_position;
        } else if (direction == 'R') {
            // Right rotation (toward higher numbers)
            new_position = position + distance;
            // Count how many times we cross 0 during this rotation
            while (new_position >= 100) {
                new_position -= 100;
                zeros_during++;
                part2_count++;
            }
            position = new_position;
        }

        // Check if we ended at 0 for Part 1
        if (position == 0) {
            part1_count++;
            printf("%c%d -> %d (ENDS AT 0)", direction, distance, position);
        } else {
            printf("%c%d -> %d", direction, distance, position);
        }

        if (zeros_during > 0) {
            printf(" (crossed 0 %d time(s))\n", zeros_during);
        } else {
            printf("\n");
        }
    }

    fclose(fp);

    printf("\nPart 1: %d\n", part1_count);
    printf("Part 2: %d\n", part2_count);

    return 0;
}
