#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINE 20

int main() {
    FILE *fp = fopen("../input.txt", "r");
    if (!fp) {
        fprintf(stderr, "Error opening input file\n");
        return 1;
    }

    char line[MAX_LINE];
    int position = 50;  // Starting position
    int part1_count = 0;  // Count of times dial ends at 0
    int part2_count = 0;  // Count of all times dial points at 0 (including during rotation)

    while (fgets(line, sizeof(line), fp)) {
        // Skip empty lines
        if (line[0] == '\n' || line[0] == '\0') {
            continue;
        }

        char direction = line[0];
        int distance = atoi(&line[1]);

        // For Part 2, we need to count how many times we click through position 0
        // We count each time the dial lands on 0 during or at the end of rotation

        if (direction == 'L') {
            // Left rotation (toward lower numbers)
            // Going from position to (position - distance)
            // We pass through 0 when we cross from positive to negative (wrap around)
            // Starting at position P, moving left D clicks:
            // We hit 0 at steps: P, P+100, P+200, ... (if those steps <= D)

            // Count how many multiples of 100 we need to add to position to reach or exceed distance
            // We hit 0 when the number of steps equals: position, position+100, position+200, etc.
            int zero_count = 0;
            for (int steps_to_zero = position; steps_to_zero <= distance; steps_to_zero += 100) {
                if (steps_to_zero > 0) {  // Don't count if we start at 0
                    zero_count++;
                }
            }
            part2_count += zero_count;

            // Calculate new position
            int new_pos = position - distance;
            while (new_pos < 0) new_pos += 100;
            position = new_pos;
        } else if (direction == 'R') {
            // Right rotation (toward higher numbers)
            // Going from position to (position + distance)
            // We pass through 0 when we cross from 99 to 0 (wrap around)
            // Starting at position P, moving right D clicks:
            // We hit 0 at steps: (100-P), (100-P)+100, (100-P)+200, ...

            int zero_count = 0;
            int steps_to_first_zero = 100 - position;
            for (int steps_to_zero = steps_to_first_zero; steps_to_zero <= distance; steps_to_zero += 100) {
                zero_count++;
            }
            part2_count += zero_count;

            // Calculate new position
            position = (position + distance) % 100;
        }

        // Check if we ended at 0 for Part 1
        if (position == 0) {
            part1_count++;
        }
    }

    fclose(fp);

    printf("Part 1: %d\n", part1_count);
    printf("Part 2: %d\n", part2_count);

    return 0;
}
