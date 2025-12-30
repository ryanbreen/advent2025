#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_INPUT 32768
#define MAX_STEPS 4096
#define MAX_LABEL 16
#define MAX_LENSES 64

typedef struct {
    char label[MAX_LABEL];
    int focal;
} Lens;

typedef struct {
    Lens lenses[MAX_LENSES];
    int count;
} Box;

static int hash_algorithm(const char *s, int len) {
    int current = 0;
    for (int i = 0; i < len; i++) {
        current = ((current + (unsigned char)s[i]) * 17) % 256;
    }
    return current;
}

static long part1(char **steps, int num_steps) {
    long total = 0;
    for (int i = 0; i < num_steps; i++) {
        total += hash_algorithm(steps[i], strlen(steps[i]));
    }
    return total;
}

static long part2(char **steps, int num_steps) {
    Box boxes[256] = {0};

    for (int i = 0; i < num_steps; i++) {
        char *step = steps[i];
        char *eq = strchr(step, '=');
        char *dash = strchr(step, '-');

        if (eq) {
            // "label=focal" operation
            int label_len = eq - step;
            char label[MAX_LABEL];
            strncpy(label, step, label_len);
            label[label_len] = '\0';

            int focal = atoi(eq + 1);
            int box_num = hash_algorithm(label, label_len);

            // Find if label already exists
            int found = 0;
            for (int j = 0; j < boxes[box_num].count; j++) {
                if (strcmp(boxes[box_num].lenses[j].label, label) == 0) {
                    boxes[box_num].lenses[j].focal = focal;
                    found = 1;
                    break;
                }
            }
            if (!found) {
                // Add new lens
                int idx = boxes[box_num].count;
                strcpy(boxes[box_num].lenses[idx].label, label);
                boxes[box_num].lenses[idx].focal = focal;
                boxes[box_num].count++;
            }
        } else if (dash) {
            // "label-" operation
            int label_len = dash - step;
            char label[MAX_LABEL];
            strncpy(label, step, label_len);
            label[label_len] = '\0';

            int box_num = hash_algorithm(label, label_len);

            // Find and remove lens
            for (int j = 0; j < boxes[box_num].count; j++) {
                if (strcmp(boxes[box_num].lenses[j].label, label) == 0) {
                    // Shift remaining lenses down
                    for (int k = j; k < boxes[box_num].count - 1; k++) {
                        boxes[box_num].lenses[k] = boxes[box_num].lenses[k + 1];
                    }
                    boxes[box_num].count--;
                    break;
                }
            }
        }
    }

    // Calculate focusing power
    long total = 0;
    for (int box_num = 0; box_num < 256; box_num++) {
        for (int slot = 0; slot < boxes[box_num].count; slot++) {
            total += (long)(box_num + 1) * (slot + 1) * boxes[box_num].lenses[slot].focal;
        }
    }
    return total;
}

int main(void) {
    FILE *fp = fopen("../input.txt", "r");
    if (!fp) {
        perror("Failed to open input file");
        return 1;
    }

    // Read entire file
    char *input = malloc(MAX_INPUT);
    if (!input) {
        fclose(fp);
        return 1;
    }

    size_t total_read = 0;
    size_t bytes;
    while ((bytes = fread(input + total_read, 1, MAX_INPUT - total_read - 1, fp)) > 0) {
        total_read += bytes;
    }
    input[total_read] = '\0';
    fclose(fp);

    // Remove trailing newlines
    while (total_read > 0 && (input[total_read - 1] == '\n' || input[total_read - 1] == '\r')) {
        input[--total_read] = '\0';
    }

    // Parse comma-separated steps
    char **steps = malloc(MAX_STEPS * sizeof(char *));
    int num_steps = 0;

    char *token = strtok(input, ",");
    while (token && num_steps < MAX_STEPS) {
        steps[num_steps++] = token;
        token = strtok(NULL, ",");
    }

    printf("Part 1: %ld\n", part1(steps, num_steps));
    printf("Part 2: %ld\n", part2(steps, num_steps));

    free(steps);
    free(input);
    return 0;
}
