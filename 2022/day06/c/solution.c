#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_INPUT 8192

int find_marker(const char *data, int len, int window_size) {
    /* Use a frequency array to track character counts in the window */
    int freq[26] = {0};
    int unique_count = 0;

    for (int i = 0; i < len; i++) {
        int c = data[i] - 'a';

        /* Add new character to window */
        if (freq[c] == 0) {
            unique_count++;
        }
        freq[c]++;

        /* Remove character that's leaving the window */
        if (i >= window_size) {
            int old_c = data[i - window_size] - 'a';
            freq[old_c]--;
            if (freq[old_c] == 0) {
                unique_count--;
            }
        }

        /* Check if we have a valid marker */
        if (i >= window_size - 1 && unique_count == window_size) {
            return i + 1;
        }
    }

    return -1;
}

int part1(const char *data, int len) {
    return find_marker(data, len, 4);
}

int part2(const char *data, int len) {
    return find_marker(data, len, 14);
}

int main(void) {
    char data[MAX_INPUT];
    FILE *fp = fopen("../input.txt", "r");

    if (!fp) {
        fprintf(stderr, "Error: Could not open input.txt\n");
        return 1;
    }

    if (!fgets(data, MAX_INPUT, fp)) {
        fprintf(stderr, "Error: Could not read input\n");
        fclose(fp);
        return 1;
    }
    fclose(fp);

    /* Remove trailing newline if present */
    int len = strlen(data);
    if (len > 0 && data[len - 1] == '\n') {
        data[--len] = '\0';
    }

    printf("Part 1: %d\n", part1(data, len));
    printf("Part 2: %d\n", part2(data, len));

    return 0;
}
