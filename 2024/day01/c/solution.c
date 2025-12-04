#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINES 1000

// Comparison function for qsort
int compare_ints(const void *a, const void *b) {
    return (*(int*)a - *(int*)b);
}

int main() {
    FILE *file = fopen("../input.txt", "r");
    if (!file) {
        perror("Failed to open input.txt");
        return 1;
    }

    int left[MAX_LINES], right[MAX_LINES];
    int count = 0;

    // Read input
    while (fscanf(file, "%d %d", &left[count], &right[count]) == 2) {
        count++;
        if (count >= MAX_LINES) break;
    }
    fclose(file);

    // Part 1: Sort both lists and calculate total distance
    int left_sorted[MAX_LINES], right_sorted[MAX_LINES];
    memcpy(left_sorted, left, count * sizeof(int));
    memcpy(right_sorted, right, count * sizeof(int));

    qsort(left_sorted, count, sizeof(int), compare_ints);
    qsort(right_sorted, count, sizeof(int), compare_ints);

    long long part1_sum = 0;
    for (int i = 0; i < count; i++) {
        int diff = left_sorted[i] - right_sorted[i];
        part1_sum += diff > 0 ? diff : -diff;
    }

    // Part 2: Calculate similarity score
    // For each number in left list, count occurrences in right list
    long long part2_sum = 0;
    for (int i = 0; i < count; i++) {
        int num = left[i];
        int occurrences = 0;
        for (int j = 0; j < count; j++) {
            if (right[j] == num) {
                occurrences++;
            }
        }
        part2_sum += (long long)num * occurrences;
    }

    printf("Part 1: %lld\n", part1_sum);
    printf("Part 2: %lld\n", part2_sum);

    return 0;
}
