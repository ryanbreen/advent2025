#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define MAX_LEVELS 100
#define MAX_LINE 1024

typedef struct {
    int levels[MAX_LEVELS];
    int count;
} Report;

bool is_safe_report(int *levels, int count) {
    if (count < 2) return true;

    bool increasing = levels[1] > levels[0];

    for (int i = 0; i < count - 1; i++) {
        int diff = levels[i + 1] - levels[i];
        int abs_diff = abs(diff);

        // Check if difference is in valid range [1, 3]
        if (abs_diff < 1 || abs_diff > 3) {
            return false;
        }

        // Check if direction is consistent
        if ((increasing && diff <= 0) || (!increasing && diff >= 0)) {
            return false;
        }
    }

    return true;
}

bool is_safe_with_dampener(int *levels, int count) {
    // First check if already safe
    if (is_safe_report(levels, count)) {
        return true;
    }

    // Try removing each level one at a time
    for (int skip = 0; skip < count; skip++) {
        int temp[MAX_LEVELS];
        int temp_count = 0;

        for (int i = 0; i < count; i++) {
            if (i != skip) {
                temp[temp_count++] = levels[i];
            }
        }

        if (is_safe_report(temp, temp_count)) {
            return true;
        }
    }

    return false;
}

int main() {
    FILE *file = fopen("../input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    Report reports[1000];
    int report_count = 0;
    char line[MAX_LINE];

    // Read and parse input
    while (fgets(line, sizeof(line), file)) {
        Report *report = &reports[report_count];
        report->count = 0;

        char *token = strtok(line, " \n");
        while (token != NULL && report->count < MAX_LEVELS) {
            report->levels[report->count++] = atoi(token);
            token = strtok(NULL, " \n");
        }

        if (report->count > 0) {
            report_count++;
        }
    }

    fclose(file);

    // Part 1: Count safe reports
    int safe_count = 0;
    for (int i = 0; i < report_count; i++) {
        if (is_safe_report(reports[i].levels, reports[i].count)) {
            safe_count++;
        }
    }

    printf("Part 1: %d\n", safe_count);

    // Part 2: Count safe reports with dampener
    int safe_with_dampener = 0;
    for (int i = 0; i < report_count; i++) {
        if (is_safe_with_dampener(reports[i].levels, reports[i].count)) {
            safe_with_dampener++;
        }
    }

    printf("Part 2: %d\n", safe_with_dampener);

    return 0;
}
