/*
 * Advent of Code 2023 Day 12: Hot Springs
 *
 * Uses memoized dynamic programming to count valid arrangements.
 * State: (position, group_index, current_run_length)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <inttypes.h>

#define MAX_PATTERN_LEN 128
#define MAX_GROUPS 64
#define MAX_RUN_LEN 32
#define UNFOLD_FACTOR 5

/* Memoization table: -1 means not computed yet */
static int64_t memo[MAX_PATTERN_LEN][MAX_GROUPS][MAX_RUN_LEN];

/* Global pattern and groups for current line */
static char pattern[MAX_PATTERN_LEN];
static int groups[MAX_GROUPS];
static int pattern_len;
static int num_groups;

static void reset_memo(void) {
    memset(memo, -1, sizeof(memo));
}

/*
 * DP function: count valid arrangements starting from (pos, group_idx, current_run)
 */
static int64_t dp(int pos, int group_idx, int current_run) {
    /* Base case: reached end of pattern */
    if (pos == pattern_len) {
        /* Valid if we've matched all groups and no partial run */
        if (group_idx == num_groups && current_run == 0)
            return 1;
        /* Or if we're on the last group and the run matches */
        if (group_idx == num_groups - 1 && groups[group_idx] == current_run)
            return 1;
        return 0;
    }

    /* Check memoization */
    if (memo[pos][group_idx][current_run] != -1)
        return memo[pos][group_idx][current_run];

    int64_t result = 0;
    char c = pattern[pos];

    /* Option 1: Place operational spring (.) */
    if (c == '.' || c == '?') {
        if (current_run == 0) {
            /* No active run, just move forward */
            result += dp(pos + 1, group_idx, 0);
        } else if (group_idx < num_groups && groups[group_idx] == current_run) {
            /* End current run if it matches expected group size */
            result += dp(pos + 1, group_idx + 1, 0);
        }
        /* Otherwise invalid (run doesn't match group) */
    }

    /* Option 2: Place damaged spring (#) */
    if (c == '#' || c == '?') {
        if (group_idx < num_groups && current_run < groups[group_idx]) {
            /* Can extend current run */
            result += dp(pos + 1, group_idx, current_run + 1);
        }
        /* Otherwise invalid (exceeds group size or no more groups) */
    }

    memo[pos][group_idx][current_run] = result;
    return result;
}

static int64_t count_arrangements(void) {
    reset_memo();
    return dp(0, 0, 0);
}

/*
 * Parse a line into pattern and groups.
 * Returns 1 on success, 0 on failure.
 */
static int parse_line(const char *line) {
    /* Find the space separator */
    const char *space = strchr(line, ' ');
    if (!space)
        return 0;

    /* Copy pattern */
    int plen = (int)(space - line);
    if (plen >= MAX_PATTERN_LEN)
        return 0;
    strncpy(pattern, line, plen);
    pattern[plen] = '\0';
    pattern_len = plen;

    /* Parse groups */
    num_groups = 0;
    char *p = (char *)(space + 1);
    while (*p && *p != '\n' && *p != '\r') {
        if (num_groups >= MAX_GROUPS)
            return 0;
        groups[num_groups++] = (int)strtol(p, &p, 10);
        if (*p == ',')
            p++;
    }

    return 1;
}

/*
 * Unfold pattern and groups by repeating 5 times.
 */
static void unfold(void) {
    /* Save original values */
    char orig_pattern[MAX_PATTERN_LEN];
    int orig_groups[MAX_GROUPS];
    int orig_plen = pattern_len;
    int orig_ngroups = num_groups;

    strcpy(orig_pattern, pattern);
    memcpy(orig_groups, groups, num_groups * sizeof(int));

    /* Build unfolded pattern: pattern?pattern?pattern?pattern?pattern */
    pattern_len = 0;
    for (int i = 0; i < UNFOLD_FACTOR; i++) {
        if (i > 0)
            pattern[pattern_len++] = '?';
        memcpy(pattern + pattern_len, orig_pattern, orig_plen);
        pattern_len += orig_plen;
    }
    pattern[pattern_len] = '\0';

    /* Build unfolded groups */
    num_groups = 0;
    for (int i = 0; i < UNFOLD_FACTOR; i++) {
        memcpy(groups + num_groups, orig_groups, orig_ngroups * sizeof(int));
        num_groups += orig_ngroups;
    }
}

int main(void) {
    FILE *fp = fopen("../input.txt", "r");
    if (!fp) {
        fprintf(stderr, "Failed to open ../input.txt\n");
        return 1;
    }

    char line[256];
    int64_t part1_total = 0;
    int64_t part2_total = 0;

    while (fgets(line, sizeof(line), fp)) {
        /* Skip empty lines */
        if (line[0] == '\n' || line[0] == '\r' || line[0] == '\0')
            continue;

        if (!parse_line(line)) {
            fprintf(stderr, "Failed to parse line: %s", line);
            continue;
        }

        /* Part 1: count arrangements for original pattern */
        part1_total += count_arrangements();

        /* Part 2: unfold and count arrangements */
        unfold();
        part2_total += count_arrangements();
    }

    fclose(fp);

    printf("Part 1: %" PRId64 "\n", part1_total);
    printf("Part 2: %" PRId64 "\n", part2_total);

    return 0;
}
