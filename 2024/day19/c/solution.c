#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <inttypes.h>

#define MAX_PATTERNS 500
#define MAX_PATTERN_LEN 16
#define MAX_DESIGNS 500
#define MAX_DESIGN_LEN 128
#define MAX_LINE_LEN 4096

static char patterns[MAX_PATTERNS][MAX_PATTERN_LEN];
static int pattern_lens[MAX_PATTERNS];
static int num_patterns = 0;

static char designs[MAX_DESIGNS][MAX_DESIGN_LEN];
static int design_lens[MAX_DESIGNS];
static int num_designs = 0;

void parse_input(const char *filename) {
    FILE *f = fopen(filename, "r");
    if (!f) {
        perror("Failed to open input file");
        exit(1);
    }

    char line[MAX_LINE_LEN];

    // Read patterns line
    if (fgets(line, sizeof(line), f)) {
        char *token = strtok(line, ", \n\r");
        while (token != NULL) {
            if (num_patterns >= MAX_PATTERNS) {
                fprintf(stderr, "Error: exceeded MAX_PATTERNS (%d)\n", MAX_PATTERNS);
                exit(1);
            }
            strncpy(patterns[num_patterns], token, MAX_PATTERN_LEN - 1);
            patterns[num_patterns][MAX_PATTERN_LEN - 1] = '\0';
            pattern_lens[num_patterns] = strlen(patterns[num_patterns]);
            num_patterns++;
            token = strtok(NULL, ", \n\r");
        }
    }

    // Skip empty line
    if (fgets(line, sizeof(line), f) == NULL) {
        fprintf(stderr, "Error: expected empty line after patterns\n");
        exit(1);
    }

    // Read designs
    while (fgets(line, sizeof(line), f)) {
        // Remove newline
        size_t len = strlen(line);
        while (len > 0 && (line[len-1] == '\n' || line[len-1] == '\r')) {
            line[--len] = '\0';
        }
        if (len > 0) {
            if (num_designs >= MAX_DESIGNS) {
                fprintf(stderr, "Error: exceeded MAX_DESIGNS (%d)\n", MAX_DESIGNS);
                exit(1);
            }
            strncpy(designs[num_designs], line, MAX_DESIGN_LEN - 1);
            designs[num_designs][MAX_DESIGN_LEN - 1] = '\0';
            design_lens[num_designs] = strlen(designs[num_designs]);
            num_designs++;
        }
    }

    fclose(f);
}

// Count ways to form design[pos:] using patterns
// Uses dynamic programming with memoization via array
int64_t count_ways(const char *design, int design_len) {
    // dp[i] = number of ways to form design[i:]
    int64_t *dp = calloc(design_len + 1, sizeof(int64_t));
    if (!dp) {
        perror("Memory allocation failed");
        exit(1);
    }

    // Base case: empty suffix can be formed in exactly one way
    dp[design_len] = 1;

    // Fill from right to left
    for (int pos = design_len - 1; pos >= 0; pos--) {
        dp[pos] = 0;
        int remaining = design_len - pos;

        for (int p = 0; p < num_patterns; p++) {
            int plen = pattern_lens[p];
            if (plen <= remaining) {
                // Check if pattern matches at this position
                if (strncmp(design + pos, patterns[p], plen) == 0) {
                    dp[pos] += dp[pos + plen];
                }
            }
        }
    }

    int64_t result = dp[0];
    free(dp);
    return result;
}

int part1(void) {
    int count = 0;
    for (int d = 0; d < num_designs; d++) {
        if (count_ways(designs[d], design_lens[d]) > 0) {
            count++;
        }
    }
    return count;
}

int64_t part2(void) {
    int64_t total = 0;
    for (int d = 0; d < num_designs; d++) {
        total += count_ways(designs[d], design_lens[d]);
    }
    return total;
}

int main(void) {
    parse_input("../input.txt");

    printf("Part 1: %d\n", part1());
    printf("Part 2: %" PRId64 "\n", part2());

    return 0;
}
