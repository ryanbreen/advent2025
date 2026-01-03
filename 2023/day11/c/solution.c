#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_LINES 200
#define MAX_LINE_LEN 200
#define MAX_GALAXIES 500

typedef struct {
    int row;
    int col;
} Galaxy;

static char grid[MAX_LINES][MAX_LINE_LEN];
static int num_rows = 0;
static int num_cols = 0;

static Galaxy galaxies[MAX_GALAXIES];
static int num_galaxies = 0;

static bool empty_rows[MAX_LINES];
static bool empty_cols[MAX_LINE_LEN];

/* Prefix sums: prefix_empty_rows[i] = count of empty rows in [0, i) */
static int prefix_empty_rows[MAX_LINES + 1];
static int prefix_empty_cols[MAX_LINE_LEN + 1];

static void parse_input(const char *filename) {
    FILE *f = fopen(filename, "r");
    if (!f) {
        perror("Failed to open input file");
        exit(1);
    }

    char line[MAX_LINE_LEN];
    while (fgets(line, sizeof(line), f)) {
        size_t len = strlen(line);
        while (len > 0 && (line[len - 1] == '\n' || line[len - 1] == '\r')) {
            line[--len] = '\0';
        }
        if (len == 0) continue;

        strcpy(grid[num_rows], line);
        if (num_cols == 0) {
            num_cols = (int)len;
        }
        num_rows++;
    }
    fclose(f);
}

static void find_galaxies(void) {
    for (int r = 0; r < num_rows; r++) {
        for (int c = 0; c < num_cols; c++) {
            if (grid[r][c] == '#') {
                galaxies[num_galaxies].row = r;
                galaxies[num_galaxies].col = c;
                num_galaxies++;
            }
        }
    }
}

static void find_empty_rows_and_cols(void) {
    /* Initialize all rows and columns as empty */
    for (int r = 0; r < num_rows; r++) {
        empty_rows[r] = true;
    }
    for (int c = 0; c < num_cols; c++) {
        empty_cols[c] = true;
    }

    /* Mark rows and columns that contain galaxies as not empty */
    for (int i = 0; i < num_galaxies; i++) {
        empty_rows[galaxies[i].row] = false;
        empty_cols[galaxies[i].col] = false;
    }

    /* Build prefix sums for O(1) range queries */
    prefix_empty_rows[0] = 0;
    for (int r = 0; r < num_rows; r++) {
        prefix_empty_rows[r + 1] = prefix_empty_rows[r] + (empty_rows[r] ? 1 : 0);
    }

    prefix_empty_cols[0] = 0;
    for (int c = 0; c < num_cols; c++) {
        prefix_empty_cols[c + 1] = prefix_empty_cols[c] + (empty_cols[c] ? 1 : 0);
    }
}

static long long calculate_distances(long long expansion_factor) {
    long long total = 0;

    for (int i = 0; i < num_galaxies; i++) {
        for (int j = i + 1; j < num_galaxies; j++) {
            int r1 = galaxies[i].row;
            int c1 = galaxies[i].col;
            int r2 = galaxies[j].row;
            int c2 = galaxies[j].col;

            /* Calculate row distance with expansion using prefix sums */
            int min_r = (r1 < r2) ? r1 : r2;
            int max_r = (r1 > r2) ? r1 : r2;
            int empty_row_count = prefix_empty_rows[max_r] - prefix_empty_rows[min_r];
            long long row_dist = (max_r - min_r) + (long long)empty_row_count * (expansion_factor - 1);

            /* Calculate column distance with expansion using prefix sums */
            int min_c = (c1 < c2) ? c1 : c2;
            int max_c = (c1 > c2) ? c1 : c2;
            int empty_col_count = prefix_empty_cols[max_c] - prefix_empty_cols[min_c];
            long long col_dist = (max_c - min_c) + (long long)empty_col_count * (expansion_factor - 1);

            total += row_dist + col_dist;
        }
    }

    return total;
}

static long long part1(void) {
    return calculate_distances(2);
}

static long long part2(void) {
    return calculate_distances(1000000);
}

int main(int argc, char *argv[]) {
    const char *filename = (argc > 1) ? argv[1] : "../input.txt";

    parse_input(filename);
    find_galaxies();
    find_empty_rows_and_cols();

    printf("Part 1: %lld\n", part1());
    printf("Part 2: %lld\n", part2());

    return 0;
}
