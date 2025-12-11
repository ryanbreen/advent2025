#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_GRID_SIZE 64
#define MAX_ANTENNAS 256
#define NUM_ASCII_CHARS 128

typedef struct {
    int r, c;
} Position;

typedef struct {
    char freq;
    Position positions[MAX_ANTENNAS];
    int count;
} AntennaGroup;

typedef struct {
    Position positions[MAX_GRID_SIZE * MAX_GRID_SIZE];
    int count;
} PositionSet;

static int rows, cols;
static AntennaGroup groups[NUM_ASCII_CHARS];

static inline bool in_bounds(int r, int c) {
    return r >= 0 && r < rows && c >= 0 && c < cols;
}

static void add_to_set(PositionSet *set, int r, int c) {
    // Bounds checking to prevent overflow
    if (set->count >= MAX_GRID_SIZE * MAX_GRID_SIZE) {
        return;
    }
    // Check if position already exists
    for (int i = 0; i < set->count; i++) {
        if (set->positions[i].r == r && set->positions[i].c == c) {
            return;
        }
    }
    // Add new position
    set->positions[set->count].r = r;
    set->positions[set->count].c = c;
    set->count++;
}

static void parse_input(const char *filename) {
    FILE *f = fopen(filename, "r");
    if (!f) {
        perror("Failed to open input file");
        exit(EXIT_FAILURE);
    }

    char grid[MAX_GRID_SIZE][MAX_GRID_SIZE];
    rows = 0;
    cols = 0;

    // Read grid
    while (fgets(grid[rows], MAX_GRID_SIZE, f) && rows < MAX_GRID_SIZE) {
        // Remove newline
        size_t len = strlen(grid[rows]);
        if (len > 0 && grid[rows][len - 1] == '\n') {
            grid[rows][len - 1] = '\0';
            len--;
        }
        if ((int)len > cols) {
            cols = (int)len;
        }
        rows++;
    }
    fclose(f);

    // Initialize groups
    memset(groups, 0, sizeof(groups));

    // Parse antennas
    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols && grid[r][c] != '\0'; c++) {
            char ch = grid[r][c];
            if (ch != '.') {
                // Cast to unsigned char to safely use as array index (0-127)
                int idx = (unsigned char)ch;
                if (groups[idx].count == 0) {
                    groups[idx].freq = ch;
                }
                groups[idx].positions[groups[idx].count].r = r;
                groups[idx].positions[groups[idx].count].c = c;
                groups[idx].count++;
            }
        }
    }
}

static int part1(void) {
    PositionSet antinodes = {0};

    // For each frequency group
    for (int i = 0; i < NUM_ASCII_CHARS; i++) {
        if (groups[i].count == 0) {
            continue;
        }

        const AntennaGroup *group = &groups[i];

        // For each pair of antennas
        for (int j = 0; j < group->count; j++) {
            for (int k = j + 1; k < group->count; k++) {
                int r1 = group->positions[j].r;
                int c1 = group->positions[j].c;
                int r2 = group->positions[k].r;
                int c2 = group->positions[k].c;

                // Calculate the two antinodes
                // Antinode beyond antenna 1 (away from antenna 2)
                int ar1 = 2 * r1 - r2;
                int ac1 = 2 * c1 - c2;

                // Antinode beyond antenna 2 (away from antenna 1)
                int ar2 = 2 * r2 - r1;
                int ac2 = 2 * c2 - c1;

                // Add if within bounds
                if (in_bounds(ar1, ac1)) {
                    add_to_set(&antinodes, ar1, ac1);
                }
                if (in_bounds(ar2, ac2)) {
                    add_to_set(&antinodes, ar2, ac2);
                }
            }
        }
    }

    return antinodes.count;
}

static int part2(void) {
    PositionSet antinodes = {0};

    // For each frequency group
    for (int i = 0; i < NUM_ASCII_CHARS; i++) {
        if (groups[i].count == 0) {
            continue;
        }

        const AntennaGroup *group = &groups[i];

        // For each pair of antennas
        for (int j = 0; j < group->count; j++) {
            for (int k = j + 1; k < group->count; k++) {
                int r1 = group->positions[j].r;
                int c1 = group->positions[j].c;
                int r2 = group->positions[k].r;
                int c2 = group->positions[k].c;

                int dr = r2 - r1;
                int dc = c2 - c1;

                // Extend in both directions along the line
                // Direction 1: from antenna 1 towards and beyond antenna 2
                int r = r1, c = c1;
                while (in_bounds(r, c)) {
                    add_to_set(&antinodes, r, c);
                    r += dr;
                    c += dc;
                }

                // Direction 2: from antenna 1 away from antenna 2
                r = r1 - dr;
                c = c1 - dc;
                while (in_bounds(r, c)) {
                    add_to_set(&antinodes, r, c);
                    r -= dr;
                    c -= dc;
                }
            }
        }
    }

    return antinodes.count;
}

int main(void) {
    parse_input("../input.txt");

    printf("Part 1: %d\n", part1());
    printf("Part 2: %d\n", part2());

    return 0;
}
