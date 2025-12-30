#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define MAX_ROWS 200
#define MAX_COLS 200
#define MAX_STATES 1000000

static char grid[MAX_ROWS][MAX_COLS];
static int rows = 0;
static int cols = 0;

// Hash states for cycle detection
static uint64_t state_hashes[MAX_STATES];
static int state_count = 0;

void tilt_north(void) {
    for (int c = 0; c < cols; c++) {
        int write_pos = 0;
        for (int r = 0; r < rows; r++) {
            if (grid[r][c] == '#') {
                write_pos = r + 1;
            } else if (grid[r][c] == 'O') {
                grid[r][c] = '.';
                grid[write_pos][c] = 'O';
                write_pos++;
            }
        }
    }
}

void tilt_south(void) {
    for (int c = 0; c < cols; c++) {
        int write_pos = rows - 1;
        for (int r = rows - 1; r >= 0; r--) {
            if (grid[r][c] == '#') {
                write_pos = r - 1;
            } else if (grid[r][c] == 'O') {
                grid[r][c] = '.';
                grid[write_pos][c] = 'O';
                write_pos--;
            }
        }
    }
}

void tilt_west(void) {
    for (int r = 0; r < rows; r++) {
        int write_pos = 0;
        for (int c = 0; c < cols; c++) {
            if (grid[r][c] == '#') {
                write_pos = c + 1;
            } else if (grid[r][c] == 'O') {
                grid[r][c] = '.';
                grid[r][write_pos] = 'O';
                write_pos++;
            }
        }
    }
}

void tilt_east(void) {
    for (int r = 0; r < rows; r++) {
        int write_pos = cols - 1;
        for (int c = cols - 1; c >= 0; c--) {
            if (grid[r][c] == '#') {
                write_pos = c - 1;
            } else if (grid[r][c] == 'O') {
                grid[r][c] = '.';
                grid[r][write_pos] = 'O';
                write_pos--;
            }
        }
    }
}

void spin_cycle(void) {
    tilt_north();
    tilt_west();
    tilt_south();
    tilt_east();
}

int calculate_load(void) {
    int load = 0;
    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            if (grid[r][c] == 'O') {
                load += (rows - r);
            }
        }
    }
    return load;
}

// FNV-1a hash for the grid state
uint64_t hash_grid(void) {
    uint64_t hash = 14695981039346656037ULL;
    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            hash ^= (uint64_t)grid[r][c];
            hash *= 1099511628211ULL;
        }
    }
    return hash;
}

// Store original grid for Part 2
static char original_grid[MAX_ROWS][MAX_COLS];

void save_grid(void) {
    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            original_grid[r][c] = grid[r][c];
        }
    }
}

void restore_grid(void) {
    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            grid[r][c] = original_grid[r][c];
        }
    }
}

int main(void) {
    FILE *f = fopen("../input.txt", "r");
    if (!f) {
        perror("Failed to open input.txt");
        return 1;
    }

    char line[MAX_COLS + 2];
    while (fgets(line, sizeof(line), f)) {
        int len = strlen(line);
        while (len > 0 && (line[len-1] == '\n' || line[len-1] == '\r')) {
            line[--len] = '\0';
        }
        if (len == 0) continue;

        if (cols == 0) cols = len;
        for (int c = 0; c < cols; c++) {
            grid[rows][c] = line[c];
        }
        rows++;
    }
    fclose(f);

    // Save original grid for Part 2
    save_grid();

    // Part 1: tilt north and calculate load
    tilt_north();
    int part1 = calculate_load();
    printf("Part 1: %d\n", part1);

    // Restore grid for Part 2
    restore_grid();

    // Part 2: 1,000,000,000 spin cycles with cycle detection
    long target = 1000000000L;
    state_count = 0;

    // Store initial state
    state_hashes[state_count++] = hash_grid();

    int cycle_start = -1;
    int cycle_len = 0;

    for (long i = 1; i <= target; i++) {
        spin_cycle();
        uint64_t h = hash_grid();

        // Check for cycle
        for (int j = 0; j < state_count; j++) {
            if (state_hashes[j] == h) {
                cycle_start = j;
                cycle_len = state_count - j;
                break;
            }
        }

        if (cycle_start >= 0) {
            // Found a cycle - calculate remaining spins
            long remaining = (target - cycle_start) % cycle_len;
            for (long k = 0; k < remaining; k++) {
                spin_cycle();
            }
            break;
        }

        state_hashes[state_count++] = h;
    }

    int part2 = calculate_load();
    printf("Part 2: %d\n", part2);

    return 0;
}
