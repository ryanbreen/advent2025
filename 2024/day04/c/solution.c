#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_ROWS 200
#define MAX_COLS 200

char grid[MAX_ROWS][MAX_COLS];
int rows = 0;
int cols = 0;

// 8 directions: right, left, down, up, and 4 diagonals
int directions[8][2] = {
    {0, 1},   // right
    {0, -1},  // left
    {1, 0},   // down
    {-1, 0},  // up
    {1, 1},   // down-right
    {1, -1},  // down-left
    {-1, 1},  // up-right
    {-1, -1}  // up-left
};

void read_input(const char *filename) {
    FILE *file = fopen(filename, "r");
    if (!file) {
        fprintf(stderr, "Error opening file: %s\n", filename);
        exit(1);
    }

    char line[MAX_COLS];
    rows = 0;
    while (fgets(line, sizeof(line), file)) {
        // Remove newline
        size_t len = strlen(line);
        if (len > 0 && line[len - 1] == '\n') {
            line[len - 1] = '\0';
            len--;
        }
        if (len > 0 && line[len - 1] == '\r') {
            line[len - 1] = '\0';
            len--;
        }

        if (len == 0) continue;

        strcpy(grid[rows], line);
        if (rows == 0) {
            cols = len;
        }
        rows++;
    }

    fclose(file);
}

int part1() {
    const char *target = "XMAS";
    int target_len = 4;
    int count = 0;

    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            // Try each direction from this position
            for (int d = 0; d < 8; d++) {
                int dr = directions[d][0];
                int dc = directions[d][1];

                // Check if XMAS fits in this direction
                bool found = true;
                for (int i = 0; i < target_len; i++) {
                    int nr = r + dr * i;
                    int nc = c + dc * i;

                    if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) {
                        found = false;
                        break;
                    }
                    if (grid[nr][nc] != target[i]) {
                        found = false;
                        break;
                    }
                }

                if (found) {
                    count++;
                }
            }
        }
    }

    return count;
}

int part2() {
    // Find X-MAS patterns: two MAS strings forming an X with A in the center
    // Each diagonal can be MAS or SAM
    int count = 0;

    // Check each possible center point (A must be in the middle)
    for (int r = 1; r < rows - 1; r++) {
        for (int c = 1; c < cols - 1; c++) {
            if (grid[r][c] != 'A') {
                continue;
            }

            // Get the four corners
            char top_left = grid[r - 1][c - 1];
            char top_right = grid[r - 1][c + 1];
            char bottom_left = grid[r + 1][c - 1];
            char bottom_right = grid[r + 1][c + 1];

            // Check diagonal 1 (top-left to bottom-right): MAS or SAM
            bool diag1_ok = (top_left == 'M' && bottom_right == 'S') ||
                           (top_left == 'S' && bottom_right == 'M');

            // Check diagonal 2 (top-right to bottom-left): MAS or SAM
            bool diag2_ok = (top_right == 'M' && bottom_left == 'S') ||
                           (top_right == 'S' && bottom_left == 'M');

            if (diag1_ok && diag2_ok) {
                count++;
            }
        }
    }

    return count;
}

int main() {
    read_input("../input.txt");

    printf("Part 1: %d\n", part1());
    printf("Part 2: %d\n", part2());

    return 0;
}
