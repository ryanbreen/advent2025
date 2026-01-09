#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_WIDTH 700
#define MAX_HEIGHT 200

// Grid: 0 = air, 1 = rock, 2 = sand
static char grid[MAX_HEIGHT][MAX_WIDTH];

int min(int a, int b) { return a < b ? a : b; }
int max(int a, int b) { return a > b ? a : b; }

// Parse a single coordinate pair "x,y"
void parse_coord(const char *str, int *x, int *y) {
    *x = 0;
    *y = 0;
    const char *p = str;

    while (*p >= '0' && *p <= '9') {
        *x = *x * 10 + (*p - '0');
        p++;
    }
    if (*p == ',') p++;
    while (*p >= '0' && *p <= '9') {
        *y = *y * 10 + (*p - '0');
        p++;
    }
}

// Draw a line from (x1,y1) to (x2,y2)
void draw_line(int x1, int y1, int x2, int y2) {
    if (x1 == x2) {
        // Vertical line
        for (int y = min(y1, y2); y <= max(y1, y2); y++) {
            grid[y][x1] = 1;
        }
    } else {
        // Horizontal line
        for (int x = min(x1, x2); x <= max(x1, x2); x++) {
            grid[y1][x] = 1;
        }
    }
}

// Parse input and fill grid with rocks, return max_y
int parse_input(const char *filename) {
    FILE *f = fopen(filename, "r");
    if (!f) {
        perror("Failed to open input file");
        exit(1);
    }

    int max_y = 0;
    char line[4096];

    while (fgets(line, sizeof(line), f)) {
        // Remove trailing newline
        size_t len = strlen(line);
        if (len > 0 && line[len-1] == '\n') line[len-1] = '\0';
        if (strlen(line) == 0) continue;

        // Parse the line: points separated by " -> "
        int prev_x = -1, prev_y = -1;
        char *token = line;
        char *next;

        while (token && *token) {
            // Skip leading whitespace
            while (*token == ' ') token++;
            if (*token == '\0') break;

            // Find the separator " -> "
            next = strstr(token, " -> ");

            int x, y;
            parse_coord(token, &x, &y);

            if (y > max_y) max_y = y;

            if (prev_x >= 0) {
                draw_line(prev_x, prev_y, x, y);
            }

            prev_x = x;
            prev_y = y;

            if (next) {
                token = next + 4;  // Skip " -> "
            } else {
                break;
            }
        }
    }

    fclose(f);
    return max_y;
}

// Simulate one unit of sand falling
// Returns: 1 if sand came to rest, 0 if fell into abyss, -1 if source blocked
int simulate_sand(int max_y, bool has_floor) {
    int x = 500, y = 0;

    // Check if source is already blocked
    if (grid[0][500] != 0) {
        return -1;
    }

    while (true) {
        // Check if sand has fallen into abyss (part 1)
        if (!has_floor && y > max_y) {
            return 0;
        }

        int floor_y = max_y + 2;

        // Try to move down
        if (has_floor && y + 1 >= floor_y) {
            // Hit the floor, come to rest
            grid[y][x] = 2;
            return 1;
        } else if (grid[y + 1][x] == 0) {
            // Move down
            y++;
        } else if (grid[y + 1][x - 1] == 0) {
            // Move down-left
            x--;
            y++;
        } else if (grid[y + 1][x + 1] == 0) {
            // Move down-right
            x++;
            y++;
        } else {
            // Sand comes to rest
            grid[y][x] = 2;
            return 1;
        }
    }
}

// Clear sand from grid (keep rocks)
void clear_sand(void) {
    for (int y = 0; y < MAX_HEIGHT; y++) {
        for (int x = 0; x < MAX_WIDTH; x++) {
            if (grid[y][x] == 2) {
                grid[y][x] = 0;
            }
        }
    }
}

int part1(int max_y) {
    int count = 0;
    while (true) {
        int result = simulate_sand(max_y, false);
        if (result <= 0) break;
        count++;
    }
    return count;
}

int part2(int max_y) {
    int count = 0;
    while (true) {
        int result = simulate_sand(max_y, true);
        if (result <= 0) break;
        count++;
        // Check if source is now blocked
        if (grid[0][500] == 2) break;
    }
    return count;
}

int main(void) {
    // Initialize grid to all air
    memset(grid, 0, sizeof(grid));

    // Parse input
    int max_y = parse_input("../input.txt");

    // Part 1
    int answer1 = part1(max_y);

    // Clear sand for part 2
    clear_sand();

    // Part 2
    int answer2 = part2(max_y);

    printf("Part 1: %d\n", answer1);
    printf("Part 2: %d\n", answer2);

    return 0;
}
