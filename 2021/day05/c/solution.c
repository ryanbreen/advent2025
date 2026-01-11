#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINES 1000
#define GRID_SIZE 1000

typedef struct {
    int x1, y1, x2, y2;
} Line;

static int sign(int x) {
    if (x > 0) return 1;
    if (x < 0) return -1;
    return 0;
}

static int count_overlaps(Line *lines, int num_lines, int include_diagonals) {
    /* Use a 2D grid to track point counts */
    static int grid[GRID_SIZE][GRID_SIZE];
    memset(grid, 0, sizeof(grid));

    for (int i = 0; i < num_lines; i++) {
        int x1 = lines[i].x1;
        int y1 = lines[i].y1;
        int x2 = lines[i].x2;
        int y2 = lines[i].y2;

        int dx = sign(x2 - x1);
        int dy = sign(y2 - y1);

        /* Skip diagonals for part 1 */
        if (!include_diagonals && dx != 0 && dy != 0) {
            continue;
        }

        int x = x1, y = y1;
        while (1) {
            grid[y][x]++;
            if (x == x2 && y == y2) {
                break;
            }
            x += dx;
            y += dy;
        }
    }

    int count = 0;
    for (int y = 0; y < GRID_SIZE; y++) {
        for (int x = 0; x < GRID_SIZE; x++) {
            if (grid[y][x] >= 2) {
                count++;
            }
        }
    }
    return count;
}

int main(void) {
    FILE *fp = fopen("../input.txt", "r");
    if (!fp) {
        perror("Failed to open input.txt");
        return 1;
    }

    Line lines[MAX_LINES];
    int num_lines = 0;

    char buffer[256];
    while (fgets(buffer, sizeof(buffer), fp) && num_lines < MAX_LINES) {
        int x1, y1, x2, y2;
        if (sscanf(buffer, "%d,%d -> %d,%d", &x1, &y1, &x2, &y2) == 4) {
            lines[num_lines].x1 = x1;
            lines[num_lines].y1 = y1;
            lines[num_lines].x2 = x2;
            lines[num_lines].y2 = y2;
            num_lines++;
        }
    }
    fclose(fp);

    int part1 = count_overlaps(lines, num_lines, 0);
    int part2 = count_overlaps(lines, num_lines, 1);

    printf("Part 1: %d\n", part1);
    printf("Part 2: %d\n", part2);

    return 0;
}
