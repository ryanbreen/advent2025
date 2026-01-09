#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_SIZE 128

static int grid[MAX_SIZE][MAX_SIZE];
static int rows = 0;
static int cols = 0;

static bool is_visible(int row, int col) {
    int height = grid[row][col];
    bool visible;

    // Check from left
    visible = true;
    for (int c = 0; c < col; c++) {
        if (grid[row][c] >= height) {
            visible = false;
            break;
        }
    }
    if (visible) return true;

    // Check from right
    visible = true;
    for (int c = col + 1; c < cols; c++) {
        if (grid[row][c] >= height) {
            visible = false;
            break;
        }
    }
    if (visible) return true;

    // Check from top
    visible = true;
    for (int r = 0; r < row; r++) {
        if (grid[r][col] >= height) {
            visible = false;
            break;
        }
    }
    if (visible) return true;

    // Check from bottom
    visible = true;
    for (int r = row + 1; r < rows; r++) {
        if (grid[r][col] >= height) {
            visible = false;
            break;
        }
    }
    return visible;
}

static int scenic_score(int row, int col) {
    int height = grid[row][col];

    // Count trees visible in each direction
    // Left
    int left = 0;
    for (int c = col - 1; c >= 0; c--) {
        left++;
        if (grid[row][c] >= height) break;
    }

    // Right
    int right = 0;
    for (int c = col + 1; c < cols; c++) {
        right++;
        if (grid[row][c] >= height) break;
    }

    // Up
    int up = 0;
    for (int r = row - 1; r >= 0; r--) {
        up++;
        if (grid[r][col] >= height) break;
    }

    // Down
    int down = 0;
    for (int r = row + 1; r < rows; r++) {
        down++;
        if (grid[r][col] >= height) break;
    }

    return left * right * up * down;
}

static int part1(void) {
    int count = 0;
    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            if (is_visible(r, c)) {
                count++;
            }
        }
    }
    return count;
}

static int part2(void) {
    int max_score = 0;
    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            int score = scenic_score(r, c);
            if (score > max_score) {
                max_score = score;
            }
        }
    }
    return max_score;
}

int main(void) {
    FILE *fp = fopen("../input.txt", "r");
    if (!fp) {
        perror("Failed to open input.txt");
        return 1;
    }

    char line[MAX_SIZE + 2];
    while (fgets(line, sizeof(line), fp)) {
        // Remove newline if present
        size_t len = strlen(line);
        while (len > 0 && (line[len-1] == '\n' || line[len-1] == '\r')) {
            line[--len] = '\0';
        }
        if (len == 0) continue;

        if (cols == 0) {
            cols = (int)len;
        }

        for (int c = 0; c < cols; c++) {
            grid[rows][c] = line[c] - '0';
        }
        rows++;
    }
    fclose(fp);

    printf("Part 1: %d\n", part1());
    printf("Part 2: %d\n", part2());

    return 0;
}
