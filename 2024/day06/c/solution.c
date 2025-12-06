#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_SIZE 200

// Direction vectors: up, right, down, left
int dx[] = {-1, 0, 1, 0};
int dy[] = {0, 1, 0, -1};

char grid[MAX_SIZE][MAX_SIZE];
int rows = 0, cols = 0;
int start_x = -1, start_y = -1, start_dir = 0;

// Simulate guard movement
// Returns true if guard gets stuck in a loop, false if they leave the map
// If count_visited is true, also counts distinct positions visited
bool simulate(int obstruction_x, int obstruction_y, bool count_visited, int* visited_count) {
    bool visited[MAX_SIZE][MAX_SIZE];
    bool state_visited[MAX_SIZE][MAX_SIZE][4]; // Track position+direction for loop detection

    memset(visited, false, sizeof(visited));
    memset(state_visited, false, sizeof(state_visited));

    int x = start_x;
    int y = start_y;
    int dir = start_dir;

    visited[x][y] = true;
    int count = 1;

    while (true) {
        // Check if we've seen this state before (loop detection)
        if (state_visited[x][y][dir]) {
            return true; // Loop detected
        }
        state_visited[x][y][dir] = true;

        // Try to move forward
        int next_x = x + dx[dir];
        int next_y = y + dy[dir];

        // Check if we're leaving the map
        if (next_x < 0 || next_x >= rows || next_y < 0 || next_y >= cols) {
            // Guard leaves the map
            if (count_visited) {
                *visited_count = count;
            }
            return false;
        }

        // Check if there's an obstacle
        bool is_obstacle = (grid[next_x][next_y] == '#') ||
                          (next_x == obstruction_x && next_y == obstruction_y);

        if (is_obstacle) {
            // Turn right
            dir = (dir + 1) % 4;
        } else {
            // Move forward
            x = next_x;
            y = next_y;

            if (count_visited && !visited[x][y]) {
                visited[x][y] = true;
                count++;
            }
        }
    }
}

int part1() {
    int visited_count = 0;
    simulate(-1, -1, true, &visited_count);
    return visited_count;
}

int part2() {
    int loop_positions = 0;

    // Try placing an obstruction at each empty position (except start)
    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            // Skip if not empty or is starting position
            if (grid[r][c] != '.' || (r == start_x && c == start_y)) {
                continue;
            }

            // Simulate with obstruction at (r, c)
            int dummy;
            if (simulate(r, c, false, &dummy)) {
                loop_positions++;
            }
        }
    }

    return loop_positions;
}

int main() {
    FILE *file = fopen("../input.txt", "r");
    if (!file) {
        fprintf(stderr, "Error opening input.txt\n");
        return 1;
    }

    // Read the grid
    char line[MAX_SIZE];
    while (fgets(line, sizeof(line), file)) {
        // Remove newline
        size_t len = strlen(line);
        if (len > 0 && line[len-1] == '\n') {
            line[len-1] = '\0';
            len--;
        }
        if (len == 0) continue;

        if (cols == 0) cols = len;

        for (int j = 0; j < len; j++) {
            grid[rows][j] = line[j];

            // Find starting position and direction
            if (line[j] == '^') {
                start_x = rows;
                start_y = j;
                start_dir = 0; // up
                grid[rows][j] = '.'; // Replace with empty space
            } else if (line[j] == '>') {
                start_x = rows;
                start_y = j;
                start_dir = 1; // right
                grid[rows][j] = '.';
            } else if (line[j] == 'v') {
                start_x = rows;
                start_y = j;
                start_dir = 2; // down
                grid[rows][j] = '.';
            } else if (line[j] == '<') {
                start_x = rows;
                start_y = j;
                start_dir = 3; // left
                grid[rows][j] = '.';
            }
        }
        rows++;
    }
    fclose(file);

    printf("Part 1: %d\n", part1());
    printf("Part 2: %d\n", part2());

    return 0;
}
