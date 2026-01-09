#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_ROWS 50
#define MAX_COLS 200
#define MAX_QUEUE 50000

typedef struct {
    int r, c, dist;
} QueueItem;

static char grid[MAX_ROWS][MAX_COLS];
static int rows = 0;
static int cols = 0;
static int start_r, start_c;
static int end_r, end_c;

// Directions: up, down, left, right
static const int dr[] = {-1, 1, 0, 0};
static const int dc[] = {0, 0, -1, 1};

void parse_grid(const char *filename) {
    FILE *f = fopen(filename, "r");
    if (!f) {
        perror("Failed to open input file");
        exit(1);
    }

    char line[MAX_COLS + 2];
    rows = 0;

    while (fgets(line, sizeof(line), f)) {
        int len = strlen(line);
        while (len > 0 && (line[len-1] == '\n' || line[len-1] == '\r')) {
            line[--len] = '\0';
        }
        if (len == 0) continue;

        cols = len;
        for (int c = 0; c < len; c++) {
            char ch = line[c];
            if (ch == 'S') {
                start_r = rows;
                start_c = c;
                grid[rows][c] = 'a';
            } else if (ch == 'E') {
                end_r = rows;
                end_c = c;
                grid[rows][c] = 'z';
            } else {
                grid[rows][c] = ch;
            }
        }
        rows++;
    }

    fclose(f);
}

int bfs_single_start(int sr, int sc) {
    static bool visited[MAX_ROWS][MAX_COLS];
    static QueueItem queue[MAX_QUEUE];
    int head = 0, tail = 0;

    memset(visited, false, sizeof(visited));

    queue[tail++] = (QueueItem){sr, sc, 0};
    visited[sr][sc] = true;

    while (head < tail) {
        QueueItem cur = queue[head++];

        if (cur.r == end_r && cur.c == end_c) {
            return cur.dist;
        }

        char current_height = grid[cur.r][cur.c];

        for (int d = 0; d < 4; d++) {
            int nr = cur.r + dr[d];
            int nc = cur.c + dc[d];

            if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && !visited[nr][nc]) {
                char next_height = grid[nr][nc];
                // Can move if destination is at most 1 higher
                if (next_height <= current_height + 1) {
                    visited[nr][nc] = true;
                    queue[tail++] = (QueueItem){nr, nc, cur.dist + 1};
                }
            }
        }
    }

    return -1; // No path found
}

int bfs_multi_start(void) {
    // BFS from all 'a' cells simultaneously
    static bool visited[MAX_ROWS][MAX_COLS];
    static QueueItem queue[MAX_QUEUE];
    int head = 0, tail = 0;

    memset(visited, false, sizeof(visited));

    // Add all 'a' cells as starting points
    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            if (grid[r][c] == 'a') {
                queue[tail++] = (QueueItem){r, c, 0};
                visited[r][c] = true;
            }
        }
    }

    while (head < tail) {
        QueueItem cur = queue[head++];

        if (cur.r == end_r && cur.c == end_c) {
            return cur.dist;
        }

        char current_height = grid[cur.r][cur.c];

        for (int d = 0; d < 4; d++) {
            int nr = cur.r + dr[d];
            int nc = cur.c + dc[d];

            if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && !visited[nr][nc]) {
                char next_height = grid[nr][nc];
                if (next_height <= current_height + 1) {
                    visited[nr][nc] = true;
                    queue[tail++] = (QueueItem){nr, nc, cur.dist + 1};
                }
            }
        }
    }

    return -1;
}

int part1(void) {
    return bfs_single_start(start_r, start_c);
}

int part2(void) {
    return bfs_multi_start();
}

int main(void) {
    parse_grid("../input.txt");

    printf("Part 1: %d\n", part1());
    printf("Part 2: %d\n", part2());

    return 0;
}
