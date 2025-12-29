#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_ROWS 256
#define MAX_COLS 256
#define QUEUE_SIZE (MAX_ROWS * MAX_COLS)

static char grid[MAX_ROWS][MAX_COLS];
static int distances[MAX_ROWS][MAX_COLS];
static bool in_loop[MAX_ROWS][MAX_COLS];
static int rows, cols;

typedef struct {
    int r, c;
} Pos;

static Pos queue[QUEUE_SIZE];
static int queue_head, queue_tail;

/* Direction offsets: N, S, W, E */
static const int DR[] = {-1, 1, 0, 0};
static const int DC[] = {0, 0, -1, 1};

/* Get the directions a pipe connects to.
 * Returns a bitmask: bit 0 = N, bit 1 = S, bit 2 = W, bit 3 = E
 */
static int get_pipe_connections(char ch) {
    switch (ch) {
        case '|': return (1 << 0) | (1 << 1);  /* N, S */
        case '-': return (1 << 2) | (1 << 3);  /* W, E */
        case 'L': return (1 << 0) | (1 << 3);  /* N, E */
        case 'J': return (1 << 0) | (1 << 2);  /* N, W */
        case '7': return (1 << 1) | (1 << 2);  /* S, W */
        case 'F': return (1 << 1) | (1 << 3);  /* S, E */
        default: return 0;
    }
}

/* Get the opposite direction index */
static int opposite_dir(int dir) {
    switch (dir) {
        case 0: return 1;  /* N -> S */
        case 1: return 0;  /* S -> N */
        case 2: return 3;  /* W -> E */
        case 3: return 2;  /* E -> W */
        default: return -1;
    }
}

/* Check if a pipe connects in a given direction */
static bool connects_in_dir(char ch, int dir) {
    return (get_pipe_connections(ch) & (1 << dir)) != 0;
}

/* Check if position is valid */
static bool valid_pos(int r, int c) {
    return r >= 0 && r < rows && c >= 0 && c < cols;
}

/* Find start position */
static Pos find_start(void) {
    Pos p = {-1, -1};
    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            if (grid[r][c] == 'S') {
                p.r = r;
                p.c = c;
                return p;
            }
        }
    }
    return p;
}

/* Determine what pipe S actually represents */
static char determine_start_pipe(Pos start) {
    int connections = 0;

    for (int dir = 0; dir < 4; dir++) {
        int nr = start.r + DR[dir];
        int nc = start.c + DC[dir];

        if (valid_pos(nr, nc) && in_loop[nr][nc]) {
            char adj = grid[nr][nc];
            int opp = opposite_dir(dir);
            if (connects_in_dir(adj, opp)) {
                connections |= (1 << dir);
            }
        }
    }

    /* Match connections to a pipe type */
    switch (connections) {
        case (1 << 0) | (1 << 1): return '|';
        case (1 << 2) | (1 << 3): return '-';
        case (1 << 0) | (1 << 3): return 'L';
        case (1 << 0) | (1 << 2): return 'J';
        case (1 << 1) | (1 << 2): return '7';
        case (1 << 1) | (1 << 3): return 'F';
        default: return 'S';
    }
}

/* BFS to find the loop and distances */
static int find_loop(Pos start) {
    memset(distances, -1, sizeof(distances));
    memset(in_loop, 0, sizeof(in_loop));

    queue_head = 0;
    queue_tail = 0;

    queue[queue_tail++] = start;
    distances[start.r][start.c] = 0;
    in_loop[start.r][start.c] = true;

    int max_dist = 0;

    while (queue_head < queue_tail) {
        Pos pos = queue[queue_head++];
        char ch = grid[pos.r][pos.c];

        for (int dir = 0; dir < 4; dir++) {
            int nr = pos.r + DR[dir];
            int nc = pos.c + DC[dir];

            if (!valid_pos(nr, nc) || distances[nr][nc] != -1) {
                continue;
            }

            char adj = grid[nr][nc];
            if (get_pipe_connections(adj) == 0) {
                continue;
            }

            /* Check if current position connects in this direction */
            bool curr_connects;
            if (ch == 'S') {
                /* S connects to any adjacent pipe that connects back */
                int opp = opposite_dir(dir);
                curr_connects = connects_in_dir(adj, opp);
            } else {
                curr_connects = connects_in_dir(ch, dir);
            }

            if (!curr_connects) {
                continue;
            }

            /* Check if adjacent pipe connects back */
            int opp = opposite_dir(dir);
            if (!connects_in_dir(adj, opp)) {
                continue;
            }

            distances[nr][nc] = distances[pos.r][pos.c] + 1;
            in_loop[nr][nc] = true;
            if (distances[nr][nc] > max_dist) {
                max_dist = distances[nr][nc];
            }

            Pos next = {nr, nc};
            queue[queue_tail++] = next;
        }
    }

    return max_dist;
}

/* Count enclosed tiles using ray casting */
static int count_enclosed(Pos start) {
    /* Replace S with its actual pipe type */
    char start_pipe = determine_start_pipe(start);
    grid[start.r][start.c] = start_pipe;

    int enclosed = 0;

    for (int r = 0; r < rows; r++) {
        bool inside = false;
        for (int c = 0; c < cols; c++) {
            if (in_loop[r][c]) {
                char ch = grid[r][c];
                /* Count pipes with north connection: |, L, J */
                if (ch == '|' || ch == 'L' || ch == 'J') {
                    inside = !inside;
                }
            } else {
                if (inside) {
                    enclosed++;
                }
            }
        }
    }

    return enclosed;
}

int main(void) {
    FILE *fp = fopen("../input.txt", "r");
    if (!fp) {
        perror("Failed to open input file");
        return 1;
    }

    rows = 0;
    cols = 0;

    char line[MAX_COLS + 2];
    while (fgets(line, sizeof(line), fp) && rows < MAX_ROWS) {
        int len = strlen(line);
        while (len > 0 && (line[len-1] == '\n' || line[len-1] == '\r')) {
            line[--len] = '\0';
        }
        if (len == 0) continue;

        if (cols == 0) {
            cols = len;
        }

        memcpy(grid[rows], line, len);
        rows++;
    }
    fclose(fp);

    Pos start = find_start();

    int part1 = find_loop(start);
    int part2 = count_enclosed(start);

    printf("Part 1: %d\n", part1);
    printf("Part 2: %d\n", part2);

    return 0;
}
