#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_ROWS 256
#define MAX_COLS 256
#define QUEUE_SIZE (MAX_ROWS * MAX_COLS)

/* Direction indices for DR/DC arrays */
#define DIR_NORTH 0
#define DIR_SOUTH 1
#define DIR_WEST  2
#define DIR_EAST  3

/* Connection bitmasks for pipe types */
#define CONN_NORTH (1 << DIR_NORTH)
#define CONN_SOUTH (1 << DIR_SOUTH)
#define CONN_WEST  (1 << DIR_WEST)
#define CONN_EAST  (1 << DIR_EAST)

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
 * Returns a bitmask using CONN_* constants.
 */
static int get_pipe_connections(char ch) {
    switch (ch) {
        case '|': return CONN_NORTH | CONN_SOUTH;
        case '-': return CONN_WEST  | CONN_EAST;
        case 'L': return CONN_NORTH | CONN_EAST;
        case 'J': return CONN_NORTH | CONN_WEST;
        case '7': return CONN_SOUTH | CONN_WEST;
        case 'F': return CONN_SOUTH | CONN_EAST;
        default: return 0;
    }
}

/* Get the opposite direction index.
 * Uses XOR trick: N(0)^1=S(1), S(1)^1=N(0), W(2)^1=E(3), E(3)^1=W(2)
 */
static int opposite_dir(int dir) {
    return dir ^ 1;
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

/* Determine what pipe S actually represents based on its neighbors */
static char determine_start_pipe(Pos start) {
    int connections = 0;

    for (int dir = 0; dir < 4; dir++) {
        int nr = start.r + DR[dir];
        int nc = start.c + DC[dir];

        if (valid_pos(nr, nc)) {
            char adj = grid[nr][nc];
            int opp = opposite_dir(dir);
            if (connects_in_dir(adj, opp)) {
                connections |= (1 << dir);
            }
        }
    }

    /* Match connections to a pipe type */
    switch (connections) {
        case CONN_NORTH | CONN_SOUTH: return '|';
        case CONN_WEST  | CONN_EAST:  return '-';
        case CONN_NORTH | CONN_EAST:  return 'L';
        case CONN_NORTH | CONN_WEST:  return 'J';
        case CONN_SOUTH | CONN_WEST:  return '7';
        case CONN_SOUTH | CONN_EAST:  return 'F';
        default: return 'S';
    }
}

/* BFS to find the loop and distances.
 * Replaces S with its actual pipe type before traversal to unify handling.
 */
static int find_loop(Pos start) {
    /* Replace S with its actual pipe type to eliminate special-case handling */
    grid[start.r][start.c] = determine_start_pipe(start);

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
            /* Check if current pipe connects in this direction */
            if (!connects_in_dir(ch, dir)) {
                continue;
            }

            int nr = pos.r + DR[dir];
            int nc = pos.c + DC[dir];

            if (!valid_pos(nr, nc) || distances[nr][nc] != -1) {
                continue;
            }

            char adj = grid[nr][nc];

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

/* Count enclosed tiles using ray casting.
 * Note: S has already been replaced with its actual pipe type by find_loop().
 */
static int count_enclosed(void) {
    int enclosed = 0;

    for (int r = 0; r < rows; r++) {
        bool inside = false;
        for (int c = 0; c < cols; c++) {
            if (in_loop[r][c]) {
                /* Toggle inside when crossing pipes with north connection (|, L, J) */
                if (connects_in_dir(grid[r][c], DIR_NORTH)) {
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

    /* +2 accounts for newline character and null terminator */
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
    int part2 = count_enclosed();

    printf("Part 1: %d\n", part1);
    printf("Part 2: %d\n", part2);

    return 0;
}
