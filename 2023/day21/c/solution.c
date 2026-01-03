/*
 * Day 21: Step Counter
 *
 * Part 1: BFS from start position to count cells reachable in exactly 64 steps.
 * Part 2: Grid tiles infinitely. Use quadratic extrapolation based on the
 *         pattern that emerges every 131 steps (grid width).
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>

#define MAX_ROWS 150
#define MAX_COLS 150
#define HASH_SIZE 2000003  /* Prime number for hash table */

/* Grid storage */
static char grid[MAX_ROWS][MAX_COLS];
static int rows = 0, cols = 0;
static int start_r, start_c;

/* Directions: up, down, left, right */
static const int DR[] = {-1, 1, 0, 0};
static const int DC[] = {0, 0, -1, 1};

/* Hash table entry for visited positions */
typedef struct {
    int r, c;
    int dist;
    bool occupied;
} HashEntry;

static HashEntry visited[HASH_SIZE];

/* BFS queue */
typedef struct {
    int r, c, dist;
} QueueItem;

static QueueItem queue[5000000];  /* Large queue for Part 2 BFS */
static int queue_head, queue_tail;

/* Hash function for (r, c) position */
static inline uint32_t hash_pos(int r, int c) {
    /* Shift to handle negative coordinates */
    uint32_t ur = (uint32_t)(r + 500000);
    uint32_t uc = (uint32_t)(c + 500000);
    return ((ur * 1000003u) ^ uc) % HASH_SIZE;
}

/* Clear hash table */
static void hash_clear(void) {
    memset(visited, 0, sizeof(visited));
}

/* Insert into hash table, returns true if newly inserted */
static bool hash_insert(int r, int c, int dist) {
    uint32_t h = hash_pos(r, c);
    while (visited[h].occupied) {
        if (visited[h].r == r && visited[h].c == c) {
            return false;  /* Already exists */
        }
        h = (h + 1) % HASH_SIZE;
    }
    visited[h].r = r;
    visited[h].c = c;
    visited[h].dist = dist;
    visited[h].occupied = true;
    return true;
}

/* Count visited entries matching criteria */
static int64_t count_matching(int max_steps) {
    int target_parity = max_steps % 2;
    int64_t count = 0;
    for (int i = 0; i < HASH_SIZE; i++) {
        if (visited[i].occupied) {
            int d = visited[i].dist;
            if (d <= max_steps && d % 2 == target_parity) {
                count++;
            }
        }
    }
    return count;
}

/* Parse input */
static void parse_input(const char *filename) {
    FILE *f = fopen(filename, "r");
    if (!f) {
        perror("Failed to open input file");
        exit(1);
    }

    char line[MAX_COLS + 2];
    rows = 0;
    while (fgets(line, sizeof(line), f)) {
        int len = (int)strlen(line);
        while (len > 0 && (line[len-1] == '\n' || line[len-1] == '\r')) {
            line[--len] = '\0';
        }
        if (len == 0) continue;

        cols = len;
        memcpy(grid[rows], line, len);
        grid[rows][len] = '\0';

        /* Find starting position */
        for (int c = 0; c < len; c++) {
            if (line[c] == 'S') {
                start_r = rows;
                start_c = c;
            }
        }
        rows++;
    }
    fclose(f);
}

/* Part 1: Count reachable in exactly 'steps' steps on bounded grid */
static int64_t count_reachable(int steps) {
    hash_clear();

    queue_head = queue_tail = 0;
    queue[queue_tail++] = (QueueItem){start_r, start_c, 0};
    hash_insert(start_r, start_c, 0);

    while (queue_head < queue_tail) {
        QueueItem cur = queue[queue_head++];

        if (cur.dist >= steps) continue;

        for (int d = 0; d < 4; d++) {
            int nr = cur.r + DR[d];
            int nc = cur.c + DC[d];

            if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
                if (grid[nr][nc] != '#') {
                    if (hash_insert(nr, nc, cur.dist + 1)) {
                        queue[queue_tail++] = (QueueItem){nr, nc, cur.dist + 1};
                    }
                }
            }
        }
    }

    return count_matching(steps);
}

/* BFS on infinite tiled grid */
static int64_t count_reachable_infinite_bfs(int steps) {
    hash_clear();

    queue_head = queue_tail = 0;
    queue[queue_tail++] = (QueueItem){start_r, start_c, 0};
    hash_insert(start_r, start_c, 0);

    while (queue_head < queue_tail) {
        QueueItem cur = queue[queue_head++];

        if (cur.dist >= steps) continue;

        for (int d = 0; d < 4; d++) {
            int nr = cur.r + DR[d];
            int nc = cur.c + DC[d];

            /* Map to grid coordinates (infinite tiling) */
            int gr = ((nr % rows) + rows) % rows;
            int gc = ((nc % cols) + cols) % cols;

            if (grid[gr][gc] != '#') {
                if (hash_insert(nr, nc, cur.dist + 1)) {
                    queue[queue_tail++] = (QueueItem){nr, nc, cur.dist + 1};
                }
            }
        }
    }

    return count_matching(steps);
}

/* Part 2: Count reachable on infinite grid using quadratic extrapolation */
static int64_t count_reachable_infinite(int64_t steps) {
    int size = rows;  /* Grid should be square */
    int half = size / 2;

    /* For small step counts, use direct BFS */
    if (steps <= size * 2) {
        return count_reachable_infinite_bfs((int)steps);
    }

    /* The number of full grid widths we travel */
    int64_t n = (steps - half) / size;

    /* Calculate reachable counts for n=0, 1, 2 */
    int64_t y0 = count_reachable_infinite_bfs(half);
    int64_t y1 = count_reachable_infinite_bfs(half + size);
    int64_t y2 = count_reachable_infinite_bfs(half + 2 * size);

    /* Solve for a, b, c using finite differences */
    /* f(x) = ax^2 + bx + c */
    /* y0 = f(0) = c */
    /* y1 = f(1) = a + b + c */
    /* y2 = f(2) = 4a + 2b + c */
    /* Second difference: y2 - 2*y1 + y0 = 2a */
    int64_t a = (y2 - 2 * y1 + y0) / 2;
    int64_t b = y1 - y0 - a;
    int64_t c = y0;

    return a * n * n + b * n + c;
}

int main(void) {
    parse_input("../input.txt");

    int64_t part1 = count_reachable(64);
    printf("Part 1: %lld\n", (long long)part1);

    int64_t part2 = count_reachable_infinite(26501365LL);
    printf("Part 2: %lld\n", (long long)part2);

    return 0;
}
