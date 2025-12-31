/*
 * Day 17: Clumsy Crucible
 * Dijkstra's shortest path with movement constraints.
 *
 * State: (row, col, direction, consecutive_steps)
 * Part 1: max 3 consecutive blocks in same direction
 * Part 2: min 4, max 10 consecutive blocks (ultra crucible)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define MAX_SIZE 150
#define MAX_CONSEC 11  /* 0-10 */
#define NUM_DIRS 4

/* Priority queue node */
typedef struct {
    int heat;
    int row;
    int col;
    int dir;      /* 0=right, 1=down, 2=left, 3=up, -1=start */
    int consec;
} State;

/* Binary heap for priority queue */
typedef struct {
    State *data;
    int size;
    int capacity;
} PriorityQueue;

/* Direction deltas: right, down, left, up */
static const int dr[4] = {0, 1, 0, -1};
static const int dc[4] = {1, 0, -1, 0};

/* Grid data */
static int grid[MAX_SIZE][MAX_SIZE];
static int rows, cols;

/* Visited array: [row][col][dir][consec] */
/* dir can be 0-3 (or we handle -1 specially) */
static char visited[MAX_SIZE][MAX_SIZE][NUM_DIRS][MAX_CONSEC];

/* Priority queue operations */
static PriorityQueue *pq_create(int capacity) {
    PriorityQueue *pq = malloc(sizeof(PriorityQueue));
    pq->data = malloc(capacity * sizeof(State));
    pq->size = 0;
    pq->capacity = capacity;
    return pq;
}

static void pq_free(PriorityQueue *pq) {
    free(pq->data);
    free(pq);
}

static void pq_push(PriorityQueue *pq, State s) {
    if (pq->size >= pq->capacity) {
        pq->capacity *= 2;
        pq->data = realloc(pq->data, pq->capacity * sizeof(State));
    }

    int i = pq->size++;
    pq->data[i] = s;

    /* Bubble up */
    while (i > 0) {
        int parent = (i - 1) / 2;
        if (pq->data[parent].heat <= pq->data[i].heat) break;
        State tmp = pq->data[parent];
        pq->data[parent] = pq->data[i];
        pq->data[i] = tmp;
        i = parent;
    }
}

static State pq_pop(PriorityQueue *pq) {
    State result = pq->data[0];
    pq->data[0] = pq->data[--pq->size];

    /* Bubble down */
    int i = 0;
    while (1) {
        int left = 2 * i + 1;
        int right = 2 * i + 2;
        int smallest = i;

        if (left < pq->size && pq->data[left].heat < pq->data[smallest].heat)
            smallest = left;
        if (right < pq->size && pq->data[right].heat < pq->data[smallest].heat)
            smallest = right;

        if (smallest == i) break;

        State tmp = pq->data[i];
        pq->data[i] = pq->data[smallest];
        pq->data[smallest] = tmp;
        i = smallest;
    }

    return result;
}

static int pq_empty(PriorityQueue *pq) {
    return pq->size == 0;
}

/* Dijkstra's algorithm with movement constraints */
static int dijkstra(int min_straight, int max_straight) {
    memset(visited, 0, sizeof(visited));

    PriorityQueue *pq = pq_create(100000);

    /* Start at (0,0) with no direction yet (dir=-1, consec=0) */
    State start = {0, 0, 0, -1, 0};
    pq_push(pq, start);

    while (!pq_empty(pq)) {
        State cur = pq_pop(pq);

        /* Check if we reached the goal */
        if (cur.row == rows - 1 && cur.col == cols - 1) {
            if (min_straight == 0 || cur.consec >= min_straight) {
                int result = cur.heat;
                pq_free(pq);
                return result;
            }
        }

        /* Check if already visited (skip dir=-1 case for visited check) */
        if (cur.dir >= 0 && visited[cur.row][cur.col][cur.dir][cur.consec]) {
            continue;
        }
        if (cur.dir >= 0) {
            visited[cur.row][cur.col][cur.dir][cur.consec] = 1;
        }

        /* Try all four directions */
        for (int nd = 0; nd < 4; nd++) {
            /* Can't reverse direction */
            if (cur.dir != -1 && nd == (cur.dir + 2) % 4) {
                continue;
            }

            int nr = cur.row + dr[nd];
            int nc = cur.col + dc[nd];

            /* Bounds check */
            if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) {
                continue;
            }

            int new_consec;
            if (nd == cur.dir) {
                /* Continuing in same direction */
                new_consec = cur.consec + 1;
                if (new_consec > max_straight) {
                    continue;
                }
            } else {
                /* Turning - must have gone min_straight in previous direction first */
                if (cur.dir != -1 && cur.consec < min_straight) {
                    continue;
                }
                new_consec = 1;
            }

            /* Skip if already visited */
            if (visited[nr][nc][nd][new_consec]) {
                continue;
            }

            State next = {
                cur.heat + grid[nr][nc],
                nr, nc, nd, new_consec
            };
            pq_push(pq, next);
        }
    }

    pq_free(pq);
    return -1; /* No path found */
}

static void parse_input(const char *filename) {
    FILE *f = fopen(filename, "r");
    if (!f) {
        perror("Failed to open input file");
        exit(1);
    }

    char line[MAX_SIZE + 2];
    rows = 0;

    while (fgets(line, sizeof(line), f)) {
        int len = strlen(line);
        while (len > 0 && (line[len-1] == '\n' || line[len-1] == '\r')) {
            line[--len] = '\0';
        }
        if (len == 0) continue;

        cols = len;
        for (int c = 0; c < len; c++) {
            grid[rows][c] = line[c] - '0';
        }
        rows++;
    }

    fclose(f);
}

int main(void) {
    parse_input("../input.txt");

    int part1 = dijkstra(0, 3);
    int part2 = dijkstra(4, 10);

    printf("Part 1: %d\n", part1);
    printf("Part 2: %d\n", part2);

    return 0;
}
