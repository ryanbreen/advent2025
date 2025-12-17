// Day 16: Reindeer Maze - C implementation for ARM64 macOS
// Implements bidirectional Dijkstra's algorithm

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define MAX_SIZE 150
#define MAX_STATES 90000

typedef struct {
    int cost;
    int x, y, dir;
} State;

typedef struct {
    State *data;
    int size;
    int capacity;
} PriorityQueue;

int dx[] = {1, 0, -1, 0};
int dy[] = {0, 1, 0, -1};

PriorityQueue* pq_create(int capacity) {
    PriorityQueue *pq = malloc(sizeof(PriorityQueue));
    pq->data = malloc(capacity * sizeof(State));
    pq->size = 0;
    pq->capacity = capacity;
    return pq;
}

void pq_free(PriorityQueue *pq) {
    free(pq->data);
    free(pq);
}

void pq_swap(State *a, State *b) {
    State temp = *a;
    *a = *b;
    *b = temp;
}

void pq_push(PriorityQueue *pq, int cost, int x, int y, int dir) {
    if (pq->size >= pq->capacity) return;

    pq->data[pq->size] = (State){cost, x, y, dir};
    int i = pq->size++;

    while (i > 0) {
        int parent = (i - 1) / 2;
        if (pq->data[i].cost >= pq->data[parent].cost) break;
        pq_swap(&pq->data[i], &pq->data[parent]);
        i = parent;
    }
}

State pq_pop(PriorityQueue *pq) {
    State result = pq->data[0];
    pq->data[0] = pq->data[--pq->size];

    int i = 0;
    while (1) {
        int left = 2 * i + 1;
        int right = 2 * i + 2;
        int smallest = i;

        if (left < pq->size && pq->data[left].cost < pq->data[smallest].cost)
            smallest = left;
        if (right < pq->size && pq->data[right].cost < pq->data[smallest].cost)
            smallest = right;

        if (smallest == i) break;
        pq_swap(&pq->data[i], &pq->data[smallest]);
        i = smallest;
    }

    return result;
}

int get_index(int x, int y, int dir) {
    return (y * MAX_SIZE + x) * 4 + dir;
}

void dijkstra_solve(char *grid, int rows, int cols, int start_x, int start_y,
                   int end_x, int end_y, long long *part1, long long *part2) {

    int *dist_fwd = calloc(MAX_STATES, sizeof(int));
    int *dist_bwd = calloc(MAX_STATES, sizeof(int));
    char *visited_fwd = calloc(MAX_STATES, sizeof(char));
    char *visited_bwd = calloc(MAX_STATES, sizeof(char));

    for (int i = 0; i < MAX_STATES; i++) {
        dist_fwd[i] = INT_MAX;
        dist_bwd[i] = INT_MAX;
    }

    // Forward Dijkstra
    PriorityQueue *pq = pq_create(200000);
    pq_push(pq, 0, start_x, start_y, 0);

    while (pq->size > 0) {
        State s = pq_pop(pq);
        int idx = get_index(s.x, s.y, s.dir);

        if (visited_fwd[idx]) continue;
        visited_fwd[idx] = 1;
        dist_fwd[idx] = s.cost;

        // Move forward
        int nx = s.x + dx[s.dir];
        int ny = s.y + dy[s.dir];
        if (nx >= 0 && nx < cols && ny >= 0 && ny < rows &&
            grid[ny * MAX_SIZE + nx] != '#') {
            int nidx = get_index(nx, ny, s.dir);
            if (!visited_fwd[nidx]) {
                pq_push(pq, s.cost + 1, nx, ny, s.dir);
            }
        }

        // Turn left
        int ndir = (s.dir + 3) % 4;
        int nidx = get_index(s.x, s.y, ndir);
        if (!visited_fwd[nidx]) {
            pq_push(pq, s.cost + 1000, s.x, s.y, ndir);
        }

        // Turn right
        ndir = (s.dir + 1) % 4;
        nidx = get_index(s.x, s.y, ndir);
        if (!visited_fwd[nidx]) {
            pq_push(pq, s.cost + 1000, s.x, s.y, ndir);
        }
    }

    // Find best score
    int best_score = INT_MAX;
    for (int d = 0; d < 4; d++) {
        int idx = get_index(end_x, end_y, d);
        if (dist_fwd[idx] < best_score) {
            best_score = dist_fwd[idx];
        }
    }
    *part1 = best_score;

    // Backward Dijkstra
    pq->size = 0;
    for (int d = 0; d < 4; d++) {
        pq_push(pq, 0, end_x, end_y, d);
    }

    while (pq->size > 0) {
        State s = pq_pop(pq);
        int idx = get_index(s.x, s.y, s.dir);

        if (visited_bwd[idx]) continue;
        visited_bwd[idx] = 1;
        dist_bwd[idx] = s.cost;

        // Move backward
        int px = s.x - dx[s.dir];
        int py = s.y - dy[s.dir];
        if (px >= 0 && px < cols && py >= 0 && py < rows &&
            grid[py * MAX_SIZE + px] != '#') {
            int pidx = get_index(px, py, s.dir);
            if (!visited_bwd[pidx]) {
                pq_push(pq, s.cost + 1, px, py, s.dir);
            }
        }

        // Turn left
        int ndir = (s.dir + 3) % 4;
        int nidx = get_index(s.x, s.y, ndir);
        if (!visited_bwd[nidx]) {
            pq_push(pq, s.cost + 1000, s.x, s.y, ndir);
        }

        // Turn right
        ndir = (s.dir + 1) % 4;
        nidx = get_index(s.x, s.y, ndir);
        if (!visited_bwd[nidx]) {
            pq_push(pq, s.cost + 1000, s.x, s.y, ndir);
        }
    }

    // Count tiles on optimal paths
    char *on_path = calloc(MAX_SIZE * MAX_SIZE, sizeof(char));
    int count = 0;

    for (int y = 0; y < rows; y++) {
        for (int x = 0; x < cols; x++) {
            if (grid[y * MAX_SIZE + x] == '#') continue;
            if (on_path[y * MAX_SIZE + x]) continue;

            for (int d = 0; d < 4; d++) {
                int idx = get_index(x, y, d);
                if (dist_fwd[idx] + dist_bwd[idx] == best_score) {
                    on_path[y * MAX_SIZE + x] = 1;
                    count++;
                    break;
                }
            }
        }
    }
    *part2 = count;

    pq_free(pq);
    free(dist_fwd);
    free(dist_bwd);
    free(visited_fwd);
    free(visited_bwd);
    free(on_path);
}

int main() {
    FILE *f = fopen("../input.txt", "r");
    if (!f) return 1;

    char grid[22500];
    memset(grid, 0, sizeof(grid));

    char buffer[256];
    int rows = 0, cols = 0;
    int start_x = -1, start_y = -1, end_x = -1, end_y = -1;

    while (fgets(buffer, sizeof(buffer), f)) {
        int len = strlen(buffer);
        if (len > 0 && buffer[len-1] == '\n') {
            buffer[--len] = '\0';
        }
        if (len == 0) continue;

        if (cols == 0) cols = len;

        for (int x = 0; x < len; x++) {
            char c = buffer[x];
            if (c == 'S') {
                start_x = x;
                start_y = rows;
                c = '.';
            } else if (c == 'E') {
                end_x = x;
                end_y = rows;
                c = '.';
            }
            grid[rows * 150 + x] = c;
        }
        rows++;
    }
    fclose(f);

    long long part1 = 0, part2 = 0;
    dijkstra_solve(grid, rows, cols, start_x, start_y, end_x, end_y, &part1, &part2);

    printf("Part 1: %lld\n", part1);
    printf("Part 2: %lld\n", part2);

    return 0;
}
