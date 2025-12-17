/*
 * Day 16: Reindeer Maze - Weighted shortest path with turn costs
 *
 * Implements bidirectional Dijkstra to find:
 * - Part 1: Minimum cost path from start to end
 * - Part 2: Count of all tiles on any optimal path
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <stdbool.h>

#define MAX_SIZE 200
#define INF INT_MAX

// Directions: 0=East, 1=South, 2=West, 3=North
static const int DX[] = {1, 0, -1, 0};
static const int DY[] = {0, 1, 0, -1};

typedef struct {
    int x, y, dir;
} Position;

typedef struct {
    int cost;
    Position pos;
} HeapNode;

typedef struct {
    HeapNode *data;
    int size;
    int capacity;
} MinHeap;

typedef struct {
    int cost;
    bool visited;
} State;

// Grid representation
static char grid[MAX_SIZE][MAX_SIZE];
static int grid_height = 0;
static int grid_width = 0;

// Distance maps for forward and backward Dijkstra
static State dist_forward[MAX_SIZE][MAX_SIZE][4];
static State dist_backward[MAX_SIZE][MAX_SIZE][4];

// Min-heap operations
MinHeap* heap_create(int capacity) {
    MinHeap *heap = malloc(sizeof(MinHeap));
    heap->data = malloc(sizeof(HeapNode) * capacity);
    heap->size = 0;
    heap->capacity = capacity;
    return heap;
}

void heap_destroy(MinHeap *heap) {
    free(heap->data);
    free(heap);
}

void heap_swap(HeapNode *a, HeapNode *b) {
    HeapNode temp = *a;
    *a = *b;
    *b = temp;
}

void heap_push(MinHeap *heap, int cost, int x, int y, int dir) {
    if (heap->size >= heap->capacity) {
        heap->capacity *= 2;
        heap->data = realloc(heap->data, sizeof(HeapNode) * heap->capacity);
    }

    int idx = heap->size++;
    heap->data[idx].cost = cost;
    heap->data[idx].pos.x = x;
    heap->data[idx].pos.y = y;
    heap->data[idx].pos.dir = dir;

    // Bubble up
    while (idx > 0) {
        int parent = (idx - 1) / 2;
        if (heap->data[parent].cost <= heap->data[idx].cost)
            break;
        heap_swap(&heap->data[parent], &heap->data[idx]);
        idx = parent;
    }
}

HeapNode heap_pop(MinHeap *heap) {
    HeapNode result = heap->data[0];
    heap->data[0] = heap->data[--heap->size];

    // Bubble down
    int idx = 0;
    while (true) {
        int left = 2 * idx + 1;
        int right = 2 * idx + 2;
        int smallest = idx;

        if (left < heap->size && heap->data[left].cost < heap->data[smallest].cost)
            smallest = left;
        if (right < heap->size && heap->data[right].cost < heap->data[smallest].cost)
            smallest = right;

        if (smallest == idx)
            break;

        heap_swap(&heap->data[idx], &heap->data[smallest]);
        idx = smallest;
    }

    return result;
}

bool heap_empty(MinHeap *heap) {
    return heap->size == 0;
}

// Parse input and find start/end
void parse_input(const char *filename, int *start_x, int *start_y, int *end_x, int *end_y) {
    FILE *fp = fopen(filename, "r");
    if (!fp) {
        fprintf(stderr, "Cannot open file: %s\n", filename);
        exit(1);
    }

    grid_height = 0;
    grid_width = 0;

    while (fgets(grid[grid_height], MAX_SIZE, fp)) {
        // Remove newline
        size_t len = strlen(grid[grid_height]);
        if (len > 0 && grid[grid_height][len - 1] == '\n') {
            grid[grid_height][len - 1] = '\0';
            len--;
        }

        if (len == 0) break;

        if (grid_width == 0) {
            grid_width = (int)len;
        }

        // Find start and end
        for (int x = 0; x < (int)len; x++) {
            if (grid[grid_height][x] == 'S') {
                *start_x = x;
                *start_y = grid_height;
            } else if (grid[grid_height][x] == 'E') {
                *end_x = x;
                *end_y = grid_height;
            }
        }

        grid_height++;
    }

    fclose(fp);
}

// Initialize distance maps
void init_dist_maps() {
    for (int y = 0; y < grid_height; y++) {
        for (int x = 0; x < grid_width; x++) {
            for (int d = 0; d < 4; d++) {
                dist_forward[y][x][d].cost = INF;
                dist_forward[y][x][d].visited = false;
                dist_backward[y][x][d].cost = INF;
                dist_backward[y][x][d].visited = false;
            }
        }
    }
}

// Forward Dijkstra from start
void dijkstra_forward(int start_x, int start_y) {
    MinHeap *heap = heap_create(10000);
    heap_push(heap, 0, start_x, start_y, 0); // Start facing East

    while (!heap_empty(heap)) {
        HeapNode node = heap_pop(heap);
        int cost = node.cost;
        int x = node.pos.x;
        int y = node.pos.y;
        int d = node.pos.dir;

        if (dist_forward[y][x][d].visited)
            continue;

        dist_forward[y][x][d].cost = cost;
        dist_forward[y][x][d].visited = true;

        // Move forward
        int nx = x + DX[d];
        int ny = y + DY[d];
        if (nx >= 0 && nx < grid_width && ny >= 0 && ny < grid_height &&
            grid[ny][nx] != '#' && !dist_forward[ny][nx][d].visited) {
            heap_push(heap, cost + 1, nx, ny, d);
        }

        // Turn left
        int left = (d + 3) % 4;
        if (!dist_forward[y][x][left].visited) {
            heap_push(heap, cost + 1000, x, y, left);
        }

        // Turn right
        int right = (d + 1) % 4;
        if (!dist_forward[y][x][right].visited) {
            heap_push(heap, cost + 1000, x, y, right);
        }
    }

    heap_destroy(heap);
}

// Backward Dijkstra from end
void dijkstra_backward(int end_x, int end_y) {
    MinHeap *heap = heap_create(10000);

    // Can arrive at end from any direction
    for (int d = 0; d < 4; d++) {
        heap_push(heap, 0, end_x, end_y, d);
    }

    while (!heap_empty(heap)) {
        HeapNode node = heap_pop(heap);
        int cost = node.cost;
        int x = node.pos.x;
        int y = node.pos.y;
        int d = node.pos.dir;

        if (dist_backward[y][x][d].visited)
            continue;

        dist_backward[y][x][d].cost = cost;
        dist_backward[y][x][d].visited = true;

        // Reverse of move forward: come from behind
        int px = x - DX[d];
        int py = y - DY[d];
        if (px >= 0 && px < grid_width && py >= 0 && py < grid_height &&
            grid[py][px] != '#' && !dist_backward[py][px][d].visited) {
            heap_push(heap, cost + 1, px, py, d);
        }

        // Reverse of turn: same position, different direction
        int left = (d + 3) % 4;
        if (!dist_backward[y][x][left].visited) {
            heap_push(heap, cost + 1000, x, y, left);
        }

        int right = (d + 1) % 4;
        if (!dist_backward[y][x][right].visited) {
            heap_push(heap, cost + 1000, x, y, right);
        }
    }

    heap_destroy(heap);
}

// Part 1: Find minimum cost path
int part1(int end_x, int end_y) {
    int min_cost = INF;
    for (int d = 0; d < 4; d++) {
        if (dist_forward[end_y][end_x][d].cost < min_cost) {
            min_cost = dist_forward[end_y][end_x][d].cost;
        }
    }
    return min_cost;
}

// Part 2: Count tiles on any optimal path
int part2(int best_score) {
    static bool on_path[MAX_SIZE][MAX_SIZE];
    memset(on_path, 0, sizeof(on_path));

    int count = 0;
    for (int y = 0; y < grid_height; y++) {
        for (int x = 0; x < grid_width; x++) {
            if (grid[y][x] == '#')
                continue;

            // Check if this tile is on any optimal path
            for (int d = 0; d < 4; d++) {
                int from_start = dist_forward[y][x][d].cost;
                int to_end = dist_backward[y][x][d].cost;

                if (from_start != INF && to_end != INF &&
                    from_start + to_end == best_score) {
                    if (!on_path[y][x]) {
                        on_path[y][x] = true;
                        count++;
                    }
                    break;
                }
            }
        }
    }

    return count;
}

int main() {
    int start_x, start_y, end_x, end_y;

    parse_input("../input.txt", &start_x, &start_y, &end_x, &end_y);
    init_dist_maps();

    // Run forward Dijkstra from start
    dijkstra_forward(start_x, start_y);

    // Part 1
    int answer1 = part1(end_x, end_y);
    printf("Part 1: %d\n", answer1);

    // Run backward Dijkstra from end
    dijkstra_backward(end_x, end_y);

    // Part 2
    int answer2 = part2(answer1);
    printf("Part 2: %d\n", answer2);

    return 0;
}
