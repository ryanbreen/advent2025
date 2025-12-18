#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define GRID_SIZE 71
#define MAX_POSITIONS 4096

typedef struct {
    int x, y;
} Position;

typedef struct {
    int x, y, steps;
} QueueNode;

static Position positions[MAX_POSITIONS];
static int num_positions = 0;

/* Grid for marking corrupted cells */
static bool corrupted[GRID_SIZE][GRID_SIZE];

/* Visited array for BFS */
static bool visited[GRID_SIZE][GRID_SIZE];

/* BFS queue */
static QueueNode queue[GRID_SIZE * GRID_SIZE];

/* Direction offsets for 4-directional movement */
static const int dx[] = {0, 0, 1, -1};
static const int dy[] = {1, -1, 0, 0};

void parse_input(const char *filename) {
    FILE *f = fopen(filename, "r");
    if (!f) {
        perror("Failed to open input file");
        exit(1);
    }

    int x, y;
    while (fscanf(f, "%d,%d", &x, &y) == 2) {
        positions[num_positions].x = x;
        positions[num_positions].y = y;
        num_positions++;
    }

    fclose(f);
}

void mark_corrupted(int num_bytes) {
    memset(corrupted, 0, sizeof(corrupted));
    for (int i = 0; i < num_bytes && i < num_positions; i++) {
        corrupted[positions[i].y][positions[i].x] = true;
    }
}

int bfs(void) {
    /* BFS from (0,0) to (GRID_SIZE-1, GRID_SIZE-1) */
    int start_x = 0, start_y = 0;
    int goal_x = GRID_SIZE - 1, goal_y = GRID_SIZE - 1;

    if (corrupted[start_y][start_x] || corrupted[goal_y][goal_x]) {
        return -1;
    }

    memset(visited, 0, sizeof(visited));

    int front = 0, back = 0;
    queue[back++] = (QueueNode){start_x, start_y, 0};
    visited[start_y][start_x] = true;

    while (front < back) {
        QueueNode current = queue[front++];

        if (current.x == goal_x && current.y == goal_y) {
            return current.steps;
        }

        for (int d = 0; d < 4; d++) {
            int nx = current.x + dx[d];
            int ny = current.y + dy[d];

            if (nx >= 0 && nx < GRID_SIZE && ny >= 0 && ny < GRID_SIZE &&
                !visited[ny][nx] && !corrupted[ny][nx]) {
                visited[ny][nx] = true;
                queue[back++] = (QueueNode){nx, ny, current.steps + 1};
            }
        }
    }

    return -1;
}

int part1(void) {
    mark_corrupted(1024);
    return bfs();
}

Position part2(void) {
    /* Binary search to find the first byte that blocks the path */
    int left = 0, right = num_positions;

    while (left < right) {
        int mid = (left + right) / 2;
        mark_corrupted(mid + 1);
        if (bfs() == -1) {
            right = mid;
        } else {
            left = mid + 1;
        }
    }

    return positions[left];
}

int main(void) {
    parse_input("../input.txt");

    int answer1 = part1();
    printf("Part 1: %d\n", answer1);

    Position blocking = part2();
    printf("Part 2: %d,%d\n", blocking.x, blocking.y);

    return 0;
}
