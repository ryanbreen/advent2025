#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_CUBES 3000
#define HASH_SIZE 65536

// 6 directions: +x, -x, +y, -y, +z, -z
static const int DX[] = {1, -1, 0, 0, 0, 0};
static const int DY[] = {0, 0, 1, -1, 0, 0};
static const int DZ[] = {0, 0, 0, 0, 1, -1};

typedef struct {
    int x, y, z;
} Point;

typedef struct HashNode {
    int x, y, z;
    struct HashNode *next;
} HashNode;

typedef struct {
    HashNode *buckets[HASH_SIZE];
} HashSet;

// Hash function for 3D coordinates
static inline unsigned int hash_point(int x, int y, int z) {
    unsigned int h = (unsigned int)(x * 73856093) ^
                     (unsigned int)(y * 19349663) ^
                     (unsigned int)(z * 83492791);
    return h % HASH_SIZE;
}

void hashset_init(HashSet *set) {
    memset(set->buckets, 0, sizeof(set->buckets));
}

void hashset_free(HashSet *set) {
    for (int i = 0; i < HASH_SIZE; i++) {
        HashNode *node = set->buckets[i];
        while (node) {
            HashNode *next = node->next;
            free(node);
            node = next;
        }
    }
}

bool hashset_contains(HashSet *set, int x, int y, int z) {
    unsigned int h = hash_point(x, y, z);
    HashNode *node = set->buckets[h];
    while (node) {
        if (node->x == x && node->y == y && node->z == z) {
            return true;
        }
        node = node->next;
    }
    return false;
}

void hashset_add(HashSet *set, int x, int y, int z) {
    if (hashset_contains(set, x, y, z)) {
        return;
    }
    unsigned int h = hash_point(x, y, z);
    HashNode *node = malloc(sizeof(HashNode));
    node->x = x;
    node->y = y;
    node->z = z;
    node->next = set->buckets[h];
    set->buckets[h] = node;
}

// Queue for BFS
typedef struct {
    Point *data;
    int head, tail, capacity;
} Queue;

void queue_init(Queue *q, int capacity) {
    q->data = malloc(sizeof(Point) * capacity);
    q->head = 0;
    q->tail = 0;
    q->capacity = capacity;
}

void queue_free(Queue *q) {
    free(q->data);
}

bool queue_empty(Queue *q) {
    return q->head == q->tail;
}

void queue_push(Queue *q, int x, int y, int z) {
    q->data[q->tail].x = x;
    q->data[q->tail].y = y;
    q->data[q->tail].z = z;
    q->tail++;
}

Point queue_pop(Queue *q) {
    return q->data[q->head++];
}

// Global data
static Point cubes[MAX_CUBES];
static int num_cubes = 0;
static HashSet cube_set;

void parse_input(const char *filename) {
    FILE *f = fopen(filename, "r");
    if (!f) {
        fprintf(stderr, "Cannot open %s\n", filename);
        exit(1);
    }

    hashset_init(&cube_set);

    int x, y, z;
    while (fscanf(f, "%d,%d,%d", &x, &y, &z) == 3) {
        cubes[num_cubes].x = x;
        cubes[num_cubes].y = y;
        cubes[num_cubes].z = z;
        num_cubes++;
        hashset_add(&cube_set, x, y, z);
    }

    fclose(f);
}

int part1(void) {
    int surface_area = 0;

    for (int i = 0; i < num_cubes; i++) {
        int x = cubes[i].x;
        int y = cubes[i].y;
        int z = cubes[i].z;

        for (int d = 0; d < 6; d++) {
            int nx = x + DX[d];
            int ny = y + DY[d];
            int nz = z + DZ[d];

            if (!hashset_contains(&cube_set, nx, ny, nz)) {
                surface_area++;
            }
        }
    }

    return surface_area;
}

int part2(void) {
    // Find bounding box with 1 unit padding
    int min_x = cubes[0].x, max_x = cubes[0].x;
    int min_y = cubes[0].y, max_y = cubes[0].y;
    int min_z = cubes[0].z, max_z = cubes[0].z;

    for (int i = 1; i < num_cubes; i++) {
        if (cubes[i].x < min_x) min_x = cubes[i].x;
        if (cubes[i].x > max_x) max_x = cubes[i].x;
        if (cubes[i].y < min_y) min_y = cubes[i].y;
        if (cubes[i].y > max_y) max_y = cubes[i].y;
        if (cubes[i].z < min_z) min_z = cubes[i].z;
        if (cubes[i].z > max_z) max_z = cubes[i].z;
    }

    min_x--; max_x++;
    min_y--; max_y++;
    min_z--; max_z++;

    // BFS to find all exterior air cells
    HashSet exterior;
    hashset_init(&exterior);

    // Queue capacity: worst case is entire bounding box
    int box_size = (max_x - min_x + 1) * (max_y - min_y + 1) * (max_z - min_z + 1);
    Queue queue;
    queue_init(&queue, box_size);

    // Start from corner of bounding box
    queue_push(&queue, min_x, min_y, min_z);
    hashset_add(&exterior, min_x, min_y, min_z);

    while (!queue_empty(&queue)) {
        Point p = queue_pop(&queue);

        for (int d = 0; d < 6; d++) {
            int nx = p.x + DX[d];
            int ny = p.y + DY[d];
            int nz = p.z + DZ[d];

            // Stay within bounds
            if (nx < min_x || nx > max_x ||
                ny < min_y || ny > max_y ||
                nz < min_z || nz > max_z) {
                continue;
            }

            // Skip cubes and already visited
            if (hashset_contains(&cube_set, nx, ny, nz) ||
                hashset_contains(&exterior, nx, ny, nz)) {
                continue;
            }

            hashset_add(&exterior, nx, ny, nz);
            queue_push(&queue, nx, ny, nz);
        }
    }

    // Count faces touching exterior air
    int surface_area = 0;
    for (int i = 0; i < num_cubes; i++) {
        int x = cubes[i].x;
        int y = cubes[i].y;
        int z = cubes[i].z;

        for (int d = 0; d < 6; d++) {
            int nx = x + DX[d];
            int ny = y + DY[d];
            int nz = z + DZ[d];

            if (hashset_contains(&exterior, nx, ny, nz)) {
                surface_area++;
            }
        }
    }

    queue_free(&queue);
    hashset_free(&exterior);

    return surface_area;
}

int main(void) {
    // Get path to input file (../input.txt from solution directory)
    parse_input("../input.txt");

    printf("Part 1: %d\n", part1());
    printf("Part 2: %d\n", part2());

    hashset_free(&cube_set);

    return 0;
}
