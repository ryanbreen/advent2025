#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define MAX_POINTS 10000

typedef struct {
    int x, y, z;
} Point;

typedef struct {
    long long dist_sq;
    int i, j;
} Pair;

typedef struct {
    int *parent;
    int *rank;
    int *size;
    int n;
} UnionFind;

// Union-Find functions
UnionFind* uf_create(int n) {
    UnionFind *uf = malloc(sizeof(UnionFind));
    uf->n = n;
    uf->parent = malloc(n * sizeof(int));
    uf->rank = malloc(n * sizeof(int));
    uf->size = malloc(n * sizeof(int));

    for (int i = 0; i < n; i++) {
        uf->parent[i] = i;
        uf->rank[i] = 0;
        uf->size[i] = 1;
    }

    return uf;
}

void uf_free(UnionFind *uf) {
    free(uf->parent);
    free(uf->rank);
    free(uf->size);
    free(uf);
}

int uf_find(UnionFind *uf, int x) {
    if (uf->parent[x] != x) {
        uf->parent[x] = uf_find(uf, uf->parent[x]);  // Path compression
    }
    return uf->parent[x];
}

int uf_union(UnionFind *uf, int x, int y) {
    int px = uf_find(uf, x);
    int py = uf_find(uf, y);

    if (px == py) {
        return 0;  // Already in same set
    }

    // Union by rank
    if (uf->rank[px] < uf->rank[py]) {
        int temp = px;
        px = py;
        py = temp;
    }

    uf->parent[py] = px;
    uf->size[px] += uf->size[py];

    if (uf->rank[px] == uf->rank[py]) {
        uf->rank[px]++;
    }

    return 1;  // Successfully merged
}

void uf_get_component_sizes(UnionFind *uf, int *sizes, int *count) {
    *count = 0;
    for (int i = 0; i < uf->n; i++) {
        if (uf->parent[i] == i) {  // Root of a component
            sizes[(*count)++] = uf->size[i];
        }
    }
}

// Calculate squared Euclidean distance
long long dist_sq(Point *p1, Point *p2) {
    long long dx = (long long)(p1->x - p2->x);
    long long dy = (long long)(p1->y - p2->y);
    long long dz = (long long)(p1->z - p2->z);
    return dx * dx + dy * dy + dz * dz;
}

// Comparison function for qsort
int compare_pairs(const void *a, const void *b) {
    Pair *pa = (Pair *)a;
    Pair *pb = (Pair *)b;
    if (pa->dist_sq < pb->dist_sq) return -1;
    if (pa->dist_sq > pb->dist_sq) return 1;
    return 0;
}

// Comparison function for sorting sizes in descending order
int compare_desc(const void *a, const void *b) {
    return (*(int *)b - *(int *)a);
}

// Parse input file
int parse_input(const char *filename, Point *points) {
    FILE *f = fopen(filename, "r");
    if (!f) {
        perror("Error opening file");
        return -1;
    }

    int count = 0;
    char line[256];
    while (fgets(line, sizeof(line), f)) {
        if (sscanf(line, "%d,%d,%d", &points[count].x, &points[count].y, &points[count].z) == 3) {
            count++;
        }
    }

    fclose(f);
    return count;
}

long long part1(Point *points, int n, int num_connections) {
    // Generate all pairs with distances
    int num_pairs = n * (n - 1) / 2;
    Pair *pairs = malloc(num_pairs * sizeof(Pair));

    int idx = 0;
    for (int i = 0; i < n; i++) {
        for (int j = i + 1; j < n; j++) {
            pairs[idx].dist_sq = dist_sq(&points[i], &points[j]);
            pairs[idx].i = i;
            pairs[idx].j = j;
            idx++;
        }
    }

    // Sort by distance
    qsort(pairs, num_pairs, sizeof(Pair), compare_pairs);

    // Union-Find to connect closest pairs
    UnionFind *uf = uf_create(n);
    int connections = 0;

    for (int k = 0; k < num_pairs && connections < num_connections; k++) {
        uf_union(uf, pairs[k].i, pairs[k].j);
        connections++;
    }

    // Get component sizes and find the 3 largest
    int *sizes = malloc(n * sizeof(int));
    int num_components;
    uf_get_component_sizes(uf, sizes, &num_components);

    // Sort sizes in descending order
    qsort(sizes, num_components, sizeof(int), compare_desc);

    // Multiply the 3 largest
    long long result = (long long)sizes[0] * sizes[1] * sizes[2];

    free(sizes);
    free(pairs);
    uf_free(uf);

    return result;
}

long long part2(Point *points, int n) {
    // Generate all pairs with distances
    int num_pairs = n * (n - 1) / 2;
    Pair *pairs = malloc(num_pairs * sizeof(Pair));

    int idx = 0;
    for (int i = 0; i < n; i++) {
        for (int j = i + 1; j < n; j++) {
            pairs[idx].dist_sq = dist_sq(&points[i], &points[j]);
            pairs[idx].i = i;
            pairs[idx].j = j;
            idx++;
        }
    }

    // Sort by distance
    qsort(pairs, num_pairs, sizeof(Pair), compare_pairs);

    // Union-Find to connect until all in one circuit
    UnionFind *uf = uf_create(n);
    int num_components = n;
    long long result = 0;

    for (int k = 0; k < num_pairs; k++) {
        if (uf_union(uf, pairs[k].i, pairs[k].j)) {
            num_components--;
            if (num_components == 1) {
                // This was the last connection
                result = (long long)points[pairs[k].i].x * points[pairs[k].j].x;
                break;
            }
        }
    }

    free(pairs);
    uf_free(uf);

    return result;
}

int main(int argc, char *argv[]) {
    const char *filename = argc > 1 ? argv[1] : "../input.txt";

    Point *points = malloc(MAX_POINTS * sizeof(Point));
    int n = parse_input(filename, points);

    if (n < 0) {
        fprintf(stderr, "Failed to parse input\n");
        free(points);
        return 1;
    }

    printf("Part 1: %lld\n", part1(points, n, 1000));
    printf("Part 2: %lld\n", part2(points, n));

    free(points);
    return 0;
}
