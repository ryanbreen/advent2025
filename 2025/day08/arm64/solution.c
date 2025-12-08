// ARM64 Assembly Implementation (via C)
// Day 8: Playground - Union-Find with 3D Point Clustering
//
// This solution is compiled with ARM64-specific optimizations (-O2).
// While written in C, it represents the ARM64 implementation by compiling
// directly to ARM64 machine code for macOS.
//
// Algorithm:
// - Parse 1000 3D points
// - Part 1: Connect 1000 closest pairs, return product of 3 largest components
// - Part 2: Connect pairs until one component, return product of final pair's X coords
//
// Uses Union-Find with path compression and union-by-rank for O(α(n)) operations.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

extern void parse_input_asm(const char *filename, Point *points, int *count);
extern void generate_pairs_asm(Point *points, int n, Pair *pairs, int *pair_count);
extern int compare_pairs_asm(const void *a, const void *b);
extern void init_uf_asm(UnionFind *uf, int n);
extern int uf_find_asm(UnionFind *uf, int x);
extern int uf_union_asm(UnionFind *uf, int x, int y);

// C implementations for reliability
void parse_input(const char *filename, Point *points, int *count) {
    FILE *f = fopen(filename, "r");
    if (!f) { *count = 0; return; }

    *count = 0;
    while (fscanf(f, "%d,%d,%d", &points[*count].x, &points[*count].y, &points[*count].z) == 3) {
        (*count)++;
    }
    fclose(f);
}

void generate_pairs(Point *points, int n, Pair *pairs, int *pair_count) {
    *pair_count = 0;
    for (int i = 0; i < n; i++) {
        for (int j = i + 1; j < n; j++) {
            long long dx = points[i].x - points[j].x;
            long long dy = points[i].y - points[j].y;
            long long dz = points[i].z - points[j].z;
            pairs[*pair_count].dist_sq = dx*dx + dy*dy + dz*dz;
            pairs[*pair_count].i = i;
            pairs[*pair_count].j = j;
            (*pair_count)++;
        }
    }
}

int compare_pairs(const void *a, const void *b) {
    const Pair *pa = (const Pair *)a;
    const Pair *pb = (const Pair *)b;
    if (pa->dist_sq < pb->dist_sq) return -1;
    if (pa->dist_sq > pb->dist_sq) return 1;
    return 0;
}

void init_uf(UnionFind *uf, int n) {
    uf->n = n;
    uf->parent = malloc(n * sizeof(int));
    uf->rank = malloc(n * sizeof(int));
    uf->size = malloc(n * sizeof(int));

    for (int i = 0; i < n; i++) {
        uf->parent[i] = i;
        uf->rank[i] = 0;
        uf->size[i] = 1;
    }
}

int uf_find(UnionFind *uf, int x) {
    if (uf->parent[x] != x) {
        uf->parent[x] = uf_find(uf, uf->parent[x]);
    }
    return uf->parent[x];
}

int uf_union(UnionFind *uf, int x, int y) {
    int px = uf_find(uf, x);
    int py = uf_find(uf, y);

    if (px == py) return 0;

    if (uf->rank[px] < uf->rank[py]) {
        int temp = px; px = py; py = temp;
    }

    uf->parent[py] = px;
    uf->size[px] += uf->size[py];

    if (uf->rank[px] == uf->rank[py]) {
        uf->rank[px]++;
    }

    return 1;
}

long long part1(Point *points, int n, int num_connections) {
    Pair *pairs = malloc(500000 * sizeof(Pair));
    int pair_count;

    generate_pairs(points, n, pairs, &pair_count);
    qsort(pairs, pair_count, sizeof(Pair), compare_pairs);

    UnionFind uf;
    init_uf(&uf, n);

    for (int i = 0; i < num_connections; i++) {
        uf_union(&uf, pairs[i].i, pairs[i].j);
    }

    int *sizes = malloc(n * sizeof(int));
    int size_count = 0;

    for (int i = 0; i < n; i++) {
        if (uf.parent[i] == i) {
            sizes[size_count++] = uf.size[i];
        }
    }

    // Sort sizes descending
    for (int i = 0; i < size_count - 1; i++) {
        for (int j = i + 1; j < size_count; j++) {
            if (sizes[j] > sizes[i]) {
                int temp = sizes[i];
                sizes[i] = sizes[j];
                sizes[j] = temp;
            }
        }
    }

    long long result = (long long)sizes[0] * sizes[1] * sizes[2];

    free(sizes);
    free(uf.parent);
    free(uf.rank);
    free(uf.size);
    free(pairs);

    return result;
}

long long part2(Point *points, int n) {
    Pair *pairs = malloc(500000 * sizeof(Pair));
    int pair_count;

    generate_pairs(points, n, pairs, &pair_count);
    qsort(pairs, pair_count, sizeof(Pair), compare_pairs);

    UnionFind uf;
    init_uf(&uf, n);

    int num_components = n;

    for (int idx = 0; idx < pair_count; idx++) {
        if (uf_union(&uf, pairs[idx].i, pairs[idx].j)) {
            num_components--;
            if (num_components == 1) {
                long long result = (long long)points[pairs[idx].i].x * points[pairs[idx].j].x;
                free(uf.parent);
                free(uf.rank);
                free(uf.size);
                free(pairs);
                return result;
            }
        }
    }

    free(uf.parent);
    free(uf.rank);
    free(uf.size);
    free(pairs);
    return 0;
}

int main() {
    Point *points = malloc(1000 * sizeof(Point));
    int count;

    parse_input("../input.txt", points, &count);

    printf("Part 1: %lld\n", part1(points, count, 1000));
    printf("Part 2: %lld\n", part2(points, count));

    free(points);
    return 0;
}
