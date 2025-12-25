#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_NODES 1000
#define MAX_EDGES 10000
#define NODE_LEN 3

// Graph representation
typedef struct {
    char nodes[MAX_NODES][NODE_LEN];
    int node_count;
    bool adj[MAX_NODES][MAX_NODES];
} Graph;

// Triangle structure
typedef struct {
    int a, b, c;
} Triangle;

// Set for tracking nodes in Bron-Kerbosch
typedef struct {
    bool present[MAX_NODES];
    int size;
} NodeSet;

// Global to track maximum clique
int max_clique[MAX_NODES];
int max_clique_size = 0;

// Find or add node to graph, return index
int get_node_index(Graph *g, const char *name) {
    for (int i = 0; i < g->node_count; i++) {
        if (strcmp(g->nodes[i], name) == 0) {
            return i;
        }
    }
    // Add new node
    strcpy(g->nodes[g->node_count], name);
    return g->node_count++;
}

// Parse input file and build graph
void parse_input(const char *filename, Graph *g) {
    g->node_count = 0;
    memset(g->adj, 0, sizeof(g->adj));

    FILE *f = fopen(filename, "r");
    if (!f) {
        perror("Failed to open input file");
        exit(1);
    }

    char line[10];
    while (fgets(line, sizeof(line), f)) {
        char a[NODE_LEN], b[NODE_LEN];
        if (sscanf(line, "%2[^-]-%2s", a, b) == 2) {
            int idx_a = get_node_index(g, a);
            int idx_b = get_node_index(g, b);
            g->adj[idx_a][idx_b] = true;
            g->adj[idx_b][idx_a] = true;
        }
    }

    fclose(f);
}

// Find all triangles
void find_triangles(Graph *g, Triangle **triangles, int *tri_count) {
    *tri_count = 0;
    *triangles = malloc(MAX_EDGES * sizeof(Triangle));

    for (int a = 0; a < g->node_count; a++) {
        for (int b = a + 1; b < g->node_count; b++) {
            if (!g->adj[a][b]) continue;

            for (int c = b + 1; c < g->node_count; c++) {
                if (g->adj[a][c] && g->adj[b][c]) {
                    (*triangles)[*tri_count].a = a;
                    (*triangles)[*tri_count].b = b;
                    (*triangles)[*tri_count].c = c;
                    (*tri_count)++;
                }
            }
        }
    }
}

// Part 1: Count triangles with at least one 't' node
int part1(Graph *g) {
    Triangle *triangles;
    int tri_count;
    find_triangles(g, &triangles, &tri_count);

    int count = 0;
    for (int i = 0; i < tri_count; i++) {
        if (g->nodes[triangles[i].a][0] == 't' ||
            g->nodes[triangles[i].b][0] == 't' ||
            g->nodes[triangles[i].c][0] == 't') {
            count++;
        }
    }

    free(triangles);
    return count;
}

// NodeSet operations
void set_init(NodeSet *s) {
    memset(s->present, 0, sizeof(s->present));
    s->size = 0;
}

void set_add(NodeSet *s, int node) {
    if (!s->present[node]) {
        s->present[node] = true;
        s->size++;
    }
}

void set_remove(NodeSet *s, int node) {
    if (s->present[node]) {
        s->present[node] = false;
        s->size--;
    }
}

void set_copy(NodeSet *dest, const NodeSet *src) {
    memcpy(dest->present, src->present, sizeof(src->present));
    dest->size = src->size;
}

// Intersection of two sets
void set_intersect(NodeSet *result, const NodeSet *a, const NodeSet *b) {
    set_init(result);
    for (int i = 0; i < MAX_NODES; i++) {
        if (a->present[i] && b->present[i]) {
            result->present[i] = true;
            result->size++;
        }
    }
}

// Union of two sets
void set_union(NodeSet *result, const NodeSet *a, const NodeSet *b) {
    set_init(result);
    for (int i = 0; i < MAX_NODES; i++) {
        if (a->present[i] || b->present[i]) {
            result->present[i] = true;
            result->size++;
        }
    }
}

// Difference of two sets
void set_difference(NodeSet *result, const NodeSet *a, const NodeSet *b) {
    set_init(result);
    for (int i = 0; i < MAX_NODES; i++) {
        if (a->present[i] && !b->present[i]) {
            result->present[i] = true;
            result->size++;
        }
    }
}

// Get neighbors of a node as a set
void get_neighbors(Graph *g, int node, NodeSet *neighbors) {
    set_init(neighbors);
    for (int i = 0; i < g->node_count; i++) {
        if (g->adj[node][i]) {
            neighbors->present[i] = true;
            neighbors->size++;
        }
    }
}

// Bron-Kerbosch algorithm with pivot
void bron_kerbosch(Graph *g, NodeSet *r, NodeSet *p, NodeSet *x) {
    if (p->size == 0 && x->size == 0) {
        // Found a maximal clique
        if (r->size > max_clique_size) {
            max_clique_size = 0;
            for (int i = 0; i < MAX_NODES; i++) {
                if (r->present[i]) {
                    max_clique[max_clique_size++] = i;
                }
            }
        }
        return;
    }

    // Find pivot (node in P ∪ X with most neighbors in P)
    NodeSet union_set;
    set_union(&union_set, p, x);

    int pivot = -1;
    int max_neighbors = -1;
    for (int i = 0; i < MAX_NODES; i++) {
        if (union_set.present[i]) {
            int neighbor_count = 0;
            for (int j = 0; j < MAX_NODES; j++) {
                if (p->present[j] && g->adj[i][j]) {
                    neighbor_count++;
                }
            }
            if (neighbor_count > max_neighbors) {
                max_neighbors = neighbor_count;
                pivot = i;
            }
        }
    }

    // Get pivot neighbors
    NodeSet pivot_neighbors;
    if (pivot != -1) {
        get_neighbors(g, pivot, &pivot_neighbors);
    } else {
        set_init(&pivot_neighbors);
    }

    // P - N(pivot)
    NodeSet candidates;
    set_difference(&candidates, p, &pivot_neighbors);

    // Iterate over candidates
    for (int v = 0; v < MAX_NODES; v++) {
        if (!candidates.present[v]) continue;

        // Get neighbors of v
        NodeSet v_neighbors;
        get_neighbors(g, v, &v_neighbors);

        // R ∪ {v}
        NodeSet new_r;
        set_copy(&new_r, r);
        set_add(&new_r, v);

        // P ∩ N(v)
        NodeSet new_p;
        set_intersect(&new_p, p, &v_neighbors);

        // X ∩ N(v)
        NodeSet new_x;
        set_intersect(&new_x, x, &v_neighbors);

        bron_kerbosch(g, &new_r, &new_p, &new_x);

        // P := P \ {v}
        set_remove(p, v);

        // X := X ∪ {v}
        set_add(x, v);
    }
}

// String comparison for qsort
int compare_strings(const void *a, const void *b) {
    return strcmp(*(const char **)a, *(const char **)b);
}

// Part 2: Find maximum clique
void part2(Graph *g, char *password) {
    max_clique_size = 0;

    // Initialize sets for Bron-Kerbosch
    NodeSet r, p, x;
    set_init(&r);
    set_init(&x);
    set_init(&p);

    // P = all nodes
    for (int i = 0; i < g->node_count; i++) {
        p.present[i] = true;
        p.size++;
    }

    bron_kerbosch(g, &r, &p, &x);

    // Sort the clique nodes alphabetically
    char *sorted_nodes[MAX_NODES];
    for (int i = 0; i < max_clique_size; i++) {
        sorted_nodes[i] = g->nodes[max_clique[i]];
    }
    qsort(sorted_nodes, max_clique_size, sizeof(char*), compare_strings);

    // Build password
    password[0] = '\0';
    for (int i = 0; i < max_clique_size; i++) {
        if (i > 0) strcat(password, ",");
        strcat(password, sorted_nodes[i]);
    }
}

int main() {
    Graph g;
    parse_input("../input.txt", &g);

    printf("Part 1: %d\n", part1(&g));

    char password[1000];
    part2(&g, password);
    printf("Part 2: %s\n", password);

    return 0;
}
