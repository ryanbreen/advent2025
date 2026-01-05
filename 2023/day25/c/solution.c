/*
 * Advent of Code 2023 - Day 25: Snowverload
 * Find the minimum cut of 3 edges that divides the graph into two components.
 *
 * Uses edge betweenness centrality: edges that form the cut between two large
 * components will have high betweenness (many shortest paths pass through them).
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_NODES 2048
#define MAX_EDGES 4096
#define MAX_LINE 512
#define SAMPLE_SIZE 100
#define TOP_CANDIDATES 20

typedef struct {
    char name[16];
    int id;
} Node;

typedef struct {
    int u, v;
    double betweenness;
} Edge;

typedef struct {
    int nodes[MAX_NODES];
    int count;
} NodeList;

// Global graph representation
static int adj_matrix[MAX_NODES][MAX_NODES];
static Node nodes[MAX_NODES];
static int node_count = 0;
static Edge edges[MAX_EDGES];
static int edge_count = 0;

// Find or create a node ID
static int get_node_id(const char *name) {
    // Linear search (simple but works for this problem size)
    for (int i = 0; i < node_count; i++) {
        if (strcmp(nodes[i].name, name) == 0)
            return nodes[i].id;
    }

    // Create new node
    if (node_count >= MAX_NODES) {
        fprintf(stderr, "Too many nodes\n");
        exit(1);
    }

    strncpy(nodes[node_count].name, name, 15);
    nodes[node_count].name[15] = '\0';
    nodes[node_count].id = node_count;
    return node_count++;
}

// Add edge to graph
static void add_edge(int u, int v) {
    if (u == v) return;
    adj_matrix[u][v] = 1;
    adj_matrix[v][u] = 1;
}

// Parse input file
static void parse_input(const char *filename) {
    FILE *f = fopen(filename, "r");
    if (!f) {
        perror("fopen");
        exit(1);
    }

    char line[MAX_LINE];
    while (fgets(line, sizeof(line), f)) {
        // Remove newline
        line[strcspn(line, "\n")] = '\0';
        if (strlen(line) == 0) continue;

        // Parse "node: neighbor1 neighbor2 ..."
        char *colon = strchr(line, ':');
        if (!colon) continue;

        *colon = '\0';
        char *left = line;
        char *right = colon + 1;

        int left_id = get_node_id(left);

        // Parse neighbors
        char *token = strtok(right, " ");
        while (token) {
            int right_id = get_node_id(token);
            add_edge(left_id, right_id);
            token = strtok(NULL, " ");
        }
    }

    fclose(f);
}

// BFS to find component size, excluding certain edges
static int bfs_component_size(int start, bool excluded[MAX_EDGES]) {
    bool visited[MAX_NODES] = {false};
    int queue[MAX_NODES];
    int front = 0, back = 0;

    visited[start] = true;
    queue[back++] = start;
    int count = 1;

    while (front < back) {
        int node = queue[front++];

        for (int neighbor = 0; neighbor < node_count; neighbor++) {
            if (!adj_matrix[node][neighbor]) continue;
            if (visited[neighbor]) continue;

            // Check if this edge is excluded
            bool skip = false;
            for (int i = 0; i < edge_count && excluded; i++) {
                if (!excluded[i]) continue;
                if ((edges[i].u == node && edges[i].v == neighbor) ||
                    (edges[i].v == node && edges[i].u == neighbor)) {
                    skip = true;
                    break;
                }
            }
            if (skip) continue;

            visited[neighbor] = true;
            queue[back++] = neighbor;
            count++;
        }
    }

    return count;
}

// Compute edge betweenness centrality using Brandes algorithm
static void compute_edge_betweenness() {
    // Initialize edges list
    edge_count = 0;
    for (int u = 0; u < node_count; u++) {
        for (int v = u + 1; v < node_count; v++) {
            if (adj_matrix[u][v]) {
                edges[edge_count].u = u;
                edges[edge_count].v = v;
                edges[edge_count].betweenness = 0.0;
                edge_count++;
            }
        }
    }

    // Sample nodes (use deterministic sampling)
    int sample_nodes[SAMPLE_SIZE];
    int sample_count = node_count < SAMPLE_SIZE ? node_count : SAMPLE_SIZE;

    // Simple deterministic sampling (every nth node)
    int step = node_count / sample_count;
    if (step < 1) step = 1;
    for (int i = 0; i < sample_count; i++) {
        sample_nodes[i] = (i * step) % node_count;
    }

    // Allocate predecessor array dynamically to avoid stack overflow
    int **pred = malloc(MAX_NODES * sizeof(int *));
    for (int i = 0; i < MAX_NODES; i++) {
        pred[i] = malloc(MAX_NODES * sizeof(int));
    }

    // For each sampled source node
    for (int s_idx = 0; s_idx < sample_count; s_idx++) {
        int source = sample_nodes[s_idx];

        // BFS to find shortest paths
        int dist[MAX_NODES];
        int pred_count[MAX_NODES];
        double num_paths[MAX_NODES];

        for (int i = 0; i < node_count; i++) {
            dist[i] = -1;
            pred_count[i] = 0;
            num_paths[i] = 0.0;
        }

        dist[source] = 0;
        num_paths[source] = 1.0;

        int queue[MAX_NODES];
        int front = 0, back = 0;
        queue[back++] = source;

        int visited_order[MAX_NODES];
        int visited_count = 0;

        while (front < back) {
            int node = queue[front++];
            visited_order[visited_count++] = node;

            for (int neighbor = 0; neighbor < node_count; neighbor++) {
                if (!adj_matrix[node][neighbor]) continue;

                if (dist[neighbor] == -1) {
                    dist[neighbor] = dist[node] + 1;
                    queue[back++] = neighbor;
                }

                if (dist[neighbor] == dist[node] + 1) {
                    pred[neighbor][pred_count[neighbor]++] = node;
                    num_paths[neighbor] += num_paths[node];
                }
            }
        }

        // Accumulate edge betweenness (reverse order)
        double dependency[MAX_NODES] = {0.0};

        for (int i = visited_count - 1; i >= 0; i--) {
            int node = visited_order[i];

            for (int p_idx = 0; p_idx < pred_count[node]; p_idx++) {
                int p = pred[node][p_idx];

                double frac = num_paths[p] / num_paths[node];
                double contrib = frac * (1.0 + dependency[node]);

                // Add to edge betweenness
                for (int e = 0; e < edge_count; e++) {
                    if ((edges[e].u == p && edges[e].v == node) ||
                        (edges[e].v == p && edges[e].u == node)) {
                        edges[e].betweenness += contrib;
                        break;
                    }
                }

                dependency[p] += contrib;
            }
        }
    }

    // Free predecessor array
    for (int i = 0; i < MAX_NODES; i++) {
        free(pred[i]);
    }
    free(pred);
}

// Compare function for sorting edges by betweenness
static int compare_edges(const void *a, const void *b) {
    const Edge *ea = (const Edge *)a;
    const Edge *eb = (const Edge *)b;
    if (ea->betweenness > eb->betweenness) return -1;
    if (ea->betweenness < eb->betweenness) return 1;
    return 0;
}

// Find the 3-edge cut
static int find_cut_edges() {
    // Compute edge betweenness
    compute_edge_betweenness();

    // Sort edges by betweenness
    qsort(edges, edge_count, sizeof(Edge), compare_edges);

    // Try combinations of top edges
    int top_count = edge_count < TOP_CANDIDATES ? edge_count : TOP_CANDIDATES;

    for (int i = 0; i < top_count; i++) {
        for (int j = i + 1; j < top_count; j++) {
            for (int k = j + 1; k < top_count; k++) {
                bool excluded[MAX_EDGES] = {false};
                excluded[i] = true;
                excluded[j] = true;
                excluded[k] = true;

                int size1 = bfs_component_size(0, excluded);

                if (size1 < node_count) {
                    int size2 = node_count - size1;
                    return size1 * size2;
                }
            }
        }
    }

    return -1;
}

int main() {
    // Parse input
    parse_input("../input.txt");

    // Solve Part 1
    int part1 = find_cut_edges();

    printf("Part 1: %d\n", part1);
    printf("Part 2: Push the big red button!\n");

    return 0;
}
