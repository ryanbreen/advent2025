#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_VALVES 64
#define MAX_NAME_LEN 3
#define INF 9999

// Valve structure
typedef struct {
    char name[MAX_NAME_LEN];
    int flow;
    int neighbors[MAX_VALVES];
    int neighbor_count;
} Valve;

// Global data
Valve valves[MAX_VALVES];
int valve_count = 0;
int distances[MAX_VALVES][MAX_VALVES];

// Valuable valves (flow > 0)
int valuable[MAX_VALVES];
int valuable_count = 0;
int flows[MAX_VALVES];  // Flow rates indexed by valuable valve index

int aa_index = -1;

// Find valve index by name
int find_valve(const char *name) {
    for (int i = 0; i < valve_count; i++) {
        if (strcmp(valves[i].name, name) == 0) {
            return i;
        }
    }
    return -1;
}

// Get or create valve index
int get_or_create_valve(const char *name) {
    int idx = find_valve(name);
    if (idx >= 0) return idx;

    idx = valve_count++;
    strncpy(valves[idx].name, name, MAX_NAME_LEN - 1);
    valves[idx].name[MAX_NAME_LEN - 1] = '\0';
    valves[idx].flow = 0;
    valves[idx].neighbor_count = 0;
    return idx;
}

// Parse input
void parse_input(const char *filename) {
    FILE *f = fopen(filename, "r");
    if (!f) {
        perror("Cannot open input file");
        exit(1);
    }

    char line[256];
    while (fgets(line, sizeof(line), f)) {
        char name[MAX_NAME_LEN];
        int flow;
        char tunnels[200];

        // Parse: "Valve XX has flow rate=N; tunnels? leads? to valves? ..."
        char *ptr = line + 6; // Skip "Valve "
        name[0] = *ptr++;
        name[1] = *ptr++;
        name[2] = '\0';

        // Find flow rate
        ptr = strstr(line, "rate=");
        if (!ptr) continue;
        flow = atoi(ptr + 5);

        int idx = get_or_create_valve(name);
        valves[idx].flow = flow;

        // Find tunnel list
        ptr = strstr(line, "valve");
        if (!ptr) continue;
        ptr = strchr(ptr, ' ');
        if (!ptr) continue;
        ptr++; // Skip space after "valve" or "valves"

        // Parse comma-separated neighbors
        while (*ptr && *ptr != '\n') {
            char neighbor[MAX_NAME_LEN];
            neighbor[0] = *ptr++;
            neighbor[1] = *ptr++;
            neighbor[2] = '\0';

            int nidx = get_or_create_valve(neighbor);
            valves[idx].neighbors[valves[idx].neighbor_count++] = nidx;

            // Skip ", " if present
            if (*ptr == ',') {
                ptr += 2;
            }
        }
    }

    fclose(f);

    // Find AA
    aa_index = find_valve("AA");

    // Find valuable valves
    for (int i = 0; i < valve_count; i++) {
        if (valves[i].flow > 0) {
            flows[valuable_count] = valves[i].flow;
            valuable[valuable_count++] = i;
        }
    }
}

// BFS to compute distance from source to all valves
void bfs_from(int source) {
    int queue[MAX_VALVES];
    int head = 0, tail = 0;

    for (int i = 0; i < valve_count; i++) {
        distances[source][i] = INF;
    }

    distances[source][source] = 0;
    queue[tail++] = source;

    while (head < tail) {
        int curr = queue[head++];
        for (int i = 0; i < valves[curr].neighbor_count; i++) {
            int neighbor = valves[curr].neighbors[i];
            if (distances[source][neighbor] == INF) {
                distances[source][neighbor] = distances[source][curr] + 1;
                queue[tail++] = neighbor;
            }
        }
    }
}

// Compute all distances
void compute_distances() {
    // Compute distances from AA
    bfs_from(aa_index);

    // Compute distances from all valuable valves
    for (int i = 0; i < valuable_count; i++) {
        bfs_from(valuable[i]);
    }
}

// Memoization for Part 1
// State: (position_index, time_left, opened_mask)
// position_index: index in valuable array (or valve_count for AA)
// opened_mask: bitmask of opened valves
#define MAX_TIME 31
#define MEMO_SIZE (MAX_VALVES * MAX_TIME * (1 << 16))

int *memo1 = NULL;

int get_pos_idx(int valve_idx) {
    if (valve_idx == aa_index) return valuable_count;
    for (int i = 0; i < valuable_count; i++) {
        if (valuable[i] == valve_idx) return i;
    }
    return -1;
}

// DFS for Part 1
int dfs1(int pos, int time_left, int opened) {
    if (time_left <= 0) return 0;

    int pos_idx = get_pos_idx(pos);
    int memo_idx = (pos_idx * MAX_TIME + time_left) * (1 << valuable_count) + opened;

    if (memo1[memo_idx] != -1) {
        return memo1[memo_idx];
    }

    int best = 0;

    for (int i = 0; i < valuable_count; i++) {
        if (opened & (1 << i)) continue; // Already opened

        int next_valve = valuable[i];
        int time_cost = distances[pos][next_valve] + 1; // Move + open

        if (time_cost < time_left) {
            int new_time = time_left - time_cost;
            int pressure = flows[i] * new_time;
            int result = pressure + dfs1(next_valve, new_time, opened | (1 << i));
            if (result > best) best = result;
        }
    }

    memo1[memo_idx] = best;
    return best;
}

int part1() {
    int memo_size = (valuable_count + 1) * MAX_TIME * (1 << valuable_count);
    memo1 = malloc(memo_size * sizeof(int));
    if (!memo1) {
        fprintf(stderr, "Memory allocation failed\n");
        exit(1);
    }
    memset(memo1, -1, memo_size * sizeof(int));

    int result = dfs1(aa_index, 30, 0);

    free(memo1);
    memo1 = NULL;

    return result;
}

// DFS for a specific subset (Part 2)
int dfs2(int pos, int time_left, int opened, int subset_mask) {
    if (time_left <= 0) return 0;

    int best = 0;

    for (int i = 0; i < valuable_count; i++) {
        // Must be in subset and not already opened
        if (!(subset_mask & (1 << i))) continue;
        if (opened & (1 << i)) continue;

        int next_valve = valuable[i];
        int time_cost = distances[pos][next_valve] + 1;

        if (time_cost < time_left) {
            int new_time = time_left - time_cost;
            int pressure = flows[i] * new_time;
            int result = pressure + dfs2(next_valve, new_time, opened | (1 << i), subset_mask);
            if (result > best) best = result;
        }
    }

    return best;
}

// Memoization structure for subset evaluation
typedef struct {
    int subset;
    int score;
} SubsetScore;

int part2() {
    int n = valuable_count;
    int full_mask = (1 << n) - 1;

    // Compute max pressure for each subset
    int *max_scores = calloc(1 << n, sizeof(int));
    if (!max_scores) {
        fprintf(stderr, "Memory allocation failed\n");
        exit(1);
    }

    for (int mask = 0; mask <= full_mask; mask++) {
        max_scores[mask] = dfs2(aa_index, 26, 0, mask);
    }

    // Find best partition
    int best = 0;
    for (int mask = 0; mask <= full_mask; mask++) {
        int complement = full_mask ^ mask;
        if (mask <= complement) { // Avoid counting twice
            int score = max_scores[mask] + max_scores[complement];
            if (score > best) best = score;
        }
    }

    free(max_scores);
    return best;
}

int main() {
    parse_input("../input.txt");
    compute_distances();

    printf("Part 1: %d\n", part1());
    printf("Part 2: %d\n", part2());

    return 0;
}
