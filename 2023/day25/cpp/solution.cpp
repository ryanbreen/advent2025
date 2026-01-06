#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <unordered_map>
#include <unordered_set>
#include <queue>
#include <algorithm>
#include <sstream>
#include <cstring>

using namespace std;

// Optimized graph representation using integer node IDs
constexpr int MAX_NODES = 2048;
constexpr int SAMPLE_SIZE = 100;
constexpr int TOP_CANDIDATES = 20;

struct Edge {
    int u, v;
    double betweenness;
};

// Global data structures (stack allocation for performance)
static vector<int> adj_list[MAX_NODES];       // Adjacency list for fast neighbor iteration
static int edge_index[MAX_NODES][MAX_NODES];  // Quick edge lookup
static int node_count = 0;
static unordered_map<string, int> name_to_id;
static vector<Edge> edges;

// Get or create node ID
int get_node_id(const string& name) {
    auto it = name_to_id.find(name);
    if (it != name_to_id.end()) {
        return it->second;
    }
    int id = node_count++;
    name_to_id[name] = id;
    return id;
}

// Parse input into adjacency list
void parse_input(const string& filename) {
    ifstream file(filename);
    string line;

    while (getline(file, line)) {
        if (line.empty()) continue;

        size_t colon_pos = line.find(':');
        string left = line.substr(0, colon_pos);
        string right_part = line.substr(colon_pos + 2);

        int left_id = get_node_id(left);

        istringstream iss(right_part);
        string neighbor;
        while (iss >> neighbor) {
            int right_id = get_node_id(neighbor);
            adj_list[left_id].push_back(right_id);
            adj_list[right_id].push_back(left_id);
        }
    }
}

// BFS to find component size, excluding certain edges
int bfs_component_size(int start, const bool excluded[]) {
    bool visited[MAX_NODES] = {false};
    int queue[MAX_NODES];
    int front = 0, back = 0;

    visited[start] = true;
    queue[back++] = start;
    int count = 1;

    while (front < back) {
        int node = queue[front++];

        for (int neighbor : adj_list[node]) {
            if (visited[neighbor]) continue;

            // Check if this edge is excluded
            bool skip = false;
            if (excluded) {
                for (size_t i = 0; i < edges.size(); i++) {
                    if (!excluded[i]) continue;
                    if ((edges[i].u == node && edges[i].v == neighbor) ||
                        (edges[i].v == node && edges[i].u == neighbor)) {
                        skip = true;
                        break;
                    }
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

// Compute edge betweenness using Brandes algorithm
void compute_edge_betweenness() {
    // Build edges list and index
    edges.clear();
    memset(edge_index, -1, sizeof(edge_index));

    for (int u = 0; u < node_count; u++) {
        for (int v : adj_list[u]) {
            if (u < v) {  // Only add each edge once
                int idx = edges.size();
                edges.push_back({u, v, 0.0});
                edge_index[u][v] = idx;
                edge_index[v][u] = idx;
            }
        }
    }

    // Deterministic sampling
    int sample_count = min(node_count, SAMPLE_SIZE);
    int step = node_count / sample_count;
    if (step < 1) step = 1;

    // Allocate predecessor arrays
    vector<vector<int>> pred(MAX_NODES);
    for (int i = 0; i < MAX_NODES; i++) {
        pred[i].reserve(MAX_NODES);
    }

    // For each sampled source node
    for (int s_idx = 0; s_idx < sample_count; s_idx++) {
        int source = (s_idx * step) % node_count;

        // BFS to find shortest paths
        int dist[MAX_NODES];
        double num_paths[MAX_NODES];

        fill_n(dist, node_count, -1);
        fill_n(num_paths, node_count, 0.0);

        for (auto& p : pred) p.clear();

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

            for (int neighbor : adj_list[node]) {
                if (dist[neighbor] == -1) {
                    dist[neighbor] = dist[node] + 1;
                    queue[back++] = neighbor;
                }

                if (dist[neighbor] == dist[node] + 1) {
                    pred[neighbor].push_back(node);
                    num_paths[neighbor] += num_paths[node];
                }
            }
        }

        // Accumulate edge betweenness (reverse order)
        double dependency[MAX_NODES] = {0.0};

        for (int i = visited_count - 1; i >= 0; i--) {
            int node = visited_order[i];

            for (int p : pred[node]) {
                double frac = num_paths[p] / num_paths[node];
                double contrib = frac * (1.0 + dependency[node]);

                // Add to edge betweenness using fast lookup
                int idx = edge_index[p][node];
                if (idx >= 0) {
                    edges[idx].betweenness += contrib;
                }

                dependency[p] += contrib;
            }
        }
    }
}

// Find the 3-edge cut
int find_cut_edges() {
    // Compute edge betweenness
    compute_edge_betweenness();

    // Sort edges by betweenness
    sort(edges.begin(), edges.end(),
         [](const Edge& a, const Edge& b) { return a.betweenness > b.betweenness; });

    // Try combinations of top edges
    int top_count = min((int)edges.size(), TOP_CANDIDATES);

    for (int i = 0; i < top_count; i++) {
        for (int j = i + 1; j < top_count; j++) {
            for (int k = j + 1; k < top_count; k++) {
                bool excluded[MAX_NODES * 2] = {false};
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
    parse_input("../input.txt");

    int part1 = find_cut_edges();

    cout << "Part 1: " << part1 << endl;
    cout << "Part 2: Push the big red button!" << endl;

    return 0;
}
