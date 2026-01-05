#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <queue>
#include <algorithm>
#include <random>
#include <sstream>

using namespace std;

// Graph represented as adjacency list
using Graph = map<string, set<string>>;
using Edge = pair<string, string>;

// Parse input into graph
Graph parse_input(const string& filename) {
    Graph graph;
    ifstream file(filename);
    string line;

    while (getline(file, line)) {
        if (line.empty()) continue;

        size_t colon_pos = line.find(':');
        string left = line.substr(0, colon_pos);
        string right_part = line.substr(colon_pos + 2);

        istringstream iss(right_part);
        string neighbor;
        while (iss >> neighbor) {
            graph[left].insert(neighbor);
            graph[neighbor].insert(left);
        }
    }

    return graph;
}

// Normalize edge (always sorted order)
Edge make_edge(const string& a, const string& b) {
    return a < b ? make_pair(a, b) : make_pair(b, a);
}

// BFS to find component size, excluding certain edges
int bfs_component_size(const Graph& graph, const string& start, const set<Edge>& excluded_edges) {
    set<string> visited;
    queue<string> q;

    visited.insert(start);
    q.push(start);

    while (!q.empty()) {
        string node = q.front();
        q.pop();

        auto it = graph.find(node);
        if (it == graph.end()) continue;

        for (const string& neighbor : it->second) {
            Edge edge = make_edge(node, neighbor);
            if (visited.find(neighbor) == visited.end() &&
                excluded_edges.find(edge) == excluded_edges.end()) {
                visited.insert(neighbor);
                q.push(neighbor);
            }
        }
    }

    return visited.size();
}

// Compute edge betweenness centrality using BFS from sampled nodes
map<Edge, double> compute_edge_betweenness(const Graph& graph, int sample_nodes = 100) {
    map<Edge, double> edge_count;

    // Get all nodes
    vector<string> nodes;
    for (const auto& pair : graph) {
        nodes.push_back(pair.first);
    }

    // Sample nodes for efficiency
    vector<string> sample;
    if (sample_nodes > 0 && (int)nodes.size() > sample_nodes) {
        mt19937 rng(42);
        shuffle(nodes.begin(), nodes.end(), rng);
        sample.assign(nodes.begin(), nodes.begin() + sample_nodes);
    } else {
        sample = nodes;
    }

    // For each source node, compute shortest paths
    for (const string& source : sample) {
        map<string, int> dist;
        map<string, vector<string>> pred;
        queue<string> q;

        dist[source] = 0;
        q.push(source);

        // BFS to find shortest paths
        while (!q.empty()) {
            string node = q.front();
            q.pop();

            auto it = graph.find(node);
            if (it == graph.end()) continue;

            for (const string& neighbor : it->second) {
                if (dist.find(neighbor) == dist.end()) {
                    dist[neighbor] = dist[node] + 1;
                    pred[neighbor].push_back(node);
                    q.push(neighbor);
                } else if (dist[neighbor] == dist[node] + 1) {
                    pred[neighbor].push_back(node);
                }
            }
        }

        // Count number of shortest paths to each node
        map<string, double> num_paths;
        num_paths[source] = 1.0;

        // Process nodes in order of increasing distance
        vector<pair<int, string>> ordered;
        for (const auto& d : dist) {
            ordered.push_back({d.second, d.first});
        }
        sort(ordered.begin(), ordered.end());

        for (const auto& p : ordered) {
            const string& node = p.second;
            for (const string& predecessor : pred[node]) {
                num_paths[node] += num_paths[predecessor];
            }
        }

        // Accumulate edge betweenness (reverse order)
        map<string, double> dependency;
        reverse(ordered.begin(), ordered.end());

        for (const auto& p : ordered) {
            const string& node = p.second;
            for (const string& predecessor : pred[node]) {
                Edge edge = make_edge(predecessor, node);
                double frac = num_paths[predecessor] / num_paths[node];
                double contrib = frac * (1.0 + dependency[node]);
                edge_count[edge] += contrib;
                dependency[predecessor] += contrib;
            }
        }
    }

    return edge_count;
}

// Find the 3 edges to cut
int find_cut_edges(const Graph& graph) {
    // Compute edge betweenness
    map<Edge, double> edge_betweenness = compute_edge_betweenness(graph, 100);

    // Sort edges by betweenness (highest first)
    vector<pair<Edge, double>> sorted_edges(edge_betweenness.begin(), edge_betweenness.end());
    sort(sorted_edges.begin(), sorted_edges.end(),
         [](const auto& a, const auto& b) { return a.second > b.second; });

    int total_nodes = graph.size();

    // Try removing top candidate edges
    vector<Edge> top_edges;
    for (int i = 0; i < min(20, (int)sorted_edges.size()); i++) {
        top_edges.push_back(sorted_edges[i].first);
    }

    // Try all combinations of 3 edges
    for (int i = 0; i < (int)top_edges.size(); i++) {
        for (int j = i + 1; j < (int)top_edges.size(); j++) {
            for (int k = j + 1; k < (int)top_edges.size(); k++) {
                set<Edge> excluded;
                excluded.insert(top_edges[i]);
                excluded.insert(top_edges[j]);
                excluded.insert(top_edges[k]);

                string start = graph.begin()->first;
                int size1 = bfs_component_size(graph, start, excluded);

                if (size1 < total_nodes) {
                    // Graph is disconnected!
                    int size2 = total_nodes - size1;
                    return size1 * size2;
                }
            }
        }
    }

    return -1;
}

int part1(const string& filename) {
    Graph graph = parse_input(filename);
    return find_cut_edges(graph);
}

string part2(const string&) {
    return "Push the big red button!";
}

int main() {
    string input_file = "../input.txt";

    cout << "Part 1: " << part1(input_file) << endl;
    cout << "Part 2: " << part2(input_file) << endl;

    return 0;
}
