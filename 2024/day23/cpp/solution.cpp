#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <set>
#include <vector>
#include <algorithm>

using namespace std;

// Type aliases for clarity
using Graph = unordered_map<string, unordered_set<string>>;
using Triangle = set<string>;

// Parse input file into adjacency list graph
Graph parse_input(const string& filename) {
    Graph graph;
    ifstream file(filename);
    string line;

    while (getline(file, line)) {
        size_t pos = line.find('-');
        string a = line.substr(0, pos);
        string b = line.substr(pos + 1);

        graph[a].insert(b);
        graph[b].insert(a);
    }

    return graph;
}

// Find all triangles in the graph
set<Triangle> find_triangles(const Graph& graph) {
    set<Triangle> triangles;

    for (const auto& [a, neighbors_a] : graph) {
        for (const string& b : neighbors_a) {
            if (a < b) {  // Only process each edge once
                // Find common neighbors (intersection of neighbor sets)
                for (const string& c : neighbors_a) {
                    if (graph.at(b).count(c)) {
                        // Create sorted triangle to avoid duplicates
                        Triangle tri = {a, b, c};
                        triangles.insert(tri);
                    }
                }
            }
        }
    }

    return triangles;
}

// Part 1: Count triangles with at least one node starting with 't'
int part1(const Graph& graph) {
    auto triangles = find_triangles(graph);
    int count = 0;

    for (const Triangle& tri : triangles) {
        for (const string& node : tri) {
            if (node[0] == 't') {
                count++;
                break;
            }
        }
    }

    return count;
}

// Helper to convert unordered_set to set for Bron-Kerbosch
set<string> to_set(const unordered_set<string>& uset) {
    return set<string>(uset.begin(), uset.end());
}

// Set intersection helper
set<string> set_intersection(const set<string>& a, const unordered_set<string>& b) {
    set<string> result;
    for (const string& item : a) {
        if (b.count(item)) {
            result.insert(item);
        }
    }
    return result;
}

// Set difference helper
set<string> set_difference(const set<string>& a, const unordered_set<string>& b) {
    set<string> result;
    for (const string& item : a) {
        if (!b.count(item)) {
            result.insert(item);
        }
    }
    return result;
}

// Set union helper
set<string> set_union(const set<string>& a, const set<string>& b) {
    set<string> result = a;
    result.insert(b.begin(), b.end());
    return result;
}

// Bron-Kerbosch algorithm to find all maximal cliques
void bron_kerbosch(
    const Graph& graph,
    set<string> r,
    set<string> p,
    set<string> x,
    vector<set<string>>& cliques
) {
    if (p.empty() && x.empty()) {
        cliques.push_back(r);
        return;
    }

    // Find pivot with most neighbors in P (to reduce branching)
    set<string> p_union_x = set_union(p, x);

    string pivot = *p_union_x.begin();  // Initialize with first element
    size_t max_neighbors = set_intersection(p, graph.at(pivot)).size();

    for (const string& v : p_union_x) {
        size_t count = set_intersection(p, graph.at(v)).size();
        if (count > max_neighbors) {
            max_neighbors = count;
            pivot = v;
        }
    }

    // Process vertices not in pivot's neighborhood
    set<string> candidates = set_difference(p, graph.at(pivot));

    for (const string& v : candidates) {
        set<string> new_r = r;
        new_r.insert(v);

        set<string> new_p = set_intersection(p, graph.at(v));
        set<string> new_x = set_intersection(x, graph.at(v));

        bron_kerbosch(graph, new_r, new_p, new_x, cliques);

        p.erase(v);
        x.insert(v);
    }
}

// Part 2: Find the largest clique and return password
string part2(const Graph& graph) {
    vector<set<string>> cliques;

    // Get all nodes as initial P set
    set<string> all_nodes;
    for (const auto& [node, _] : graph) {
        all_nodes.insert(node);
    }

    bron_kerbosch(graph, {}, all_nodes, {}, cliques);

    // Find the largest clique
    auto largest = max_element(cliques.begin(), cliques.end(),
        [](const set<string>& a, const set<string>& b) {
            return a.size() < b.size();
        });

    // Build comma-separated password (already sorted in set)
    string password;
    for (auto it = largest->begin(); it != largest->end(); ++it) {
        if (it != largest->begin()) password += ",";
        password += *it;
    }

    return password;
}

int main() {
    Graph graph = parse_input("../input.txt");

    cout << "Part 1: " << part1(graph) << endl;
    cout << "Part 2: " << part2(graph) << endl;

    return 0;
}
