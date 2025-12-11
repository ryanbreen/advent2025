#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <unordered_map>
#include <functional>

using namespace std;

// Type aliases for clarity
using Graph = unordered_map<string, vector<string>>;
using Memo = unordered_map<string, uint64_t>;

// Parse input file into adjacency list graph
Graph parse_input(const string& filename) {
    Graph graph;
    ifstream file(filename);
    string line;

    while (getline(file, line)) {
        if (line.empty()) continue;

        size_t colon_pos = line.find(": ");
        if (colon_pos == string::npos) continue;

        string node = line.substr(0, colon_pos);
        string neighbors_str = line.substr(colon_pos + 2);

        vector<string> neighbors;
        istringstream iss(neighbors_str);
        string neighbor;
        while (iss >> neighbor) {
            neighbors.push_back(neighbor);
        }

        graph[node] = neighbors;
    }

    return graph;
}

// Count paths from a given node to 'out' using memoization
uint64_t count_paths_to_out(const string& node, const Graph& graph, Memo& memo) {
    if (node == "out") {
        return 1;
    }

    // Check if already computed
    auto it = memo.find(node);
    if (it != memo.end()) {
        return it->second;
    }

    // Check if node exists in graph
    auto graph_it = graph.find(node);
    if (graph_it == graph.end()) {
        memo[node] = 0;
        return 0;
    }

    // Sum paths through all neighbors
    uint64_t total = 0;
    for (const string& neighbor : graph_it->second) {
        total += count_paths_to_out(neighbor, graph, memo);
    }

    memo[node] = total;
    return total;
}

// Part 1: Count all paths from 'you' to 'out'
uint64_t part1(const Graph& graph) {
    Memo memo;
    return count_paths_to_out("you", graph, memo);
}

// Generic function to count paths from any node to a specific target
function<uint64_t(const string&)> create_path_counter(const string& target, const Graph& graph) {
    // Shared memo map for this target
    auto memo = make_shared<Memo>();

    return [target, &graph, memo](const string& node) -> uint64_t {
        if (node == target) {
            return 1;
        }

        auto it = memo->find(node);
        if (it != memo->end()) {
            return it->second;
        }

        auto graph_it = graph.find(node);
        if (graph_it == graph.end()) {
            (*memo)[node] = 0;
            return 0;
        }

        uint64_t total = 0;
        for (const string& neighbor : graph_it->second) {
            // Need to recurse - create a temporary counter for recursion
            Memo temp_memo = *memo;
            function<uint64_t(const string&)> recurse = [&](const string& n) -> uint64_t {
                if (n == target) return 1;
                auto it = temp_memo.find(n);
                if (it != temp_memo.end()) return it->second;
                auto git = graph.find(n);
                if (git == graph.end()) {
                    temp_memo[n] = 0;
                    return 0;
                }
                uint64_t sum = 0;
                for (const string& nb : git->second) {
                    sum += recurse(nb);
                }
                temp_memo[n] = sum;
                return sum;
            };
            total += recurse(neighbor);
            *memo = temp_memo;
        }

        (*memo)[node] = total;
        return total;
    };
}

// Part 2: Count paths from 'svr' to 'out' that visit both 'dac' and 'fft'
uint64_t part2(const Graph& graph) {
    // Create separate memoization for each target
    Memo memo_to_out, memo_to_dac, memo_to_fft;

    // Helper to count paths to a specific target
    auto count_to_target = [&graph](const string& start, const string& target, Memo& memo) -> uint64_t {
        function<uint64_t(const string&)> count = [&](const string& node) -> uint64_t {
            if (node == target) return 1;

            auto it = memo.find(node);
            if (it != memo.end()) return it->second;

            auto git = graph.find(node);
            if (git == graph.end()) {
                memo[node] = 0;
                return 0;
            }

            uint64_t total = 0;
            for (const string& neighbor : git->second) {
                total += count(neighbor);
            }

            memo[node] = total;
            return total;
        };
        return count(start);
    };

    // Count paths: svr -> dac, dac -> fft, fft -> out
    uint64_t svr_to_dac = count_to_target("svr", "dac", memo_to_dac);
    uint64_t dac_to_fft = count_to_target("dac", "fft", memo_to_fft);
    uint64_t fft_to_out = count_to_target("fft", "out", memo_to_out);

    // Clear memos for reuse
    memo_to_dac.clear();
    memo_to_fft.clear();

    // Count paths: svr -> fft, fft -> dac, dac -> out
    uint64_t svr_to_fft = count_to_target("svr", "fft", memo_to_fft);
    uint64_t fft_to_dac = count_to_target("fft", "dac", memo_to_dac);
    memo_to_out.clear();
    uint64_t dac_to_out = count_to_target("dac", "out", memo_to_out);

    // Paths visiting dac before fft + paths visiting fft before dac
    uint64_t dac_before_fft = svr_to_dac * dac_to_fft * fft_to_out;
    uint64_t fft_before_dac = svr_to_fft * fft_to_dac * dac_to_out;

    return dac_before_fft + fft_before_dac;
}

int main() {
    Graph graph = parse_input("../input.txt");

    cout << "Part 1: " << part1(graph) << endl;
    cout << "Part 2: " << part2(graph) << endl;

    return 0;
}
