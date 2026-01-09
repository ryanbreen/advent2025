#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <unordered_map>
#include <map>
#include <queue>
#include <regex>
#include <algorithm>

using namespace std;

struct Valve {
    string name;
    int flowRate;
    vector<string> tunnels;
};

unordered_map<string, Valve> parseInput(const string& filename) {
    unordered_map<string, Valve> valves;
    ifstream file(filename);
    string line;

    regex pattern(R"(Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.+))");

    while (getline(file, line)) {
        smatch match;
        if (regex_match(line, match, pattern)) {
            Valve v;
            v.name = match[1];
            v.flowRate = stoi(match[2]);

            string neighbors = match[3];
            stringstream ss(neighbors);
            string neighbor;
            while (getline(ss, neighbor, ',')) {
                // Trim whitespace
                size_t start = neighbor.find_first_not_of(" ");
                size_t end = neighbor.find_last_not_of(" ");
                if (start != string::npos) {
                    v.tunnels.push_back(neighbor.substr(start, end - start + 1));
                }
            }
            valves[v.name] = v;
        }
    }
    return valves;
}

// Compute shortest distances between all relevant valves using BFS
map<string, map<string, int>> computeDistances(
    const unordered_map<string, Valve>& valves,
    const vector<string>& relevant
) {
    map<string, map<string, int>> distances;

    for (const string& start : relevant) {
        distances[start] = {};
        queue<pair<string, int>> q;
        unordered_map<string, bool> visited;

        q.push({start, 0});
        visited[start] = true;

        while (!q.empty()) {
            auto [curr, dist] = q.front();
            q.pop();

            // Check if current is a relevant valve (not start)
            if (curr != start && find(relevant.begin(), relevant.end(), curr) != relevant.end()) {
                distances[start][curr] = dist;
            }

            for (const string& neighbor : valves.at(curr).tunnels) {
                if (!visited[neighbor]) {
                    visited[neighbor] = true;
                    q.push({neighbor, dist + 1});
                }
            }
        }
    }

    return distances;
}

// DFS with memoization for Part 1
map<tuple<string, int, uint64_t>, int> memo1;

int dfs1(
    const string& pos,
    int timeLeft,
    uint64_t openedMask,
    const vector<string>& valuable,
    const map<string, map<string, int>>& distances,
    const unordered_map<string, Valve>& valves,
    const map<string, int>& valveIndex
) {
    if (timeLeft <= 0) return 0;

    auto key = make_tuple(pos, timeLeft, openedMask);
    auto it = memo1.find(key);
    if (it != memo1.end()) return it->second;

    int best = 0;

    for (size_t i = 0; i < valuable.size(); i++) {
        if (openedMask & (1ULL << i)) continue;  // Already opened

        const string& nextValve = valuable[i];
        int timeCost = distances.at(pos).at(nextValve) + 1;

        if (timeCost < timeLeft) {
            int newTime = timeLeft - timeCost;
            int pressure = valves.at(nextValve).flowRate * newTime;
            uint64_t newMask = openedMask | (1ULL << i);

            int result = pressure + dfs1(nextValve, newTime, newMask, valuable, distances, valves, valveIndex);
            best = max(best, result);
        }
    }

    memo1[key] = best;
    return best;
}

int part1(const unordered_map<string, Valve>& valves) {
    // Find relevant valves (flow > 0 plus AA)
    vector<string> relevant;
    relevant.push_back("AA");

    vector<string> valuable;  // Only valves with flow > 0
    for (const auto& [name, valve] : valves) {
        if (valve.flowRate > 0) {
            relevant.push_back(name);
            valuable.push_back(name);
        }
    }

    auto distances = computeDistances(valves, relevant);

    // Create valve index map
    map<string, int> valveIndex;
    for (size_t i = 0; i < valuable.size(); i++) {
        valveIndex[valuable[i]] = i;
    }

    memo1.clear();
    return dfs1("AA", 30, 0, valuable, distances, valves, valveIndex);
}

// DFS for computing max pressure for a specific subset
int dfsSubset(
    const string& pos,
    int timeLeft,
    uint64_t openedMask,
    uint64_t subsetMask,
    const vector<string>& valuable,
    const map<string, map<string, int>>& distances,
    const unordered_map<string, Valve>& valves,
    map<tuple<string, int, uint64_t>, int>& memo
) {
    if (timeLeft <= 0) return 0;

    auto key = make_tuple(pos, timeLeft, openedMask);
    auto it = memo.find(key);
    if (it != memo.end()) return it->second;

    int best = 0;

    for (size_t i = 0; i < valuable.size(); i++) {
        // Skip if not in our subset or already opened
        if (!(subsetMask & (1ULL << i))) continue;
        if (openedMask & (1ULL << i)) continue;

        const string& nextValve = valuable[i];
        int timeCost = distances.at(pos).at(nextValve) + 1;

        if (timeCost < timeLeft) {
            int newTime = timeLeft - timeCost;
            int pressure = valves.at(nextValve).flowRate * newTime;
            uint64_t newMask = openedMask | (1ULL << i);

            int result = pressure + dfsSubset(nextValve, newTime, newMask, subsetMask,
                                               valuable, distances, valves, memo);
            best = max(best, result);
        }
    }

    memo[key] = best;
    return best;
}

int part2(const unordered_map<string, Valve>& valves) {
    // Find relevant valves
    vector<string> relevant;
    relevant.push_back("AA");

    vector<string> valuable;
    for (const auto& [name, valve] : valves) {
        if (valve.flowRate > 0) {
            relevant.push_back(name);
            valuable.push_back(name);
        }
    }

    auto distances = computeDistances(valves, relevant);

    int n = valuable.size();
    uint64_t fullMask = (1ULL << n) - 1;

    // Compute max pressure for each subset
    vector<int> maxScores(1 << n);

    for (uint64_t mask = 0; mask <= fullMask; mask++) {
        map<tuple<string, int, uint64_t>, int> memo;
        maxScores[mask] = dfsSubset("AA", 26, 0, mask, valuable, distances, valves, memo);
    }

    // Find best partition
    int best = 0;
    for (uint64_t mask = 0; mask <= fullMask; mask++) {
        uint64_t complement = fullMask ^ mask;
        if (mask <= complement) {  // Avoid counting same partition twice
            best = max(best, maxScores[mask] + maxScores[complement]);
        }
    }

    return best;
}

int main() {
    string inputFile = "../input.txt";
    auto valves = parseInput(inputFile);

    cout << "Part 1: " << part1(valves) << endl;
    cout << "Part 2: " << part2(valves) << endl;

    return 0;
}
