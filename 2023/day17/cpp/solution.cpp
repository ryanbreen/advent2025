/**
 * Day 17: Clumsy Crucible
 * Dijkstra's shortest path with movement constraints.
 */

#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <queue>
#include <unordered_set>
#include <string>

using namespace std;

// Direction deltas: 0=right, 1=down, 2=left, 3=up
const int DR[] = {0, 1, 0, -1};
const int DC[] = {1, 0, -1, 0};

struct State {
    int heat;
    int row;
    int col;
    int dir;
    int consec;

    bool operator>(const State& other) const {
        return heat > other.heat;
    }
};

// Hash function for visited states
struct StateHash {
    size_t operator()(const tuple<int, int, int, int>& s) const {
        auto [r, c, d, consec] = s;
        return hash<long long>()(
            ((long long)r << 24) | ((long long)c << 16) |
            ((long long)d << 8) | consec
        );
    }
};

vector<vector<int>> parseInput(const string& filename) {
    ifstream file(filename);
    vector<vector<int>> grid;
    string line;

    while (getline(file, line)) {
        if (line.empty()) continue;
        vector<int> row;
        for (char c : line) {
            row.push_back(c - '0');
        }
        grid.push_back(row);
    }

    return grid;
}

int dijkstra(const vector<vector<int>>& grid, int minStraight, int maxStraight) {
    int rows = grid.size();
    int cols = grid[0].size();

    // Priority queue: min-heap by heat loss
    priority_queue<State, vector<State>, greater<State>> pq;
    unordered_set<tuple<int, int, int, int>, StateHash> visited;

    // Start with no direction (-1)
    pq.push({0, 0, 0, -1, 0});

    while (!pq.empty()) {
        State curr = pq.top();
        pq.pop();

        // Check if we reached the goal
        if (curr.row == rows - 1 && curr.col == cols - 1) {
            if (minStraight == 0 || curr.consec >= minStraight) {
                return curr.heat;
            }
        }

        auto state = make_tuple(curr.row, curr.col, curr.dir, curr.consec);
        if (visited.count(state)) {
            continue;
        }
        visited.insert(state);

        // Try all four directions
        for (int nd = 0; nd < 4; nd++) {
            // Can't reverse direction
            if (curr.dir != -1 && nd == (curr.dir + 2) % 4) {
                continue;
            }

            int nr = curr.row + DR[nd];
            int nc = curr.col + DC[nd];

            // Bounds check
            if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) {
                continue;
            }

            int newConsec;
            if (nd == curr.dir) {
                // Continuing in same direction
                newConsec = curr.consec + 1;
                if (newConsec > maxStraight) {
                    continue;
                }
            } else {
                // Turning - must have gone minStraight in previous direction first
                if (curr.dir != -1 && curr.consec < minStraight) {
                    continue;
                }
                newConsec = 1;
            }

            int newHeat = curr.heat + grid[nr][nc];
            auto newState = make_tuple(nr, nc, nd, newConsec);

            if (!visited.count(newState)) {
                pq.push({newHeat, nr, nc, nd, newConsec});
            }
        }
    }

    return -1; // No path found
}

int part1(const vector<vector<int>>& grid) {
    return dijkstra(grid, 0, 3);
}

int part2(const vector<vector<int>>& grid) {
    return dijkstra(grid, 4, 10);
}

int main() {
    auto grid = parseInput("../input.txt");

    cout << "Part 1: " << part1(grid) << endl;
    cout << "Part 2: " << part2(grid) << endl;

    return 0;
}
