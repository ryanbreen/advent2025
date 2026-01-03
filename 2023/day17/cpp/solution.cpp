/**
 * Day 17: Clumsy Crucible
 * Dijkstra's shortest path with movement constraints.
 *
 * Optimized: uses static array for visited states instead of hash set.
 */

#include <iostream>
#include <fstream>
#include <vector>
#include <queue>
#include <cstring>
#include <string>

using namespace std;

constexpr int MAX_SIZE = 150;
constexpr int MAX_CONSEC = 11;  // 0-10
constexpr int NUM_DIRS = 4;

// Direction deltas: 0=right, 1=down, 2=left, 3=up
constexpr int DR[] = {0, 1, 0, -1};
constexpr int DC[] = {1, 0, -1, 0};

// Static visited array: [row][col][dir][consec]
static bool visited[MAX_SIZE][MAX_SIZE][NUM_DIRS][MAX_CONSEC];

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

    // Clear visited array
    memset(visited, 0, sizeof(visited));

    // Priority queue: min-heap by heat loss
    priority_queue<State, vector<State>, greater<State>> pq;

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

        // Check if already visited (skip dir=-1 case)
        if (curr.dir >= 0 && visited[curr.row][curr.col][curr.dir][curr.consec]) {
            continue;
        }
        if (curr.dir >= 0) {
            visited[curr.row][curr.col][curr.dir][curr.consec] = true;
        }

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

            // Skip if already visited
            if (visited[nr][nc][nd][newConsec]) {
                continue;
            }

            int newHeat = curr.heat + grid[nr][nc];
            pq.push({newHeat, nr, nc, nd, newConsec});
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
