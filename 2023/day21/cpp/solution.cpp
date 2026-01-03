// Day 21: Step Counter - Garden plot reachability
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <queue>
#include <unordered_map>
#include <utility>

using namespace std;

// Custom hash for pair<int, int>
struct PairHash {
    size_t operator()(const pair<int, int>& p) const {
        return hash<long long>()(((long long)p.first << 32) | (unsigned int)p.second);
    }
};

// Parse grid and find starting position
pair<vector<string>, pair<int, int>> parseInput(const string& filename) {
    vector<string> grid;
    pair<int, int> start = {-1, -1};

    ifstream file(filename);
    string line;
    int row = 0;

    while (getline(file, line)) {
        if (line.empty()) continue;
        grid.push_back(line);
        for (int col = 0; col < (int)line.size(); col++) {
            if (line[col] == 'S') {
                start = {row, col};
            }
        }
        row++;
    }

    return {grid, start};
}

// Count cells reachable in exactly 'steps' steps (bounded grid for Part 1)
long long countReachable(const vector<string>& grid, pair<int, int> start, int steps) {
    int rows = grid.size();
    int cols = grid[0].size();

    unordered_map<pair<int, int>, int, PairHash> visited;
    queue<tuple<int, int, int>> q;

    q.push({start.first, start.second, 0});
    visited[start] = 0;

    int dr[] = {-1, 1, 0, 0};
    int dc[] = {0, 0, -1, 1};

    while (!q.empty()) {
        auto [r, c, dist] = q.front();
        q.pop();

        if (dist >= steps) continue;

        for (int i = 0; i < 4; i++) {
            int nr = r + dr[i];
            int nc = c + dc[i];

            if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
                if (grid[nr][nc] != '#' && visited.find({nr, nc}) == visited.end()) {
                    visited[{nr, nc}] = dist + 1;
                    q.push({nr, nc, dist + 1});
                }
            }
        }
    }

    // Count cells reachable in exactly 'steps' steps
    // A cell reachable in d steps can be reached in d+2, d+4, ... steps
    int targetParity = steps % 2;
    long long count = 0;

    for (const auto& [pos, d] : visited) {
        if (d <= steps && d % 2 == targetParity) {
            count++;
        }
    }

    return count;
}

// BFS on infinite tiled grid for small step counts
long long countReachableInfiniteBFS(const vector<string>& grid, pair<int, int> start, int steps) {
    int rows = grid.size();
    int cols = grid[0].size();

    unordered_map<pair<int, int>, int, PairHash> visited;
    queue<tuple<int, int, int>> q;

    q.push({start.first, start.second, 0});
    visited[start] = 0;

    int dr[] = {-1, 1, 0, 0};
    int dc[] = {0, 0, -1, 1};

    while (!q.empty()) {
        auto [r, c, dist] = q.front();
        q.pop();

        if (dist >= steps) continue;

        for (int i = 0; i < 4; i++) {
            int nr = r + dr[i];
            int nc = c + dc[i];

            // Map to grid coordinates (infinite tiling)
            int gr = ((nr % rows) + rows) % rows;
            int gc = ((nc % cols) + cols) % cols;

            if (grid[gr][gc] != '#' && visited.find({nr, nc}) == visited.end()) {
                visited[{nr, nc}] = dist + 1;
                q.push({nr, nc, dist + 1});
            }
        }
    }

    int targetParity = steps % 2;
    long long count = 0;

    for (const auto& [pos, d] : visited) {
        if (d <= steps && d % 2 == targetParity) {
            count++;
        }
    }

    return count;
}

// Count cells reachable in exactly 'steps' steps on an infinite tiled grid
// Uses the quadratic pattern that emerges due to the grid structure
long long countReachableInfinite(const vector<string>& grid, pair<int, int> start, long long steps) {
    int rows = grid.size();
    int size = rows;
    int half = size / 2;

    // For small step counts, use direct BFS on infinite grid
    if (steps <= size * 2) {
        return countReachableInfiniteBFS(grid, start, steps);
    }

    // The number of full grid widths we travel
    // steps = 26501365 = 65 + 202300 * 131
    long long n = (steps - half) / size;

    // Calculate reachable counts for n=0, 1, 2 to determine the quadratic
    // f(n) = an^2 + bn + c
    long long y0 = countReachableInfiniteBFS(grid, start, half);
    long long y1 = countReachableInfiniteBFS(grid, start, half + size);
    long long y2 = countReachableInfiniteBFS(grid, start, half + 2 * size);

    // Solve for a, b, c using finite differences
    // f(x) = y0 + (y1-y0)*x + (y2-2*y1+y0)*x*(x-1)/2
    long long a = (y2 - 2 * y1 + y0) / 2;
    long long b = y1 - y0 - a;
    long long c = y0;

    return a * n * n + b * n + c;
}

long long part1(const vector<string>& grid, pair<int, int> start) {
    return countReachable(grid, start, 64);
}

long long part2(const vector<string>& grid, pair<int, int> start) {
    return countReachableInfinite(grid, start, 26501365LL);
}

int main() {
    auto [grid, start] = parseInput("../input.txt");

    cout << "Part 1: " << part1(grid, start) << endl;
    cout << "Part 2: " << part2(grid, start) << endl;

    return 0;
}
