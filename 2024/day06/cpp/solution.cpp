#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <set>
#include <utility>
#include <tuple>

using namespace std;

// Directions: Up, Right, Down, Left (0, 1, 2, 3)
const int dx[] = {-1, 0, 1, 0};
const int dy[] = {0, 1, 0, -1};

// Find starting position and direction
tuple<int, int, int> findStart(const vector<string>& grid) {
    int rows = grid.size();
    int cols = grid[0].size();

    for (int i = 0; i < rows; i++) {
        for (int j = 0; j < cols; j++) {
            if (grid[i][j] == '^') {
                return {i, j, 0};  // Up
            } else if (grid[i][j] == '>') {
                return {i, j, 1};  // Right
            } else if (grid[i][j] == 'v') {
                return {i, j, 2};  // Down
            } else if (grid[i][j] == '<') {
                return {i, j, 3};  // Left
            }
        }
    }
    return {0, 0, 0};
}

// Simulate patrol and return visited positions
set<pair<int, int>> simulatePatrol(const vector<string>& grid, int startX, int startY, int startDir) {
    int rows = grid.size();
    int cols = grid[0].size();

    int x = startX, y = startY, dir = startDir;
    set<pair<int, int>> visited;
    visited.insert({x, y});

    while (true) {
        int nx = x + dx[dir];
        int ny = y + dy[dir];

        if (nx < 0 || nx >= rows || ny < 0 || ny >= cols) {
            break;
        }

        if (grid[nx][ny] == '#') {
            dir = (dir + 1) % 4;
        } else {
            x = nx;
            y = ny;
            visited.insert({x, y});
        }
    }

    return visited;
}

// Check if placing an obstruction creates a loop
bool createsLoop(vector<string> grid, int startX, int startY, int startDir, int obsX, int obsY) {
    // Don't place on starting position or existing obstacle
    if ((obsX == startX && obsY == startY) || grid[obsX][obsY] == '#') {
        return false;
    }

    // Place obstruction
    grid[obsX][obsY] = '#';

    int rows = grid.size();
    int cols = grid[0].size();

    // Track states (position + direction) to detect loops
    set<tuple<int, int, int>> states;
    int x = startX, y = startY, dir = startDir;
    states.insert({x, y, dir});

    while (true) {
        int nx = x + dx[dir];
        int ny = y + dy[dir];

        if (nx < 0 || nx >= rows || ny < 0 || ny >= cols) {
            return false; // Guard escaped
        }

        if (grid[nx][ny] == '#') {
            dir = (dir + 1) % 4;
        } else {
            x = nx;
            y = ny;
        }

        // Check for loop
        auto state = make_tuple(x, y, dir);
        if (states.count(state)) {
            return true; // Loop detected
        }
        states.insert(state);
    }
}

int part1(const vector<string>& grid) {
    auto [x, y, dir] = findStart(grid);
    return simulatePatrol(grid, x, y, dir).size();
}

int part2(const vector<string>& grid) {
    auto [startX, startY, startDir] = findStart(grid);

    // Get positions on original path (optimization)
    set<pair<int, int>> originalPath = simulatePatrol(grid, startX, startY, startDir);

    int count = 0;
    for (const auto& [obsX, obsY] : originalPath) {
        if (createsLoop(grid, startX, startY, startDir, obsX, obsY)) {
            count++;
        }
    }

    return count;
}

int main() {
    ifstream file("../input.txt");
    if (!file.is_open()) {
        cerr << "Error: Could not open input file" << endl;
        return 1;
    }

    vector<string> grid;
    string line;
    while (getline(file, line)) {
        if (!line.empty()) {
            grid.push_back(line);
        }
    }
    file.close();

    cout << "Part 1: " << part1(grid) << endl;
    cout << "Part 2: " << part2(grid) << endl;

    return 0;
}
