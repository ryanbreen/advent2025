#include <iostream>
#include <fstream>
#include <vector>
#include <string>

using namespace std;

// Directions for 8 neighbors (including diagonals)
const int directions[8][2] = {
    {-1, -1}, {-1, 0}, {-1, 1},
    {0, -1},           {0, 1},
    {1, -1},  {1, 0},  {1, 1}
};

// Count adjacent rolls for a given position
int countAdjacentRolls(const vector<string>& grid, int r, int c) {
    int rows = grid.size();
    int cols = grid[0].size();
    int count = 0;

    for (int i = 0; i < 8; i++) {
        int nr = r + directions[i][0];
        int nc = c + directions[i][1];

        // Check bounds
        if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
            if (grid[nr][nc] == '@') {
                count++;
            }
        }
    }

    return count;
}

int part1(const vector<string>& lines) {
    int rows = lines.size();
    int cols = lines[0].size();
    int accessible_count = 0;

    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            if (lines[r][c] == '@') {
                int adjacent = countAdjacentRolls(lines, r, c);
                if (adjacent < 4) {
                    accessible_count++;
                }
            }
        }
    }

    return accessible_count;
}

int part2(const vector<string>& lines) {
    // Create a mutable copy of the grid
    vector<string> grid = lines;
    int rows = grid.size();
    int cols = grid[0].size();

    // Precompute neighbor counts for all rolls
    vector<vector<int>> neighborCount(rows, vector<int>(cols, 0));
    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            if (grid[r][c] == '@') {
                neighborCount[r][c] = countAdjacentRolls(grid, r, c);
            }
        }
    }

    // Initialize queue with all accessible rolls (< 4 neighbors)
    vector<pair<int, int>> queue;
    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            if (grid[r][c] == '@' && neighborCount[r][c] < 4) {
                queue.push_back({r, c});
            }
        }
    }

    int total_removed = 0;
    size_t queue_index = 0;

    // Process queue
    while (queue_index < queue.size()) {
        auto [r, c] = queue[queue_index++];

        // Skip if already removed
        if (grid[r][c] != '@') {
            continue;
        }

        // Remove this roll
        grid[r][c] = '.';
        total_removed++;

        // Decrement neighbor counts for all adjacent rolls
        for (int i = 0; i < 8; i++) {
            int nr = r + directions[i][0];
            int nc = c + directions[i][1];

            if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && grid[nr][nc] == '@') {
                neighborCount[nr][nc]--;
                // If this neighbor just became accessible, add to queue
                if (neighborCount[nr][nc] == 3) {
                    queue.push_back({nr, nc});
                }
            }
        }
    }

    return total_removed;
}

int main() {
    // Read input file
    ifstream file("../input.txt");
    if (!file.is_open()) {
        cerr << "Error: Could not open input file" << endl;
        return 1;
    }

    vector<string> lines;
    string line;
    while (getline(file, line)) {
        if (!line.empty()) {
            lines.push_back(line);
        }
    }
    file.close();

    if (lines.empty()) {
        cerr << "Error: No input data" << endl;
        return 1;
    }

    cout << "Part 1: " << part1(lines) << endl;
    cout << "Part 2: " << part2(lines) << endl;

    return 0;
}
