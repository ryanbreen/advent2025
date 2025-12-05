#include <iostream>
#include <fstream>
#include <vector>
#include <string>

using namespace std;

// 8 directions: right, left, down, up, and 4 diagonals
const int DIRECTIONS[8][2] = {
    {0, 1},   // right
    {0, -1},  // left
    {1, 0},   // down
    {-1, 0},  // up
    {1, 1},   // down-right
    {1, -1},  // down-left
    {-1, 1},  // up-right
    {-1, -1}  // up-left
};

vector<string> read_input(const string& filename) {
    vector<string> grid;
    ifstream file(filename);
    string line;

    while (getline(file, line)) {
        if (!line.empty()) {
            grid.push_back(line);
        }
    }

    return grid;
}

int part1(const vector<string>& grid) {
    const string target = "XMAS";
    int count = 0;
    int rows = grid.size();
    int cols = grid[0].size();

    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            // Try each direction from this position
            for (int d = 0; d < 8; d++) {
                int dr = DIRECTIONS[d][0];
                int dc = DIRECTIONS[d][1];

                // Check if XMAS fits in this direction
                bool found = true;
                for (int i = 0; i < target.length(); i++) {
                    int nr = r + dr * i;
                    int nc = c + dc * i;

                    if (nr < 0 || nr >= rows || nc < 0 || nc >= cols) {
                        found = false;
                        break;
                    }
                    if (grid[nr][nc] != target[i]) {
                        found = false;
                        break;
                    }
                }

                if (found) {
                    count++;
                }
            }
        }
    }

    return count;
}

int part2(const vector<string>& grid) {
    // Find X-MAS patterns: two MAS strings forming an X with A in the center
    // Each diagonal can be MAS or SAM
    int count = 0;
    int rows = grid.size();
    int cols = grid[0].size();

    // Check each possible center point (A must be in the middle)
    for (int r = 1; r < rows - 1; r++) {
        for (int c = 1; c < cols - 1; c++) {
            if (grid[r][c] != 'A') {
                continue;
            }

            // Get the four corners
            char top_left = grid[r - 1][c - 1];
            char top_right = grid[r - 1][c + 1];
            char bottom_left = grid[r + 1][c - 1];
            char bottom_right = grid[r + 1][c + 1];

            // Check diagonal 1 (top-left to bottom-right): MAS or SAM
            bool diag1_ok = (top_left == 'M' && bottom_right == 'S') ||
                           (top_left == 'S' && bottom_right == 'M');

            // Check diagonal 2 (top-right to bottom-left): MAS or SAM
            bool diag2_ok = (top_right == 'M' && bottom_left == 'S') ||
                           (top_right == 'S' && bottom_left == 'M');

            if (diag1_ok && diag2_ok) {
                count++;
            }
        }
    }

    return count;
}

int main() {
    vector<string> grid = read_input("../input.txt");

    cout << "Part 1: " << part1(grid) << endl;
    cout << "Part 2: " << part2(grid) << endl;

    return 0;
}
