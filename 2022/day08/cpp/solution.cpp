#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm>

using Grid = std::vector<std::vector<int>>;

Grid parseGrid(const std::string& filename) {
    Grid grid;
    std::ifstream file(filename);
    std::string line;

    while (std::getline(file, line)) {
        if (line.empty()) continue;
        std::vector<int> row;
        for (char c : line) {
            row.push_back(c - '0');
        }
        grid.push_back(row);
    }
    return grid;
}

bool isVisible(const Grid& grid, int row, int col) {
    int rows = grid.size();
    int cols = grid[0].size();
    int height = grid[row][col];

    // Check from left
    bool visibleLeft = true;
    for (int c = 0; c < col; ++c) {
        if (grid[row][c] >= height) {
            visibleLeft = false;
            break;
        }
    }
    if (visibleLeft) return true;

    // Check from right
    bool visibleRight = true;
    for (int c = col + 1; c < cols; ++c) {
        if (grid[row][c] >= height) {
            visibleRight = false;
            break;
        }
    }
    if (visibleRight) return true;

    // Check from top
    bool visibleTop = true;
    for (int r = 0; r < row; ++r) {
        if (grid[r][col] >= height) {
            visibleTop = false;
            break;
        }
    }
    if (visibleTop) return true;

    // Check from bottom
    bool visibleBottom = true;
    for (int r = row + 1; r < rows; ++r) {
        if (grid[r][col] >= height) {
            visibleBottom = false;
            break;
        }
    }
    return visibleBottom;
}

int scenicScore(const Grid& grid, int row, int col) {
    int rows = grid.size();
    int cols = grid[0].size();
    int height = grid[row][col];

    // Count trees visible in each direction
    // Left
    int left = 0;
    for (int c = col - 1; c >= 0; --c) {
        ++left;
        if (grid[row][c] >= height) break;
    }

    // Right
    int right = 0;
    for (int c = col + 1; c < cols; ++c) {
        ++right;
        if (grid[row][c] >= height) break;
    }

    // Up
    int up = 0;
    for (int r = row - 1; r >= 0; --r) {
        ++up;
        if (grid[r][col] >= height) break;
    }

    // Down
    int down = 0;
    for (int r = row + 1; r < rows; ++r) {
        ++down;
        if (grid[r][col] >= height) break;
    }

    return left * right * up * down;
}

int part1(const Grid& grid) {
    int rows = grid.size();
    int cols = grid[0].size();
    int count = 0;

    for (int r = 0; r < rows; ++r) {
        for (int c = 0; c < cols; ++c) {
            if (isVisible(grid, r, c)) {
                ++count;
            }
        }
    }
    return count;
}

int part2(const Grid& grid) {
    int rows = grid.size();
    int cols = grid[0].size();
    int maxScore = 0;

    for (int r = 0; r < rows; ++r) {
        for (int c = 0; c < cols; ++c) {
            int score = scenicScore(grid, r, c);
            maxScore = std::max(maxScore, score);
        }
    }
    return maxScore;
}

int main() {
    Grid grid = parseGrid("../input.txt");

    std::cout << "Part 1: " << part1(grid) << std::endl;
    std::cout << "Part 2: " << part2(grid) << std::endl;

    return 0;
}
