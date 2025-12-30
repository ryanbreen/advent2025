#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <unordered_map>

void tiltNorth(std::vector<std::string>& grid) {
    int rows = static_cast<int>(grid.size());
    int cols = static_cast<int>(grid[0].size());

    for (int col = 0; col < cols; ++col) {
        int writePos = 0;
        for (int row = 0; row < rows; ++row) {
            if (grid[row][col] == '#') {
                writePos = row + 1;
            } else if (grid[row][col] == 'O') {
                grid[row][col] = '.';
                grid[writePos][col] = 'O';
                ++writePos;
            }
        }
    }
}

void tiltSouth(std::vector<std::string>& grid) {
    int rows = static_cast<int>(grid.size());
    int cols = static_cast<int>(grid[0].size());

    for (int col = 0; col < cols; ++col) {
        int writePos = rows - 1;
        for (int row = rows - 1; row >= 0; --row) {
            if (grid[row][col] == '#') {
                writePos = row - 1;
            } else if (grid[row][col] == 'O') {
                grid[row][col] = '.';
                grid[writePos][col] = 'O';
                --writePos;
            }
        }
    }
}

void tiltWest(std::vector<std::string>& grid) {
    int rows = static_cast<int>(grid.size());
    int cols = static_cast<int>(grid[0].size());

    for (int row = 0; row < rows; ++row) {
        int writePos = 0;
        for (int col = 0; col < cols; ++col) {
            if (grid[row][col] == '#') {
                writePos = col + 1;
            } else if (grid[row][col] == 'O') {
                grid[row][col] = '.';
                grid[row][writePos] = 'O';
                ++writePos;
            }
        }
    }
}

void tiltEast(std::vector<std::string>& grid) {
    int rows = static_cast<int>(grid.size());
    int cols = static_cast<int>(grid[0].size());

    for (int row = 0; row < rows; ++row) {
        int writePos = cols - 1;
        for (int col = cols - 1; col >= 0; --col) {
            if (grid[row][col] == '#') {
                writePos = col - 1;
            } else if (grid[row][col] == 'O') {
                grid[row][col] = '.';
                grid[row][writePos] = 'O';
                --writePos;
            }
        }
    }
}

void spinCycle(std::vector<std::string>& grid) {
    tiltNorth(grid);
    tiltWest(grid);
    tiltSouth(grid);
    tiltEast(grid);
}

long long calculateLoad(const std::vector<std::string>& grid) {
    long long load = 0;
    int rows = static_cast<int>(grid.size());

    for (int row = 0; row < rows; ++row) {
        for (char c : grid[row]) {
            if (c == 'O') {
                load += rows - row;
            }
        }
    }
    return load;
}

std::string gridToString(const std::vector<std::string>& grid) {
    std::string result;
    for (const auto& row : grid) {
        result += row;
    }
    return result;
}

long long part1(std::vector<std::string> grid) {
    tiltNorth(grid);
    return calculateLoad(grid);
}

long long part2(std::vector<std::string> grid) {
    std::unordered_map<std::string, long long> seen;
    const long long targetCycles = 1000000000;

    for (long long cycle = 0; cycle < targetCycles; ++cycle) {
        std::string key = gridToString(grid);

        auto it = seen.find(key);
        if (it != seen.end()) {
            long long cycleStart = it->second;
            long long cycleLength = cycle - cycleStart;
            long long remaining = (targetCycles - cycle) % cycleLength;

            for (long long i = 0; i < remaining; ++i) {
                spinCycle(grid);
            }
            return calculateLoad(grid);
        }

        seen[key] = cycle;
        spinCycle(grid);
    }

    return calculateLoad(grid);
}

int main() {
    std::ifstream file("../input.txt");
    if (!file.is_open()) {
        std::cerr << "Error: Could not open input file" << std::endl;
        return 1;
    }

    std::vector<std::string> grid;
    std::string line;
    while (std::getline(file, line)) {
        if (!line.empty()) {
            grid.push_back(line);
        }
    }
    file.close();

    std::cout << "Part 1: " << part1(grid) << std::endl;
    std::cout << "Part 2: " << part2(grid) << std::endl;

    return 0;
}
