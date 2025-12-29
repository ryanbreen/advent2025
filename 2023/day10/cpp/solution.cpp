#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <queue>
#include <unordered_map>
#include <unordered_set>
#include <array>
#include <algorithm>
#include <utility>

// Type alias for coordinates
using Coord = std::pair<int, int>;

// Hash function for Coord to use with unordered containers
struct CoordHash {
    std::size_t operator()(const Coord& c) const noexcept {
        return std::hash<int>()(c.first) ^ (std::hash<int>()(c.second) << 16);
    }
};

// Direction offsets: N, S, W, E
static constexpr std::array<Coord, 4> DIRECTIONS = {{{-1, 0}, {1, 0}, {0, -1}, {0, 1}}};

// Pipe connections: each pipe type maps to which directions it connects
static const std::unordered_map<char, std::array<Coord, 2>> PIPE_CONNECTIONS = {
    {'|', {{{-1, 0}, {1, 0}}}},   // N, S
    {'-', {{{0, -1}, {0, 1}}}},   // W, E
    {'L', {{{-1, 0}, {0, 1}}}},   // N, E
    {'J', {{{-1, 0}, {0, -1}}}},  // N, W
    {'7', {{{1, 0}, {0, -1}}}},   // S, W
    {'F', {{{1, 0}, {0, 1}}}}     // S, E
};

Coord findStart(const std::vector<std::string>& grid) {
    for (int r = 0; r < static_cast<int>(grid.size()); r++) {
        for (int c = 0; c < static_cast<int>(grid[r].size()); c++) {
            if (grid[r][c] == 'S') {
                return {r, c};
            }
        }
    }
    return {-1, -1};
}

std::vector<Coord> getNeighbors(const std::vector<std::string>& grid, const Coord& pos) {
    const int r = pos.first;
    const int c = pos.second;
    const int rows = static_cast<int>(grid.size());
    const int cols = static_cast<int>(grid[0].size());
    const char ch = grid[r][c];
    std::vector<Coord> neighbors;

    if (ch == 'S') {
        // S can connect to any adjacent pipe that connects back to it
        for (const auto& [dr, dc] : DIRECTIONS) {
            const int nr = r + dr;
            const int nc = c + dc;
            if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
                const char adjCh = grid[nr][nc];
                auto it = PIPE_CONNECTIONS.find(adjCh);
                if (it != PIPE_CONNECTIONS.end()) {
                    // Check if adjacent pipe connects back to S
                    for (const auto& [adjDr, adjDc] : it->second) {
                        if (nr + adjDr == r && nc + adjDc == c) {
                            neighbors.push_back({nr, nc});
                            break;
                        }
                    }
                }
            }
        }
    } else {
        auto it = PIPE_CONNECTIONS.find(ch);
        if (it != PIPE_CONNECTIONS.end()) {
            for (const auto& [dr, dc] : it->second) {
                const int nr = r + dr;
                const int nc = c + dc;
                if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
                    neighbors.push_back({nr, nc});
                }
            }
        }
    }

    return neighbors;
}

std::unordered_map<Coord, int, CoordHash> findLoop(const std::vector<std::string>& grid, const Coord& start) {
    std::unordered_map<Coord, int, CoordHash> distances;
    std::queue<Coord> q;

    distances[start] = 0;
    q.push(start);

    while (!q.empty()) {
        const auto pos = q.front();
        q.pop();

        for (const auto& neighbor : getNeighbors(grid, pos)) {
            if (distances.find(neighbor) == distances.end()) {
                distances[neighbor] = distances[pos] + 1;
                q.push(neighbor);
            }
        }
    }

    return distances;
}

char determineStartPipe(const std::vector<std::string>& grid, const Coord& start,
                        const std::unordered_set<Coord, CoordHash>& loopPositions) {
    const int r = start.first;
    const int c = start.second;

    std::unordered_set<Coord, CoordHash> connections;
    for (const auto& [dr, dc] : DIRECTIONS) {
        const int nr = r + dr;
        const int nc = c + dc;
        if (loopPositions.count({nr, nc})) {
            const char adjCh = grid[nr][nc];
            auto it = PIPE_CONNECTIONS.find(adjCh);
            if (it != PIPE_CONNECTIONS.end()) {
                // Check if this pipe connects back to S
                for (const auto& [adjDr, adjDc] : it->second) {
                    if (nr + adjDr == r && nc + adjDc == c) {
                        connections.insert({dr, dc});
                        break;
                    }
                }
            }
        }
    }

    for (const auto& [pipe, dirs] : PIPE_CONNECTIONS) {
        std::unordered_set<Coord, CoordHash> pipeDirections(dirs.begin(), dirs.end());
        if (pipeDirections == connections) {
            return pipe;
        }
    }

    return 'S';
}

int part1(const std::vector<std::string>& grid) {
    const auto start = findStart(grid);
    const auto distances = findLoop(grid, start);

    auto maxIt = std::max_element(distances.begin(), distances.end(),
        [](const auto& a, const auto& b) { return a.second < b.second; });
    return maxIt != distances.end() ? maxIt->second : 0;
}

int part2(const std::vector<std::string>& grid) {
    const auto start = findStart(grid);
    const auto distances = findLoop(grid, start);

    std::unordered_set<Coord, CoordHash> loopPositions;
    for (const auto& [pos, dist] : distances) {
        loopPositions.insert(pos);
    }

    // Replace S with its actual pipe type
    const char startPipe = determineStartPipe(grid, start, loopPositions);
    std::vector<std::string> modifiedGrid = grid;
    modifiedGrid[start.first][start.second] = startPipe;

    const int rows = static_cast<int>(modifiedGrid.size());
    const int cols = static_cast<int>(modifiedGrid[0].size());
    int enclosed = 0;

    for (int r = 0; r < rows; r++) {
        bool inside = false;
        for (int c = 0; c < cols; c++) {
            if (loopPositions.count({r, c})) {
                const char ch = modifiedGrid[r][c];
                // Count vertical crossings: pipes that have a north connection
                if (ch == '|' || ch == 'L' || ch == 'J') {
                    inside = !inside;
                }
            } else {
                if (inside) {
                    enclosed++;
                }
            }
        }
    }

    return enclosed;
}

int main() {
    std::ifstream inputFile("../input.txt");
    if (!inputFile) {
        std::cerr << "Error: Cannot open input file" << std::endl;
        return 1;
    }

    std::vector<std::string> grid;
    std::string line;
    while (std::getline(inputFile, line)) {
        if (!line.empty()) {
            grid.push_back(line);
        }
    }
    // RAII handles file closing

    std::cout << "Part 1: " << part1(grid) << std::endl;
    std::cout << "Part 2: " << part2(grid) << std::endl;

    return 0;
}
