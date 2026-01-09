#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <queue>
#include <set>
#include <tuple>

using Grid = std::vector<std::string>;

struct State {
    int row, col, dist;
};

struct ParseResult {
    Grid grid;
    std::pair<int, int> start;
    std::pair<int, int> end;
};

ParseResult parse_grid(const std::string& text) {
    ParseResult result;
    std::istringstream iss(text);
    std::string line;

    int row = 0;
    while (std::getline(iss, line)) {
        if (line.empty()) continue;

        for (int col = 0; col < static_cast<int>(line.size()); ++col) {
            if (line[col] == 'S') {
                result.start = {row, col};
                line[col] = 'a';
            } else if (line[col] == 'E') {
                result.end = {row, col};
                line[col] = 'z';
            }
        }
        result.grid.push_back(line);
        ++row;
    }

    return result;
}

int bfs(const Grid& grid, const std::vector<std::pair<int, int>>& starts,
        const std::pair<int, int>& end) {
    int rows = static_cast<int>(grid.size());
    int cols = static_cast<int>(grid[0].size());

    std::set<std::pair<int, int>> visited;
    std::queue<State> queue;

    for (const auto& start : starts) {
        queue.push({start.first, start.second, 0});
        visited.insert(start);
    }

    const int dr[] = {-1, 1, 0, 0};
    const int dc[] = {0, 0, -1, 1};

    while (!queue.empty()) {
        auto [r, c, dist] = queue.front();
        queue.pop();

        if (r == end.first && c == end.second) {
            return dist;
        }

        char current_height = grid[r][c];

        for (int i = 0; i < 4; ++i) {
            int nr = r + dr[i];
            int nc = c + dc[i];

            if (nr >= 0 && nr < rows && nc >= 0 && nc < cols) {
                auto next_pos = std::make_pair(nr, nc);
                if (visited.find(next_pos) == visited.end()) {
                    char next_height = grid[nr][nc];
                    // Can move if destination is at most 1 higher
                    if (next_height <= current_height + 1) {
                        visited.insert(next_pos);
                        queue.push({nr, nc, dist + 1});
                    }
                }
            }
        }
    }

    return -1;  // No path found
}

int part1(const std::string& text) {
    auto [grid, start, end] = parse_grid(text);
    return bfs(grid, {start}, end);
}

int part2(const std::string& text) {
    auto [grid, start, end] = parse_grid(text);

    // Find all cells with elevation 'a'
    std::vector<std::pair<int, int>> starts;
    for (int r = 0; r < static_cast<int>(grid.size()); ++r) {
        for (int c = 0; c < static_cast<int>(grid[r].size()); ++c) {
            if (grid[r][c] == 'a') {
                starts.push_back({r, c});
            }
        }
    }

    return bfs(grid, starts, end);
}

int main() {
    std::ifstream file("../input.txt");
    if (!file) {
        std::cerr << "Error: Could not open input file" << std::endl;
        return 1;
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string text = buffer.str();

    std::cout << "Part 1: " << part1(text) << std::endl;
    std::cout << "Part 2: " << part2(text) << std::endl;

    return 0;
}
