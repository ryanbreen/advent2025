#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <tuple>
#include <set>
#include <queue>
#include <algorithm>
#include <numeric>

struct Blizzard {
    int row, col;
    char direction;
};

std::tuple<std::vector<Blizzard>, int, int, int, int, std::pair<int,int>, std::pair<int,int>>
parseInput(const std::string& text) {
    std::vector<std::string> lines;
    std::istringstream iss(text);
    std::string line;
    while (std::getline(iss, line)) {
        if (!line.empty()) {
            lines.push_back(line);
        }
    }

    int height = static_cast<int>(lines.size());
    int width = static_cast<int>(lines[0].size());
    int innerH = height - 2;
    int innerW = width - 2;

    std::vector<Blizzard> blizzards;
    for (int r = 0; r < height; ++r) {
        for (int c = 0; c < static_cast<int>(lines[r].size()); ++c) {
            char ch = lines[r][c];
            if (ch == '^' || ch == 'v' || ch == '<' || ch == '>') {
                blizzards.push_back({r, c, ch});
            }
        }
    }

    // Find start and end positions
    int startCol = static_cast<int>(lines[0].find('.'));
    int endCol = static_cast<int>(lines[height-1].find('.'));
    std::pair<int,int> start = {0, startCol};
    std::pair<int,int> end = {height - 1, endCol};

    return {blizzards, height, width, innerH, innerW, start, end};
}

int mod(int a, int b) {
    return ((a % b) + b) % b;
}

std::set<std::pair<int,int>> getBlizzardPositions(
    const std::vector<Blizzard>& blizzards,
    int innerH, int innerW, int time) {

    std::set<std::pair<int,int>> positions;

    for (const auto& b : blizzards) {
        // Adjust to inner coordinates (subtract 1 for wall)
        int ir = b.row - 1;
        int ic = b.col - 1;
        int nr, nc;

        switch (b.direction) {
            case '^':
                nr = mod(ir - time, innerH);
                nc = ic;
                break;
            case 'v':
                nr = mod(ir + time, innerH);
                nc = ic;
                break;
            case '<':
                nr = ir;
                nc = mod(ic - time, innerW);
                break;
            case '>':
                nr = ir;
                nc = mod(ic + time, innerW);
                break;
            default:
                continue;
        }

        // Convert back to full coordinates
        positions.insert({nr + 1, nc + 1});
    }

    return positions;
}

int bfs(const std::vector<Blizzard>& blizzards, int height, int width,
        int innerH, int innerW,
        std::pair<int,int> start, std::pair<int,int> end,
        int startTime) {

    int period = std::lcm(innerH, innerW);

    // Precompute blizzard positions for all times in one period
    std::vector<std::set<std::pair<int,int>>> blizzardCache(period);
    for (int t = 0; t < period; ++t) {
        blizzardCache[t] = getBlizzardPositions(blizzards, innerH, innerW, t);
    }

    // BFS: state = (time % period, row, col)
    std::queue<std::tuple<int, int, int>> q;
    std::set<std::tuple<int, int, int>> visited;

    q.push({startTime, start.first, start.second});
    visited.insert({startTime % period, start.first, start.second});

    // Directions: wait, up, down, left, right
    std::vector<std::pair<int,int>> directions = {{0, 0}, {-1, 0}, {1, 0}, {0, -1}, {0, 1}};

    while (!q.empty()) {
        auto [time, r, c] = q.front();
        q.pop();

        if (r == end.first && c == end.second) {
            return time;
        }

        int nextTime = time + 1;
        const auto& nextBlizzards = blizzardCache[nextTime % period];

        for (const auto& [dr, dc] : directions) {
            int nr = r + dr;
            int nc = c + dc;

            // Check bounds
            bool valid = false;
            if (nr == start.first && nc == start.second) {
                valid = true;  // Start position is always valid
            } else if (nr == end.first && nc == end.second) {
                valid = true;  // End position is always valid
            } else if (nr > 0 && nr < height - 1 && nc > 0 && nc < width - 1) {
                valid = true;  // Inside the grid (not on walls)
            }

            if (!valid) continue;

            // Check blizzards
            if (nextBlizzards.count({nr, nc})) continue;

            auto state = std::make_tuple(nextTime % period, nr, nc);
            if (!visited.count(state)) {
                visited.insert(state);
                q.push({nextTime, nr, nc});
            }
        }
    }

    return -1;  // No path found
}

int part1(const std::string& text) {
    auto [blizzards, height, width, innerH, innerW, start, end] = parseInput(text);
    return bfs(blizzards, height, width, innerH, innerW, start, end, 0);
}

int part2(const std::string& text) {
    auto [blizzards, height, width, innerH, innerW, start, end] = parseInput(text);

    // Trip 1: start to end
    int t1 = bfs(blizzards, height, width, innerH, innerW, start, end, 0);

    // Trip 2: end to start
    int t2 = bfs(blizzards, height, width, innerH, innerW, end, start, t1);

    // Trip 3: start to end again
    int t3 = bfs(blizzards, height, width, innerH, innerW, start, end, t2);

    return t3;
}

int main() {
    std::ifstream file("../input.txt");
    if (!file) {
        std::cerr << "Error: Cannot open input file" << std::endl;
        return 1;
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string text = buffer.str();

    std::cout << "Part 1: " << part1(text) << std::endl;
    std::cout << "Part 2: " << part2(text) << std::endl;

    return 0;
}
