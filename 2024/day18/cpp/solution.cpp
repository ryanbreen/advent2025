#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <queue>
#include <unordered_set>
#include <string>

constexpr int SIZE = 71;

// Custom hash for pair<int, int>
struct PairHash {
    std::size_t operator()(const std::pair<int, int>& p) const {
        return std::hash<int>()(p.first) ^ (std::hash<int>()(p.second) << 16);
    }
};

using Position = std::pair<int, int>;
using PositionSet = std::unordered_set<Position, PairHash>;

std::vector<Position> parseInput(const std::string& filename) {
    std::vector<Position> positions;
    std::ifstream file(filename);
    std::string line;

    while (std::getline(file, line)) {
        if (line.empty()) continue;

        std::size_t commaPos = line.find(',');
        int x = std::stoi(line.substr(0, commaPos));
        int y = std::stoi(line.substr(commaPos + 1));
        positions.emplace_back(x, y);
    }

    return positions;
}

int bfs(const PositionSet& corrupted) {
    Position start{0, 0};
    Position goal{SIZE - 1, SIZE - 1};

    if (corrupted.count(start) || corrupted.count(goal)) {
        return -1;
    }

    std::queue<std::pair<Position, int>> queue;
    PositionSet visited;

    queue.push({start, 0});
    visited.insert(start);

    const int dx[] = {0, 0, 1, -1};
    const int dy[] = {1, -1, 0, 0};

    while (!queue.empty()) {
        auto [pos, steps] = queue.front();
        queue.pop();

        if (pos == goal) {
            return steps;
        }

        for (int i = 0; i < 4; ++i) {
            int nx = pos.first + dx[i];
            int ny = pos.second + dy[i];
            Position next{nx, ny};

            if (nx >= 0 && nx < SIZE && ny >= 0 && ny < SIZE &&
                !visited.count(next) && !corrupted.count(next)) {
                visited.insert(next);
                queue.push({next, steps + 1});
            }
        }
    }

    return -1;
}

int part1(const std::vector<Position>& positions, int numBytes = 1024) {
    PositionSet corrupted(positions.begin(), positions.begin() + numBytes);
    return bfs(corrupted);
}

std::string part2(const std::vector<Position>& positions) {
    // Binary search to find the first byte that blocks the path
    int left = 0;
    int right = static_cast<int>(positions.size());

    while (left < right) {
        int mid = (left + right) / 2;
        PositionSet corrupted(positions.begin(), positions.begin() + mid + 1);

        if (bfs(corrupted) == -1) {
            right = mid;
        } else {
            left = mid + 1;
        }
    }

    const auto& blockingPos = positions[left];
    return std::to_string(blockingPos.first) + "," + std::to_string(blockingPos.second);
}

int main() {
    auto positions = parseInput("../input.txt");

    std::cout << "Part 1: " << part1(positions) << std::endl;
    std::cout << "Part 2: " << part2(positions) << std::endl;

    return 0;
}
