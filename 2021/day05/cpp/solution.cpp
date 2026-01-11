#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <unordered_map>

struct Line {
    int x1, y1, x2, y2;
};

int sign(int x) {
    if (x > 0) return 1;
    if (x < 0) return -1;
    return 0;
}

struct PairHash {
    size_t operator()(const std::pair<int, int>& p) const {
        return std::hash<long long>()(static_cast<long long>(p.first) << 32 | static_cast<unsigned int>(p.second));
    }
};

std::vector<Line> parseInput() {
    std::vector<Line> lines;
    std::ifstream file("../input.txt");
    std::string line;

    while (std::getline(file, line)) {
        if (line.empty()) continue;

        Line l;
        char comma, arrow1, arrow2;
        std::istringstream iss(line);
        iss >> l.x1 >> comma >> l.y1 >> arrow1 >> arrow2 >> l.x2 >> comma >> l.y2;
        lines.push_back(l);
    }

    return lines;
}

int countOverlaps(const std::vector<Line>& lines, bool includeDiagonals) {
    std::unordered_map<std::pair<int, int>, int, PairHash> grid;

    for (const auto& line : lines) {
        int dx = sign(line.x2 - line.x1);
        int dy = sign(line.y2 - line.y1);

        // Skip diagonals in part 1
        if (!includeDiagonals && dx != 0 && dy != 0) {
            continue;
        }

        int x = line.x1;
        int y = line.y1;

        while (true) {
            grid[{x, y}]++;
            if (x == line.x2 && y == line.y2) {
                break;
            }
            x += dx;
            y += dy;
        }
    }

    int count = 0;
    for (const auto& entry : grid) {
        if (entry.second >= 2) {
            count++;
        }
    }

    return count;
}

int part1(const std::vector<Line>& lines) {
    return countOverlaps(lines, false);
}

int part2(const std::vector<Line>& lines) {
    return countOverlaps(lines, true);
}

int main() {
    std::vector<Line> lines = parseInput();

    std::cout << "Part 1: " << part1(lines) << std::endl;
    std::cout << "Part 2: " << part2(lines) << std::endl;

    return 0;
}
