#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <unordered_set>
#include <vector>
#include <algorithm>
#include <utility>

struct PairHash {
    size_t operator()(const std::pair<int, int>& p) const {
        return std::hash<long long>()(static_cast<long long>(p.first) << 32 |
                                      static_cast<unsigned int>(p.second));
    }
};

using BlockedSet = std::unordered_set<std::pair<int, int>, PairHash>;

void parsePaths(const std::string& text, BlockedSet& rocks, int& maxY) {
    std::istringstream stream(text);
    std::string line;
    maxY = 0;

    while (std::getline(stream, line)) {
        if (line.empty()) continue;

        std::vector<std::pair<int, int>> points;
        size_t pos = 0;

        while (pos < line.length()) {
            // Skip leading spaces
            while (pos < line.length() && line[pos] == ' ') pos++;
            if (pos >= line.length()) break;

            // Parse x coordinate
            int x = 0;
            while (pos < line.length() && isdigit(line[pos])) {
                x = x * 10 + (line[pos] - '0');
                pos++;
            }

            // Skip comma
            if (pos < line.length() && line[pos] == ',') pos++;

            // Parse y coordinate
            int y = 0;
            while (pos < line.length() && isdigit(line[pos])) {
                y = y * 10 + (line[pos] - '0');
                pos++;
            }

            points.emplace_back(x, y);
            maxY = std::max(maxY, y);

            // Skip " -> "
            while (pos < line.length() && (line[pos] == ' ' || line[pos] == '-' || line[pos] == '>')) {
                pos++;
            }
        }

        // Draw lines between consecutive points
        for (size_t i = 0; i + 1 < points.size(); i++) {
            int x1 = points[i].first, y1 = points[i].second;
            int x2 = points[i + 1].first, y2 = points[i + 1].second;

            if (x1 == x2) {
                // Vertical line
                int minY = std::min(y1, y2);
                int maxYLine = std::max(y1, y2);
                for (int y = minY; y <= maxYLine; y++) {
                    rocks.emplace(x1, y);
                }
            } else {
                // Horizontal line
                int minX = std::min(x1, x2);
                int maxX = std::max(x1, x2);
                for (int x = minX; x <= maxX; x++) {
                    rocks.emplace(x, y1);
                }
            }
        }
    }
}

// Returns resting position, or {-1, -1} if sand falls into abyss
std::pair<int, int> simulateSand(const BlockedSet& blocked, int maxY, bool floor) {
    int x = 500, y = 0;

    while (true) {
        // Check if sand has fallen below all rocks (into abyss)
        if (!floor && y > maxY) {
            return {-1, -1};
        }

        // Try to move down
        if (floor && y + 1 == maxY + 2) {
            // Hit the floor
            return {x, y};
        } else if (blocked.find({x, y + 1}) == blocked.end()) {
            y++;
        }
        // Try to move down-left
        else if (blocked.find({x - 1, y + 1}) == blocked.end()) {
            x--;
            y++;
        }
        // Try to move down-right
        else if (blocked.find({x + 1, y + 1}) == blocked.end()) {
            x++;
            y++;
        }
        // Sand comes to rest
        else {
            return {x, y};
        }
    }
}

int part1(const std::string& text) {
    BlockedSet rocks;
    int maxY;
    parsePaths(text, rocks, maxY);

    BlockedSet blocked = rocks;
    int count = 0;

    while (true) {
        auto pos = simulateSand(blocked, maxY, false);
        if (pos.first == -1) {
            break;
        }
        blocked.insert(pos);
        count++;
    }

    return count;
}

int part2(const std::string& text) {
    BlockedSet rocks;
    int maxY;
    parsePaths(text, rocks, maxY);

    BlockedSet blocked = rocks;
    int count = 0;

    while (true) {
        auto pos = simulateSand(blocked, maxY, true);
        blocked.insert(pos);
        count++;
        if (pos.first == 500 && pos.second == 0) {
            break;
        }
    }

    return count;
}

int main() {
    std::ifstream file("../input.txt");
    if (!file.is_open()) {
        std::cerr << "Error: Could not open input file" << std::endl;
        return 1;
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string text = buffer.str();
    file.close();

    std::cout << "Part 1: " << part1(text) << std::endl;
    std::cout << "Part 2: " << part2(text) << std::endl;

    return 0;
}
