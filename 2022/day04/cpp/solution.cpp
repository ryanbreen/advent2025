#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <tuple>

struct Range {
    int start;
    int end;
};

std::vector<std::pair<Range, Range>> parseInput(const std::string& filename) {
    std::vector<std::pair<Range, Range>> pairs;
    std::ifstream file(filename);
    std::string line;

    while (std::getline(file, line)) {
        if (line.empty()) continue;

        Range r1, r2;
        char dash1, dash2, comma;
        std::istringstream iss(line);
        iss >> r1.start >> dash1 >> r1.end >> comma >> r2.start >> dash2 >> r2.end;

        pairs.push_back({r1, r2});
    }

    return pairs;
}

bool fullyContains(const Range& r1, const Range& r2) {
    // Check if one range fully contains the other
    return (r1.start <= r2.start && r1.end >= r2.end) ||
           (r2.start <= r1.start && r2.end >= r1.end);
}

bool overlaps(const Range& r1, const Range& r2) {
    // Check if ranges overlap at all
    return r1.start <= r2.end && r2.start <= r1.end;
}

int part1(const std::vector<std::pair<Range, Range>>& pairs) {
    int count = 0;
    for (const auto& [r1, r2] : pairs) {
        if (fullyContains(r1, r2)) {
            count++;
        }
    }
    return count;
}

int part2(const std::vector<std::pair<Range, Range>>& pairs) {
    int count = 0;
    for (const auto& [r1, r2] : pairs) {
        if (overlaps(r1, r2)) {
            count++;
        }
    }
    return count;
}

int main() {
    auto pairs = parseInput("../input.txt");

    std::cout << "Part 1: " << part1(pairs) << std::endl;
    std::cout << "Part 2: " << part2(pairs) << std::endl;

    return 0;
}
