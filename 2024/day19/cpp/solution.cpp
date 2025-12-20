#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>
#include <unordered_map>
#include <cstdint>

class Solver {
    std::vector<std::string> patterns;

    bool startsWith(std::string_view str, std::string_view prefix) const {
        return str.size() >= prefix.size() &&
               str.substr(0, prefix.size()) == prefix;
    }

    int64_t countWaysImpl(std::string_view design, size_t pos,
                          std::unordered_map<size_t, int64_t>& memo) {
        if (pos == design.size()) {
            return 1;
        }

        if (auto [it, inserted] = memo.try_emplace(pos, 0); !inserted) {
            return it->second;
        }

        std::string_view remaining = design.substr(pos);
        int64_t total = 0;

        for (const auto& pattern : patterns) {
            if (startsWith(remaining, pattern)) {
                total += countWaysImpl(design, pos + pattern.size(), memo);
            }
        }

        memo[pos] = total;
        return total;
    }

public:
    explicit Solver(std::vector<std::string> p) : patterns(std::move(p)) {}

    int64_t countWays(std::string_view design) {
        std::unordered_map<size_t, int64_t> memo;
        return countWaysImpl(design, 0, memo);
    }
};

std::string trim(std::string_view str) {
    size_t start = str.find_first_not_of(" ");
    if (start == std::string_view::npos) return "";
    size_t end = str.find_last_not_of(" ");
    return std::string(str.substr(start, end - start + 1));
}

int main() {
    std::ifstream file("../input.txt");
    if (!file) {
        std::cerr << "Could not open input.txt" << '\n';
        return 1;
    }

    std::string line;
    std::vector<std::string> patterns;

    // Parse patterns (first line)
    std::getline(file, line);
    std::stringstream ss(line);
    std::string pattern;
    while (std::getline(ss, pattern, ',')) {
        std::string trimmed = trim(pattern);
        if (!trimmed.empty()) {
            patterns.push_back(std::move(trimmed));
        }
    }

    // Skip blank line
    std::getline(file, line);

    // Parse designs
    std::vector<std::string> designs;
    while (std::getline(file, line)) {
        if (!line.empty()) {
            designs.push_back(std::move(line));
        }
    }

    Solver solver(std::move(patterns));

    int64_t part1 = 0;
    int64_t part2 = 0;

    for (const auto& design : designs) {
        int64_t ways = solver.countWays(design);
        if (ways > 0) {
            part1++;
        }
        part2 += ways;
    }

    std::cout << "Part 1: " << part1 << '\n';
    std::cout << "Part 2: " << part2 << '\n';

    return 0;
}
