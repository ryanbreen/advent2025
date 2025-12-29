#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <unordered_map>
#include <cstdint>

constexpr int UNFOLD_FACTOR = 5;

struct State {
    int pos;
    int group_idx;
    int current_run;

    bool operator==(const State& other) const {
        return pos == other.pos && group_idx == other.group_idx && current_run == other.current_run;
    }
};

struct StateHash {
    std::size_t operator()(const State& s) const {
        return std::hash<int>()(s.pos) ^
               (std::hash<int>()(s.group_idx) << 10) ^
               (std::hash<int>()(s.current_run) << 20);
    }
};

class ArrangementCounter {
private:
    const std::string& pattern;
    const std::vector<int>& groups;
    const int pattern_size;
    const int groups_size;
    std::unordered_map<State, int64_t, StateHash> memo;

public:
    ArrangementCounter(const std::string& p, const std::vector<int>& g)
        : pattern(p), groups(g),
          pattern_size(static_cast<int>(p.size())),
          groups_size(static_cast<int>(g.size())) {}

    int64_t count() {
        memo.clear();
        return dp(0, 0, 0);
    }

private:
    int64_t dp(int pos, int group_idx, int current_run) {
        // Base case: reached end of pattern
        if (pos == pattern_size) {
            // Valid if we've matched all groups and no partial run
            if (group_idx == groups_size && current_run == 0) {
                return 1;
            }
            // Or if we're on the last group and the run matches
            if (group_idx == groups_size - 1 &&
                groups[group_idx] == current_run) {
                return 1;
            }
            return 0;
        }

        State state{pos, group_idx, current_run};
        auto it = memo.find(state);
        if (it != memo.end()) {
            return it->second;
        }

        int64_t result = 0;
        char c = pattern[pos];

        // Option 1: Place operational spring (.)
        if (c == '.' || c == '?') {
            if (current_run == 0) {
                // No active run, just move forward
                result += dp(pos + 1, group_idx, 0);
            } else if (group_idx < groups_size &&
                       groups[group_idx] == current_run) {
                // End current run if it matches expected group size
                result += dp(pos + 1, group_idx + 1, 0);
            }
            // Otherwise invalid (run doesn't match group)
        }

        // Option 2: Place damaged spring (#)
        if (c == '#' || c == '?') {
            if (group_idx < groups_size &&
                current_run < groups[group_idx]) {
                // Can extend current run
                result += dp(pos + 1, group_idx, current_run + 1);
            }
            // Otherwise invalid (exceeds group size or no more groups)
        }

        memo[state] = result;
        return result;
    }
};

std::pair<std::string, std::vector<int>> parse_line(const std::string& line) {
    std::istringstream iss(line);
    std::string pattern;
    std::string groups_str;
    iss >> pattern >> groups_str;

    std::vector<int> groups;
    std::istringstream gss(groups_str);
    std::string num;
    while (std::getline(gss, num, ',')) {
        groups.push_back(std::stoi(num));
    }

    return {pattern, groups};
}

std::pair<std::string, std::vector<int>> unfold(const std::string& pattern,
                                                 const std::vector<int>& groups,
                                                 int times = UNFOLD_FACTOR) {
    std::string unfolded_pattern;
    unfolded_pattern.reserve(pattern.size() * times + times - 1);
    for (int i = 0; i < times; i++) {
        if (i > 0) unfolded_pattern += '?';
        unfolded_pattern += pattern;
    }

    std::vector<int> unfolded_groups;
    unfolded_groups.reserve(groups.size() * times);
    for (int i = 0; i < times; i++) {
        unfolded_groups.insert(unfolded_groups.end(), groups.begin(), groups.end());
    }

    return {unfolded_pattern, unfolded_groups};
}

int64_t part1(const std::vector<std::string>& lines) {
    int64_t total = 0;
    for (const auto& line : lines) {
        if (line.empty()) continue;
        auto [pattern, groups] = parse_line(line);
        ArrangementCounter counter(pattern, groups);
        total += counter.count();
    }
    return total;
}

int64_t part2(const std::vector<std::string>& lines) {
    int64_t total = 0;
    for (const auto& line : lines) {
        if (line.empty()) continue;
        auto [pattern, groups] = parse_line(line);
        auto [unfolded_pattern, unfolded_groups] = unfold(pattern, groups);
        ArrangementCounter counter(unfolded_pattern, unfolded_groups);
        total += counter.count();
    }
    return total;
}

int main() {
    std::ifstream file("../input.txt");
    if (!file.is_open()) {
        std::cerr << "Error: Could not open input.txt" << std::endl;
        return 1;
    }

    std::vector<std::string> lines;
    std::string line;
    while (std::getline(file, line)) {
        lines.push_back(line);
    }
    file.close();

    std::cout << "Part 1: " << part1(lines) << std::endl;
    std::cout << "Part 2: " << part2(lines) << std::endl;

    return 0;
}
