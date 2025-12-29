#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <set>
#include <cstdint>
#include <algorithm>

struct Galaxy {
    int row;
    int col;
};

std::vector<Galaxy> parse_grid(const std::vector<std::string>& lines) {
    std::vector<Galaxy> galaxies;
    for (int r = 0; r < static_cast<int>(lines.size()); ++r) {
        for (int c = 0; c < static_cast<int>(lines[r].size()); ++c) {
            if (lines[r][c] == '#') {
                galaxies.push_back({r, c});
            }
        }
    }
    return galaxies;
}

std::pair<std::set<int>, std::set<int>> find_empty_rows_and_cols(const std::vector<std::string>& lines) {
    std::set<int> empty_rows;
    std::set<int> empty_cols;

    const int rows = static_cast<int>(lines.size());
    const int cols = rows > 0 ? static_cast<int>(lines[0].size()) : 0;

    // Find empty rows
    for (int r = 0; r < rows; ++r) {
        if (lines[r].find('#') == std::string::npos) {
            empty_rows.insert(r);
        }
    }

    // Find empty columns using std::none_of
    for (int c = 0; c < cols; ++c) {
        const bool is_empty = std::none_of(lines.begin(), lines.end(),
            [c](const std::string& row) { return row[c] == '#'; });
        if (is_empty) {
            empty_cols.insert(c);
        }
    }

    return {empty_rows, empty_cols};
}

std::int64_t calculate_distances(const std::vector<Galaxy>& galaxies,
                                  const std::set<int>& empty_rows,
                                  const std::set<int>& empty_cols,
                                  const std::int64_t expansion_factor) {
    std::int64_t total = 0;

    for (std::size_t i = 0; i < galaxies.size(); ++i) {
        for (std::size_t j = i + 1; j < galaxies.size(); ++j) {
            const int r1 = galaxies[i].row;
            const int c1 = galaxies[i].col;
            const int r2 = galaxies[j].row;
            const int c2 = galaxies[j].col;

            // Calculate row distance with expansion
            const int min_r = std::min(r1, r2);
            const int max_r = std::max(r1, r2);
            std::int64_t row_dist = max_r - min_r;
            for (int r = min_r; r < max_r; ++r) {
                if (empty_rows.count(r)) {
                    row_dist += expansion_factor - 1;
                }
            }

            // Calculate column distance with expansion
            const int min_c = std::min(c1, c2);
            const int max_c = std::max(c1, c2);
            std::int64_t col_dist = max_c - min_c;
            for (int c = min_c; c < max_c; ++c) {
                if (empty_cols.count(c)) {
                    col_dist += expansion_factor - 1;
                }
            }

            total += row_dist + col_dist;
        }
    }

    return total;
}

std::int64_t part1(const std::vector<Galaxy>& galaxies,
                   const std::set<int>& empty_rows,
                   const std::set<int>& empty_cols) {
    return calculate_distances(galaxies, empty_rows, empty_cols, 2);
}

std::int64_t part2(const std::vector<Galaxy>& galaxies,
                   const std::set<int>& empty_rows,
                   const std::set<int>& empty_cols) {
    return calculate_distances(galaxies, empty_rows, empty_cols, 1000000);
}

int main() {
    std::ifstream file("../input.txt");
    if (!file) {
        std::cerr << "Error: Could not open ../input.txt" << std::endl;
        return 1;
    }

    std::vector<std::string> lines;
    std::string line;
    while (std::getline(file, line)) {
        if (!line.empty()) {
            lines.push_back(line);
        }
    }

    // Parse shared data once
    const auto galaxies = parse_grid(lines);
    const auto [empty_rows, empty_cols] = find_empty_rows_and_cols(lines);

    std::cout << "Part 1: " << part1(galaxies, empty_rows, empty_cols) << std::endl;
    std::cout << "Part 2: " << part2(galaxies, empty_rows, empty_cols) << std::endl;

    return 0;
}
