#include <iostream>
#include <fstream>
#include <vector>
#include <string>
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

std::pair<std::vector<int>, std::vector<int>> build_prefix_sums(
    const std::vector<std::string>& lines,
    const std::vector<Galaxy>& galaxies) {

    const int rows = static_cast<int>(lines.size());
    const int cols = rows > 0 ? static_cast<int>(lines[0].size()) : 0;

    // Track which rows and columns contain galaxies
    std::vector<bool> empty_rows(rows, true);
    std::vector<bool> empty_cols(cols, true);

    for (const auto& g : galaxies) {
        empty_rows[g.row] = false;
        empty_cols[g.col] = false;
    }

    // Build prefix sums for O(1) range queries
    std::vector<int> prefix_rows(rows + 1, 0);
    for (int r = 0; r < rows; ++r) {
        prefix_rows[r + 1] = prefix_rows[r] + (empty_rows[r] ? 1 : 0);
    }

    std::vector<int> prefix_cols(cols + 1, 0);
    for (int c = 0; c < cols; ++c) {
        prefix_cols[c + 1] = prefix_cols[c] + (empty_cols[c] ? 1 : 0);
    }

    return {prefix_rows, prefix_cols};
}

std::int64_t calculate_distances(const std::vector<Galaxy>& galaxies,
                                  const std::vector<int>& prefix_rows,
                                  const std::vector<int>& prefix_cols,
                                  const std::int64_t expansion_factor) {
    std::int64_t total = 0;

    for (std::size_t i = 0; i < galaxies.size(); ++i) {
        for (std::size_t j = i + 1; j < galaxies.size(); ++j) {
            const int r1 = galaxies[i].row;
            const int c1 = galaxies[i].col;
            const int r2 = galaxies[j].row;
            const int c2 = galaxies[j].col;

            // Calculate row distance with expansion using prefix sums
            const int min_r = std::min(r1, r2);
            const int max_r = std::max(r1, r2);
            const int empty_row_count = prefix_rows[max_r] - prefix_rows[min_r];
            const std::int64_t row_dist = (max_r - min_r) +
                static_cast<std::int64_t>(empty_row_count) * (expansion_factor - 1);

            // Calculate column distance with expansion using prefix sums
            const int min_c = std::min(c1, c2);
            const int max_c = std::max(c1, c2);
            const int empty_col_count = prefix_cols[max_c] - prefix_cols[min_c];
            const std::int64_t col_dist = (max_c - min_c) +
                static_cast<std::int64_t>(empty_col_count) * (expansion_factor - 1);

            total += row_dist + col_dist;
        }
    }

    return total;
}

std::int64_t part1(const std::vector<Galaxy>& galaxies,
                   const std::vector<int>& prefix_rows,
                   const std::vector<int>& prefix_cols) {
    return calculate_distances(galaxies, prefix_rows, prefix_cols, 2);
}

std::int64_t part2(const std::vector<Galaxy>& galaxies,
                   const std::vector<int>& prefix_rows,
                   const std::vector<int>& prefix_cols) {
    return calculate_distances(galaxies, prefix_rows, prefix_cols, 1000000);
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
    const auto [prefix_rows, prefix_cols] = build_prefix_sums(lines, galaxies);

    std::cout << "Part 1: " << part1(galaxies, prefix_rows, prefix_cols) << std::endl;
    std::cout << "Part 2: " << part2(galaxies, prefix_rows, prefix_cols) << std::endl;

    return 0;
}
