#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <set>
#include <queue>
#include <utility>

using namespace std;

struct Grid {
    vector<string> data;
    int rows;
    int cols;

    Grid(const vector<string>& lines) : data(lines) {
        rows = data.size();
        cols = data.empty() ? 0 : data[0].size();
    }

    char at(int r, int c) const {
        if (r < 0 || r >= rows || c < 0 || c >= cols) return '\0';
        return data[r][c];
    }
};

using Cell = pair<int, int>;
using Region = set<Cell>;

vector<Region> find_regions(const Grid& grid) {
    set<Cell> visited;
    vector<Region> regions;

    const int dr[] = {0, 0, 1, -1};
    const int dc[] = {1, -1, 0, 0};

    for (int r = 0; r < grid.rows; r++) {
        for (int c = 0; c < grid.cols; c++) {
            if (visited.count({r, c})) continue;

            // BFS to find all cells in this region
            char plant = grid.at(r, c);
            Region region;
            queue<Cell> q;
            q.push({r, c});

            while (!q.empty()) {
                auto [cr, cc] = q.front();
                q.pop();

                if (visited.count({cr, cc})) continue;
                if (cr < 0 || cr >= grid.rows || cc < 0 || cc >= grid.cols) continue;
                if (grid.at(cr, cc) != plant) continue;

                visited.insert({cr, cc});
                region.insert({cr, cc});

                for (int i = 0; i < 4; i++) {
                    int nr = cr + dr[i];
                    int nc = cc + dc[i];
                    if (!visited.count({nr, nc})) {
                        q.push({nr, nc});
                    }
                }
            }

            regions.push_back(region);
        }
    }

    return regions;
}

int calculate_perimeter(const Region& region) {
    int perimeter = 0;
    const int dr[] = {0, 0, 1, -1};
    const int dc[] = {1, -1, 0, 0};

    for (const auto& [r, c] : region) {
        for (int i = 0; i < 4; i++) {
            int nr = r + dr[i];
            int nc = c + dc[i];
            if (!region.count({nr, nc})) {
                perimeter++;
            }
        }
    }

    return perimeter;
}

int count_sides(const Region& region) {
    int corners = 0;

    for (const auto& [r, c] : region) {
        // Check all 8 neighbors
        bool up = region.count({r - 1, c});
        bool down = region.count({r + 1, c});
        bool left = region.count({r, c - 1});
        bool right = region.count({r, c + 1});
        bool up_left = region.count({r - 1, c - 1});
        bool up_right = region.count({r - 1, c + 1});
        bool down_left = region.count({r + 1, c - 1});
        bool down_right = region.count({r + 1, c + 1});

        // Top-left corner
        if (!up && !left) corners++;  // convex
        else if (up && left && !up_left) corners++;  // concave

        // Top-right corner
        if (!up && !right) corners++;  // convex
        else if (up && right && !up_right) corners++;  // concave

        // Bottom-left corner
        if (!down && !left) corners++;  // convex
        else if (down && left && !down_left) corners++;  // concave

        // Bottom-right corner
        if (!down && !right) corners++;  // convex
        else if (down && right && !down_right) corners++;  // concave
    }

    return corners;
}

long long part1(const Grid& grid) {
    auto regions = find_regions(grid);
    long long total = 0;

    for (const auto& region : regions) {
        int area = region.size();
        int perimeter = calculate_perimeter(region);
        total += static_cast<long long>(area) * perimeter;
    }

    return total;
}

long long part2(const Grid& grid) {
    auto regions = find_regions(grid);
    long long total = 0;

    for (const auto& region : regions) {
        int area = region.size();
        int sides = count_sides(region);
        total += static_cast<long long>(area) * sides;
    }

    return total;
}

int main() {
    // Read input
    ifstream infile("../input.txt");
    if (!infile) {
        cerr << "Error opening input file" << endl;
        return 1;
    }

    vector<string> lines;
    string line;
    while (getline(infile, line)) {
        if (!line.empty()) {
            lines.push_back(line);
        }
    }

    Grid grid(lines);

    cout << "Part 1: " << part1(grid) << endl;
    cout << "Part 2: " << part2(grid) << endl;

    return 0;
}
