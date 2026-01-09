#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <set>
#include <algorithm>
#include <regex>
#include <cstdlib>

struct Sensor {
    long long sx, sy, bx, by, dist;
};

std::vector<Sensor> parse_sensors(const std::string& text) {
    std::vector<Sensor> sensors;
    std::regex pattern(R"(Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+))");

    std::istringstream stream(text);
    std::string line;

    while (std::getline(stream, line)) {
        std::smatch match;
        if (std::regex_match(line, match, pattern)) {
            Sensor s;
            s.sx = std::stoll(match[1]);
            s.sy = std::stoll(match[2]);
            s.bx = std::stoll(match[3]);
            s.by = std::stoll(match[4]);
            s.dist = std::abs(s.sx - s.bx) + std::abs(s.sy - s.by);
            sensors.push_back(s);
        }
    }

    return sensors;
}

std::vector<std::pair<long long, long long>> merge_ranges(std::vector<std::pair<long long, long long>>& ranges) {
    if (ranges.empty()) return {};

    std::sort(ranges.begin(), ranges.end());
    std::vector<std::pair<long long, long long>> merged;
    merged.push_back(ranges[0]);

    for (size_t i = 1; i < ranges.size(); i++) {
        auto& last = merged.back();
        if (ranges[i].first <= last.second + 1) {
            last.second = std::max(last.second, ranges[i].second);
        } else {
            merged.push_back(ranges[i]);
        }
    }

    return merged;
}

std::vector<std::pair<long long, long long>> get_coverage_at_row(const std::vector<Sensor>& sensors, long long row) {
    std::vector<std::pair<long long, long long>> ranges;

    for (const auto& s : sensors) {
        long long row_dist = std::abs(s.sy - row);
        if (row_dist > s.dist) continue;

        long long x_spread = s.dist - row_dist;
        ranges.push_back({s.sx - x_spread, s.sx + x_spread});
    }

    return merge_ranges(ranges);
}

long long part1(const std::string& text) {
    auto sensors = parse_sensors(text);
    long long target_row = 2000000;

    auto ranges = get_coverage_at_row(sensors, target_row);

    long long total = 0;
    for (const auto& r : ranges) {
        total += r.second - r.first + 1;
    }

    // Subtract beacons on this row
    std::set<long long> beacons_on_row;
    for (const auto& s : sensors) {
        if (s.by == target_row) {
            beacons_on_row.insert(s.bx);
        }
    }

    return total - beacons_on_row.size();
}

long long part2(const std::string& text) {
    auto sensors = parse_sensors(text);
    long long max_coord = 4000000;

    for (long long row = 0; row <= max_coord; row++) {
        auto ranges = get_coverage_at_row(sensors, row);

        // Clip ranges to search area
        std::vector<std::pair<long long, long long>> clipped;
        for (const auto& r : ranges) {
            if (r.second < 0 || r.first > max_coord) continue;
            clipped.push_back({std::max(0LL, r.first), std::min(max_coord, r.second)});
        }

        clipped = merge_ranges(clipped);

        // Check if full row is covered
        if (clipped.size() == 1 && clipped[0].first == 0 && clipped[0].second == max_coord) {
            continue;
        }

        // Found a gap
        long long x;
        if (clipped.size() > 1) {
            x = clipped[0].second + 1;
        } else if (clipped[0].first > 0) {
            x = 0;
        } else {
            x = clipped[0].second + 1;
        }

        return x * 4000000 + row;
    }

    return -1;
}

int main() {
    std::ifstream file("../input.txt");
    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string text = buffer.str();

    std::cout << "Part 1: " << part1(text) << std::endl;
    std::cout << "Part 2: " << part2(text) << std::endl;

    return 0;
}
