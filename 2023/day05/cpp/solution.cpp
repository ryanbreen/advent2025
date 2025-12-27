/**
 * Advent of Code 2023 - Day 5: If You Give A Seed A Fertilizer
 *
 * This solution processes almanac data that maps seeds through a chain of
 * transformations (seed -> soil -> fertilizer -> water -> light -> temperature
 * -> humidity -> location) to find the minimum location number.
 *
 * Algorithm Overview:
 * - Part 1: Direct mapping - apply each transformation to individual seed values
 * - Part 2: Interval arithmetic - treat seed ranges as intervals and split them
 *   as they pass through each mapping stage, preserving range semantics
 *
 * The key insight for Part 2 is that mapping ranges through transformations
 * requires splitting intervals where they overlap with mapping boundaries.
 * An input interval may split into multiple output intervals when it partially
 * overlaps different mapping ranges.
 *
 * Time Complexity: O(S * M * R) for Part 1, O(I * M * R) for Part 2
 *   where S = seeds, M = maps, R = ranges per map, I = intervals (grows with splits)
 * Space Complexity: O(I) for the interval list in Part 2
 */

#include <algorithm>
#include <cstdint>
#include <fstream>
#include <iostream>
#include <limits>
#include <sstream>
#include <string>
#include <vector>

/**
 * Represents a mapping rule: values in [src_start, src_start + length)
 * are mapped to [dst_start, dst_start + length)
 */
struct Range {
    int64_t dst_start;
    int64_t src_start;
    int64_t length;
};

/**
 * Represents a half-open interval [start, end)
 * Used for tracking seed ranges as they transform through mappings
 */
struct Interval {
    int64_t start;
    int64_t end;
};

// Global state for parsed input
std::vector<int64_t> seeds;
std::vector<std::vector<Range>> maps;

/**
 * Parses the almanac input file, extracting seeds and mapping rules.
 * Format: "seeds: n1 n2 ..." followed by map sections with "X-to-Y map:" headers
 */
void parse_input(const std::string& filename) {
    std::ifstream file(filename);
    std::string line;

    // Parse seeds from first line (format: "seeds: 79 14 55 13")
    std::getline(file, line);
    std::istringstream iss(line.substr(7));  // Skip "seeds: " prefix
    int64_t num;
    while (iss >> num) {
        seeds.push_back(num);
    }

    std::getline(file, line);  // Skip empty line after seeds

    // Parse each mapping section (7 maps: seed->soil->fertilizer->...->location)
    std::vector<Range> current_map;
    while (std::getline(file, line)) {
        if (line.empty()) {
            if (!current_map.empty()) {
                maps.push_back(current_map);
                current_map.clear();
            }
        } else if (line.find("map:") != std::string::npos) {
            continue;  // Skip header lines like "seed-to-soil map:"
        } else {
            // Parse mapping rule: "dst_start src_start length"
            Range r;
            std::istringstream range_iss(line);
            range_iss >> r.dst_start >> r.src_start >> r.length;
            current_map.push_back(r);
        }
    }
    if (!current_map.empty()) {
        maps.push_back(current_map);
    }
}

/**
 * Applies a single mapping stage to a value.
 * Returns the mapped value if it falls within any range, otherwise returns unchanged.
 */
int64_t apply_map(int64_t value, const std::vector<Range>& ranges) {
    for (const auto& r : ranges) {
        if (value >= r.src_start && value < r.src_start + r.length) {
            return r.dst_start + (value - r.src_start);
        }
    }
    return value;  // Unmapped values pass through unchanged
}

/**
 * Transforms a seed number through all 7 mapping stages to get its location.
 */
int64_t seed_to_location(int64_t seed) {
    int64_t value = seed;
    for (const auto& map_ranges : maps) {
        value = apply_map(value, map_ranges);
    }
    return value;
}

/**
 * Part 1: Find the minimum location for individually-listed seeds.
 * Each number in the seeds list is a single seed to process.
 */
int64_t part1() {
    int64_t min_location = std::numeric_limits<int64_t>::max();
    for (int64_t seed : seeds) {
        min_location = std::min(min_location, seed_to_location(seed));
    }
    return min_location;
}

/**
 * Applies a mapping stage to a collection of intervals using interval arithmetic.
 *
 * The key challenge is that a single input interval may be split into multiple
 * output intervals when it partially overlaps with mapping ranges:
 *
 *   Input interval:     [==========]
 *   Mapping range:           [----]
 *   Result:             [===][----][==]
 *                        ^    ^     ^
 *                        |    |     +-- unmapped (identity)
 *                        |    +-------- mapped (offset applied)
 *                        +------------- unmapped (identity)
 *
 * The algorithm tracks "remaining" unmapped portions of each interval as it
 * processes each mapping rule, then adds any still-remaining portions as
 * identity mappings at the end.
 */
std::vector<Interval> apply_map_to_ranges(const std::vector<Interval>& input_ranges,
                                          const std::vector<Range>& map_ranges) {
    std::vector<Interval> result;

    for (const auto& interval : input_ranges) {
        // Track portions of this interval not yet mapped by any rule
        std::vector<Interval> remaining;
        remaining.push_back(interval);

        for (const auto& r : map_ranges) {
            int64_t src_end = r.src_start + r.length;
            std::vector<Interval> new_remaining;

            for (const auto& rem : remaining) {
                // Split the remaining interval into up to 3 parts:
                // 1. Before the mapping range - stays in remaining (unmapped)
                // 2. Overlapping the mapping range - goes to result (mapped)
                // 3. After the mapping range - stays in remaining (unmapped)

                // Part 1: Portion before the map range [rem.start, r.src_start)
                if (rem.start < r.src_start) {
                    new_remaining.push_back({rem.start, std::min(rem.end, r.src_start)});
                }

                // Part 2: Overlapping portion - apply the mapping offset
                int64_t overlap_start = std::max(rem.start, r.src_start);
                int64_t overlap_end = std::min(rem.end, src_end);
                if (overlap_start < overlap_end) {
                    int64_t offset = r.dst_start - r.src_start;
                    result.push_back({overlap_start + offset, overlap_end + offset});
                }

                // Part 3: Portion after the map range [src_end, rem.end)
                if (rem.end > src_end) {
                    new_remaining.push_back({std::max(rem.start, src_end), rem.end});
                }
            }

            remaining = new_remaining;
        }

        // Any portions not matched by any mapping rule pass through unchanged
        for (const auto& rem : remaining) {
            result.push_back(rem);
        }
    }

    return result;
}

/**
 * Part 2: Find the minimum location when seeds are interpreted as ranges.
 * The seeds list is now pairs: (start, length) defining ranges of seed values.
 *
 * Brute-force iteration over billions of seeds would be too slow, so we use
 * interval arithmetic to track entire ranges through the transformation chain.
 */
int64_t part2() {
    // Interpret seeds as range pairs: [start, start + length)
    std::vector<Interval> ranges;
    for (size_t i = 0; i < seeds.size(); i += 2) {
        int64_t start = seeds[i];
        int64_t length = seeds[i + 1];
        ranges.push_back({start, start + length});
    }

    // Transform all ranges through each mapping stage
    for (const auto& map_ranges : maps) {
        ranges = apply_map_to_ranges(ranges, map_ranges);
    }

    // The minimum location is the smallest start value among all output ranges
    int64_t min_location = std::numeric_limits<int64_t>::max();
    for (const auto& r : ranges) {
        min_location = std::min(min_location, r.start);
    }
    return min_location;
}

int main() {
    parse_input("../input.txt");

    std::cout << "Part 1: " << part1() << std::endl;
    std::cout << "Part 2: " << part2() << std::endl;

    return 0;
}
