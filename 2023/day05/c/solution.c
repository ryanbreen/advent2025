/*
 * Advent of Code 2023 - Day 5: If You Give A Seed A Fertilizer
 *
 * Algorithm Overview:
 * This problem involves mapping seed numbers through a chain of transformation
 * maps (seed -> soil -> fertilizer -> water -> light -> temperature -> humidity
 * -> location) to find minimum location values.
 *
 * Part 1: Direct Value Mapping
 * - For each individual seed value, apply each map in sequence
 * - Each map transforms a value by finding which range it falls into
 * - If no range matches, the value passes through unchanged (identity mapping)
 * - Track the minimum final location across all seeds
 *
 * Part 2: Range-Based Mapping
 * - Seeds are now interpreted as ranges (start, length pairs)
 * - Instead of mapping individual values, we map entire ranges
 * - When a range intersects a map range, it may be split into:
 *   1. A portion before the map range (unmapped, passed to next map range)
 *   2. An overlapping portion (transformed according to the map)
 *   3. A portion after the map range (unmapped, passed to next map range)
 * - This range splitting approach handles billions of seeds efficiently
 * - The minimum location is the smallest start value among all final ranges
 *
 * Time Complexity:
 * - Part 1: O(seeds * maps * ranges_per_map)
 * - Part 2: O(initial_ranges * maps * ranges_per_map^2) in worst case,
 *           but typically much better due to limited range fragmentation
 *
 * Space Complexity: O(max_ranges) for intermediate range storage
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <limits.h>

/* Buffer size limits */
#define MAX_SEEDS 32
#define MAX_RANGES_PER_MAP 64
#define MAX_MAPS 8
#define MAX_RESULT_RANGES 4096

/*
 * Represents a single mapping rule within a map.
 * Maps values in [src_start, src_start + length) to
 * [dst_start, dst_start + length).
 */
typedef struct {
    int64_t dst_start;
    int64_t src_start;
    int64_t length;
} MapRange;

/*
 * A complete transformation map containing multiple mapping rules.
 * Values not covered by any range pass through unchanged.
 */
typedef struct {
    MapRange ranges[MAX_RANGES_PER_MAP];
    int count;
} Map;

/*
 * A half-open interval [start, end) representing a contiguous range of values.
 * Used in Part 2 for efficient range-based transformations.
 */
typedef struct {
    int64_t start;
    int64_t end;
} Range;

/* Global data storage */
static int64_t g_seeds[MAX_SEEDS];
static int g_seed_count = 0;
static Map g_maps[MAX_MAPS];
static int g_map_count = 0;

/*
 * Parse the input file containing seeds and transformation maps.
 * File format:
 *   seeds: <space-separated numbers>
 *   <blank line>
 *   <map-name> map:
 *   <dst> <src> <len>
 *   ...
 */
static void parse_input(const char *filename) {
    FILE *fp = fopen(filename, "r");
    if (!fp) {
        perror("Failed to open input file");
        exit(1);
    }

    char line[1024];
    int current_map = -1;

    while (fgets(line, sizeof(line), fp)) {
        /* Remove trailing newline */
        line[strcspn(line, "\n")] = '\0';

        if (strlen(line) == 0) {
            continue;
        }

        /* Parse seeds line */
        if (strncmp(line, "seeds:", 6) == 0) {
            char *ptr = line + 7;
            char *token;
            while ((token = strtok(ptr, " ")) != NULL) {
                g_seeds[g_seed_count++] = strtoll(token, NULL, 10);
                ptr = NULL;
            }
            continue;
        }

        /* Check for map header (e.g., "seed-to-soil map:") */
        if (strstr(line, "map:") != NULL) {
            current_map++;
            g_maps[current_map].count = 0;
            g_map_count = current_map + 1;
            continue;
        }

        /* Parse map range (three numbers: dst src len) */
        if (current_map >= 0 && line[0] >= '0' && line[0] <= '9') {
            int64_t dst, src, len;
            if (sscanf(line, "%lld %lld %lld", &dst, &src, &len) == 3) {
                Map *map = &g_maps[current_map];
                map->ranges[map->count].dst_start = dst;
                map->ranges[map->count].src_start = src;
                map->ranges[map->count].length = len;
                map->count++;
            }
        }
    }

    fclose(fp);
}

/*
 * Apply a single map to transform a value.
 * Searches through all ranges in the map to find one containing the value.
 * Returns the transformed value, or the original if no range matches.
 */
static int64_t apply_map_to_value(int64_t value, const Map *map) {
    for (int i = 0; i < map->count; i++) {
        const MapRange *range = &map->ranges[i];
        if (value >= range->src_start &&
            value < range->src_start + range->length) {
            return range->dst_start + (value - range->src_start);
        }
    }
    return value;  /* Identity mapping for uncovered values */
}

/*
 * Transform a seed value through all maps to get its final location.
 */
static int64_t seed_to_location(int64_t seed) {
    int64_t value = seed;
    for (int i = 0; i < g_map_count; i++) {
        value = apply_map_to_value(value, &g_maps[i]);
    }
    return value;
}

/*
 * Part 1: Find the minimum location for individual seed values.
 * Simply applies all transformations to each seed and tracks the minimum.
 */
static int64_t solve_part1(void) {
    int64_t min_location = INT64_MAX;
    for (int i = 0; i < g_seed_count; i++) {
        int64_t location = seed_to_location(g_seeds[i]);
        if (location < min_location) {
            min_location = location;
        }
    }
    return min_location;
}

/*
 * Apply a map to a collection of ranges, producing transformed ranges.
 *
 * Range Splitting Algorithm:
 * For each input range and each map range, we consider three cases:
 *
 *   input range:     [--------------------]
 *   map range:              [========]
 *
 *   Result:
 *   - Before portion: [----]               (stays unmapped, try next map range)
 *   - Overlap:              [========]     (transformed and added to output)
 *   - After portion:                 [---] (stays unmapped, try next map range)
 *
 * After processing all map ranges, any remaining unmapped portions
 * pass through unchanged (identity mapping).
 *
 * Returns the number of ranges in the output array.
 */
static int apply_map_to_ranges(const Range *input, int input_count,
                                const Map *map, Range *output) {
    int output_count = 0;

    for (int i = 0; i < input_count; i++) {
        /*
         * Track portions of the input range that haven't been mapped yet.
         * These will either be transformed by a later map range or
         * pass through unchanged.
         */
        Range remaining[MAX_RESULT_RANGES];
        int remaining_count = 1;
        remaining[0] = input[i];

        /* Try each map range against the remaining unmapped portions */
        for (int j = 0; j < map->count; j++) {
            const MapRange *map_range = &map->ranges[j];
            const int64_t map_src_end = map_range->src_start + map_range->length;

            Range new_remaining[MAX_RESULT_RANGES];
            int new_remaining_count = 0;

            for (int k = 0; k < remaining_count; k++) {
                const int64_t range_start = remaining[k].start;
                const int64_t range_end = remaining[k].end;

                /*
                 * Case 1: Portion before the map range
                 * This part is not affected by this map range, but might be
                 * affected by a later map range, so keep it in remaining.
                 */
                if (range_start < map_range->src_start) {
                    int64_t before_end = range_end < map_range->src_start
                                             ? range_end
                                             : map_range->src_start;
                    new_remaining[new_remaining_count].start = range_start;
                    new_remaining[new_remaining_count].end = before_end;
                    new_remaining_count++;
                }

                /*
                 * Case 2: Overlapping portion
                 * This part is transformed according to the map and added
                 * directly to output (won't be processed by other map ranges).
                 */
                int64_t overlap_start = range_start > map_range->src_start
                                            ? range_start
                                            : map_range->src_start;
                int64_t overlap_end = range_end < map_src_end
                                          ? range_end
                                          : map_src_end;
                if (overlap_start < overlap_end) {
                    int64_t offset = map_range->dst_start - map_range->src_start;
                    output[output_count].start = overlap_start + offset;
                    output[output_count].end = overlap_end + offset;
                    output_count++;
                }

                /*
                 * Case 3: Portion after the map range
                 * Like Case 1, this might be affected by a later map range.
                 */
                if (range_end > map_src_end) {
                    int64_t after_start = range_start > map_src_end
                                              ? range_start
                                              : map_src_end;
                    new_remaining[new_remaining_count].start = after_start;
                    new_remaining[new_remaining_count].end = range_end;
                    new_remaining_count++;
                }
            }

            /* Update remaining with the portions that weren't mapped */
            remaining_count = new_remaining_count;
            for (int k = 0; k < new_remaining_count; k++) {
                remaining[k] = new_remaining[k];
            }
        }

        /*
         * Any portions still in remaining weren't covered by any map range,
         * so they pass through unchanged (identity mapping).
         */
        for (int k = 0; k < remaining_count; k++) {
            output[output_count++] = remaining[k];
        }
    }

    return output_count;
}

/*
 * Part 2: Find the minimum location for seed ranges.
 * Seeds are now interpreted as (start, length) pairs defining ranges.
 * Uses range-based transformations for efficiency.
 */
static int64_t solve_part2(void) {
    /* Convert seed pairs to ranges: [start, start + length) */
    Range ranges[MAX_RESULT_RANGES];
    int range_count = 0;

    for (int i = 0; i < g_seed_count; i += 2) {
        ranges[range_count].start = g_seeds[i];
        ranges[range_count].end = g_seeds[i] + g_seeds[i + 1];
        range_count++;
    }

    /* Apply each transformation map to all ranges */
    Range temp[MAX_RESULT_RANGES];
    for (int i = 0; i < g_map_count; i++) {
        int new_count = apply_map_to_ranges(ranges, range_count,
                                            &g_maps[i], temp);
        range_count = new_count;
        for (int j = 0; j < new_count; j++) {
            ranges[j] = temp[j];
        }
    }

    /* The minimum location is the smallest range start value */
    int64_t min_location = INT64_MAX;
    for (int i = 0; i < range_count; i++) {
        if (ranges[i].start < min_location) {
            min_location = ranges[i].start;
        }
    }

    return min_location;
}

int main(void) {
    parse_input("../input.txt");

    printf("Part 1: %lld\n", solve_part1());
    printf("Part 2: %lld\n", solve_part2());

    return 0;
}
