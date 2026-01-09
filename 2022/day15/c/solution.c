#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define MAX_SENSORS 50
#define MAX_RANGES 100

typedef struct {
    int sx, sy;
    int bx, by;
    int dist;
} Sensor;

typedef struct {
    long long start, end;
} Range;

static int sensor_count = 0;
static Sensor sensors[MAX_SENSORS];

static int abs_val(int x) {
    return x < 0 ? -x : x;
}

static int range_compare(const void *a, const void *b) {
    const Range *ra = (const Range *)a;
    const Range *rb = (const Range *)b;
    if (ra->start < rb->start) return -1;
    if (ra->start > rb->start) return 1;
    return 0;
}

static int merge_ranges(Range *ranges, int count) {
    if (count == 0) return 0;

    qsort(ranges, count, sizeof(Range), range_compare);

    int merged_count = 1;
    for (int i = 1; i < count; i++) {
        if (ranges[i].start <= ranges[merged_count - 1].end + 1) {
            if (ranges[i].end > ranges[merged_count - 1].end) {
                ranges[merged_count - 1].end = ranges[i].end;
            }
        } else {
            ranges[merged_count++] = ranges[i];
        }
    }

    return merged_count;
}

static int get_coverage_at_row(int row, Range *ranges) {
    int range_count = 0;

    for (int i = 0; i < sensor_count; i++) {
        int row_dist = abs_val(sensors[i].sy - row);
        if (row_dist > sensors[i].dist) continue;

        int x_spread = sensors[i].dist - row_dist;
        ranges[range_count].start = sensors[i].sx - x_spread;
        ranges[range_count].end = sensors[i].sx + x_spread;
        range_count++;
    }

    return merge_ranges(ranges, range_count);
}

static long long part1(void) {
    int target_row = 2000000;
    Range ranges[MAX_RANGES];

    int range_count = get_coverage_at_row(target_row, ranges);

    long long total = 0;
    for (int i = 0; i < range_count; i++) {
        total += ranges[i].end - ranges[i].start + 1;
    }

    // Subtract beacons on this row (count unique beacons)
    int beacons_on_row[MAX_SENSORS];
    int beacon_count = 0;

    for (int i = 0; i < sensor_count; i++) {
        if (sensors[i].by == target_row) {
            int found = 0;
            for (int j = 0; j < beacon_count; j++) {
                if (beacons_on_row[j] == sensors[i].bx) {
                    found = 1;
                    break;
                }
            }
            if (!found) {
                beacons_on_row[beacon_count++] = sensors[i].bx;
            }
        }
    }

    return total - beacon_count;
}

static long long part2(void) {
    int max_coord = 4000000;
    Range ranges[MAX_RANGES];
    Range clipped[MAX_RANGES];

    for (int row = 0; row <= max_coord; row++) {
        int range_count = get_coverage_at_row(row, ranges);

        // Clip ranges to search area
        int clipped_count = 0;
        for (int i = 0; i < range_count; i++) {
            if (ranges[i].end < 0 || ranges[i].start > max_coord) continue;

            clipped[clipped_count].start = ranges[i].start < 0 ? 0 : ranges[i].start;
            clipped[clipped_count].end = ranges[i].end > max_coord ? max_coord : ranges[i].end;
            clipped_count++;
        }

        clipped_count = merge_ranges(clipped, clipped_count);

        // Check if full row is covered
        if (clipped_count == 1 && clipped[0].start == 0 && clipped[0].end == max_coord) {
            continue;
        }

        // Found a gap
        long long x;
        if (clipped_count > 1) {
            x = clipped[0].end + 1;
        } else if (clipped[0].start > 0) {
            x = 0;
        } else {
            x = clipped[0].end + 1;
        }

        return x * 4000000LL + row;
    }

    return -1;
}

static void parse_input(const char *filename) {
    FILE *f = fopen(filename, "r");
    if (!f) {
        perror("Failed to open input file");
        exit(1);
    }

    char line[256];
    while (fgets(line, sizeof(line), f)) {
        int sx, sy, bx, by;
        if (sscanf(line, "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d",
                   &sx, &sy, &bx, &by) == 4) {
            sensors[sensor_count].sx = sx;
            sensors[sensor_count].sy = sy;
            sensors[sensor_count].bx = bx;
            sensors[sensor_count].by = by;
            sensors[sensor_count].dist = abs_val(sx - bx) + abs_val(sy - by);
            sensor_count++;
        }
    }

    fclose(f);
}

int main(void) {
    parse_input("../input.txt");

    printf("Part 1: %lld\n", part1());
    printf("Part 2: %lld\n", part2());

    return 0;
}
