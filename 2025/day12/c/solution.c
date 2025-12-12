#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

#define MAX_SHAPES 100
#define MAX_REGIONS 1000
#define INITIAL_BUF_SIZE 8192

typedef struct {
    int width;
    int height;
    int *counts;
    int num_counts;
} Region;

typedef struct {
    int *shape_sizes;
    int num_shapes;
    Region *regions;
    int num_regions;
    int region_capacity;
} PuzzleData;

static void init_puzzle_data(PuzzleData *data) {
    data->shape_sizes = calloc(MAX_SHAPES, sizeof(int));
    data->num_shapes = 0;
    data->regions = malloc(sizeof(Region) * 100);
    data->num_regions = 0;
    data->region_capacity = 100;
}

static void free_puzzle_data(PuzzleData *data) {
    free(data->shape_sizes);
    for (int i = 0; i < data->num_regions; i++) {
        free(data->regions[i].counts);
    }
    free(data->regions);
}

static char* read_file(const char *filename, size_t *size) {
    FILE *f = fopen(filename, "rb");
    if (!f) {
        perror("fopen");
        exit(1);
    }

    fseek(f, 0, SEEK_END);
    *size = ftell(f);
    fseek(f, 0, SEEK_SET);

    char *buffer = malloc(*size + 1);
    if (!buffer) {
        perror("malloc");
        exit(1);
    }

    size_t read_size = fread(buffer, 1, *size, f);
    buffer[read_size] = '\0';
    *size = read_size;

    fclose(f);
    return buffer;
}

static inline bool is_shape_header(const char *line) {
    // Shape header: digit(s) followed by ':'
    if (!isdigit(*line)) return false;
    while (*line && isdigit(*line)) line++;
    return *line == ':' && *(line + 1) == '\0';
}

static inline bool is_region_line(const char *line) {
    // Region: digits, 'x', digits, ':', counts
    const char *p = line;
    while (*p && isdigit(*p)) p++;
    if (*p != 'x') return false;
    p++;
    while (*p && isdigit(*p)) p++;
    return *p == ':';
}

static void parse_input(const char *filename, PuzzleData *data) {
    size_t file_size;
    char *content = read_file(filename, &file_size);
    char *line = content;
    char *end = content + file_size;

    int current_shape_idx = -1;
    int current_shape_cells = 0;
    bool in_shape = false;

    while (line < end) {
        // Find line end
        char *line_end = line;
        while (line_end < end && *line_end != '\n') line_end++;

        // Null-terminate line temporarily
        char saved = *line_end;
        *line_end = '\0';

        // Skip empty lines
        if (*line == '\0') {
            if (in_shape) {
                data->shape_sizes[current_shape_idx] = current_shape_cells;
                if (current_shape_idx >= data->num_shapes) {
                    data->num_shapes = current_shape_idx + 1;
                }
                in_shape = false;
            }
        } else if (is_shape_header(line)) {
            // Shape header
            current_shape_idx = atoi(line);
            current_shape_cells = 0;
            in_shape = true;
        } else if (is_region_line(line)) {
            // Region definition
            Region region;
            sscanf(line, "%dx%d:", &region.width, &region.height);

            // Find colon and parse counts
            const char *counts_str = strchr(line, ':') + 1;

            // Count how many numbers we have
            int count = 0;
            const char *p = counts_str;
            while (*p) {
                while (*p && isspace(*p)) p++;
                if (isdigit(*p)) {
                    count++;
                    while (*p && isdigit(*p)) p++;
                }
            }

            region.counts = malloc(count * sizeof(int));
            region.num_counts = count;

            // Parse the counts
            p = counts_str;
            for (int i = 0; i < count; i++) {
                while (*p && isspace(*p)) p++;
                region.counts[i] = atoi(p);
                while (*p && isdigit(*p)) p++;
            }

            // Expand regions array if needed
            if (data->num_regions >= data->region_capacity) {
                data->region_capacity *= 2;
                data->regions = realloc(data->regions,
                    sizeof(Region) * data->region_capacity);
            }

            data->regions[data->num_regions++] = region;
        } else if (in_shape) {
            // Count '#' in shape body
            for (const char *p = line; *p; p++) {
                if (*p == '#') current_shape_cells++;
            }
        }

        // Restore character and move to next line
        *line_end = saved;
        line = (saved == '\n') ? line_end + 1 : line_end;
    }

    // Handle last shape if no trailing newline
    if (in_shape) {
        data->shape_sizes[current_shape_idx] = current_shape_cells;
        if (current_shape_idx >= data->num_shapes) {
            data->num_shapes = current_shape_idx + 1;
        }
    }

    free(content);
}

static bool can_fit_region(const Region *region, const int *shape_sizes) {
    int total_cells_needed = 0;
    for (int i = 0; i < region->num_counts; i++) {
        total_cells_needed += region->counts[i] * shape_sizes[i];
    }
    return total_cells_needed <= region->width * region->height;
}

static int part1(const PuzzleData *data) {
    int count = 0;
    for (int i = 0; i < data->num_regions; i++) {
        if (can_fit_region(&data->regions[i], data->shape_sizes)) {
            count++;
        }
    }
    return count;
}

static int part2(void) {
    // Part 2 is just a button click - no computation
    return 0;
}

int main(void) {
    PuzzleData data;
    init_puzzle_data(&data);
    parse_input("../input.txt", &data);

    printf("Part 1: %d\n", part1(&data));
    printf("Part 2: %d\n", part2());

    free_puzzle_data(&data);
    return 0;
}
