#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_WIDTH 256
#define MAX_HEIGHT 64
#define MAX_BLIZZARDS 10000
#define MAX_QUEUE 1000000

typedef struct {
    int r, c;
    char dir;
} Blizzard;

typedef struct {
    int time, r, c;
} State;

// Global state
static char grid[MAX_HEIGHT][MAX_WIDTH];
static int height, width;
static int inner_h, inner_w;
static Blizzard blizzards[MAX_BLIZZARDS];
static int num_blizzards;
static int start_r, start_c;
static int end_r, end_c;
static int period;

// Blizzard position cache: period x height x width bitmask
static bool *blizzard_cache;

// BFS queue and visited set
static State queue[MAX_QUEUE];
static bool *visited;  // period x height x width

int gcd(int a, int b) {
    while (b) {
        int t = b;
        b = a % b;
        a = t;
    }
    return a;
}

int lcm(int a, int b) {
    return a / gcd(a, b) * b;
}

// Proper modulo that handles negative numbers
int mod(int x, int m) {
    return ((x % m) + m) % m;
}

void parse_input(const char *filename) {
    FILE *f = fopen(filename, "r");
    if (!f) {
        fprintf(stderr, "Cannot open %s\n", filename);
        exit(1);
    }

    height = 0;
    num_blizzards = 0;

    while (fgets(grid[height], MAX_WIDTH, f)) {
        // Remove newline
        int len = strlen(grid[height]);
        if (len > 0 && grid[height][len-1] == '\n') {
            grid[height][len-1] = '\0';
            len--;
        }

        if (height == 0) {
            width = len;
        }

        // Find blizzards
        for (int c = 0; c < len; c++) {
            char ch = grid[height][c];
            if (ch == '^' || ch == 'v' || ch == '<' || ch == '>') {
                blizzards[num_blizzards].r = height;
                blizzards[num_blizzards].c = c;
                blizzards[num_blizzards].dir = ch;
                num_blizzards++;
            }
        }

        height++;
    }
    fclose(f);

    inner_h = height - 2;
    inner_w = width - 2;

    // Find start position (in first row)
    for (int c = 0; c < width; c++) {
        if (grid[0][c] == '.') {
            start_r = 0;
            start_c = c;
            break;
        }
    }

    // Find end position (in last row)
    for (int c = 0; c < width; c++) {
        if (grid[height-1][c] == '.') {
            end_r = height - 1;
            end_c = c;
            break;
        }
    }

    period = lcm(inner_h, inner_w);
}

void precompute_blizzards() {
    // Allocate blizzard cache
    size_t cache_size = (size_t)period * height * width;
    blizzard_cache = calloc(cache_size, sizeof(bool));
    if (!blizzard_cache) {
        fprintf(stderr, "Cannot allocate blizzard cache\n");
        exit(1);
    }

    for (int t = 0; t < period; t++) {
        for (int i = 0; i < num_blizzards; i++) {
            int ir = blizzards[i].r - 1;  // Inner row
            int ic = blizzards[i].c - 1;  // Inner col
            int nr, nc;

            switch (blizzards[i].dir) {
                case '^':
                    nr = mod(ir - t, inner_h);
                    nc = ic;
                    break;
                case 'v':
                    nr = mod(ir + t, inner_h);
                    nc = ic;
                    break;
                case '<':
                    nr = ir;
                    nc = mod(ic - t, inner_w);
                    break;
                case '>':
                    nr = ir;
                    nc = mod(ic + t, inner_w);
                    break;
                default:
                    continue;
            }

            // Convert back to full coordinates
            int final_r = nr + 1;
            int final_c = nc + 1;

            size_t idx = (size_t)t * height * width + final_r * width + final_c;
            blizzard_cache[idx] = true;
        }
    }
}

bool has_blizzard(int time, int r, int c) {
    int t = time % period;
    size_t idx = (size_t)t * height * width + r * width + c;
    return blizzard_cache[idx];
}

bool is_valid_position(int r, int c, int goal_r, int goal_c) {
    // Start and end are always valid
    if (r == start_r && c == start_c) return true;
    if (r == goal_r && c == goal_c) return true;

    // Check if in inner area
    if (r <= 0 || r >= height - 1) return false;
    if (c <= 0 || c >= width - 1) return false;

    return true;
}

int bfs(int start_time, int from_r, int from_c, int to_r, int to_c) {
    // Allocate visited set
    size_t visited_size = (size_t)period * height * width;
    visited = calloc(visited_size, sizeof(bool));
    if (!visited) {
        fprintf(stderr, "Cannot allocate visited set\n");
        exit(1);
    }

    int front = 0, back = 0;

    // Initial state
    queue[back++] = (State){start_time, from_r, from_c};
    size_t init_idx = (size_t)(start_time % period) * height * width + from_r * width + from_c;
    visited[init_idx] = true;

    // Directions: wait, up, down, left, right
    int dr[] = {0, -1, 1, 0, 0};
    int dc[] = {0, 0, 0, -1, 1};

    while (front < back) {
        State cur = queue[front++];

        if (cur.r == to_r && cur.c == to_c) {
            free(visited);
            return cur.time;
        }

        int next_time = cur.time + 1;

        for (int d = 0; d < 5; d++) {
            int nr = cur.r + dr[d];
            int nc = cur.c + dc[d];

            if (!is_valid_position(nr, nc, to_r, to_c)) continue;
            if (has_blizzard(next_time, nr, nc)) continue;

            size_t state_idx = (size_t)(next_time % period) * height * width + nr * width + nc;
            if (visited[state_idx]) continue;

            visited[state_idx] = true;
            queue[back++] = (State){next_time, nr, nc};

            if (back >= MAX_QUEUE) {
                fprintf(stderr, "Queue overflow\n");
                exit(1);
            }
        }
    }

    free(visited);
    return -1;  // No path found
}

int part1() {
    return bfs(0, start_r, start_c, end_r, end_c);
}

int part2() {
    // Trip 1: start -> end
    int t1 = bfs(0, start_r, start_c, end_r, end_c);

    // Trip 2: end -> start
    int t2 = bfs(t1, end_r, end_c, start_r, start_c);

    // Trip 3: start -> end
    int t3 = bfs(t2, start_r, start_c, end_r, end_c);

    return t3;
}

int main(int argc, char *argv[]) {
    const char *input_file = "../input.txt";
    if (argc > 1) {
        input_file = argv[1];
    }

    parse_input(input_file);
    precompute_blizzards();

    printf("Part 1: %d\n", part1());
    printf("Part 2: %d\n", part2());

    free(blizzard_cache);

    return 0;
}
