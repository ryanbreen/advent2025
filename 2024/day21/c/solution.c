/*
 * Day 21: Keypad Conundrum
 * Robot chain control with shortest path optimization
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <stdint.h>

#define MAX_CODES 10
#define MAX_CODE_LEN 10
#define MAX_PATHS 100
#define MAX_PATH_LEN 20

typedef struct {
    int row;
    int col;
} Position;

typedef struct {
    char path[MAX_PATH_LEN];
    int len;
} Path;

// Keypad layouts
Position numeric_keypad(char c) {
    switch(c) {
        case '7': return (Position){0, 0};
        case '8': return (Position){0, 1};
        case '9': return (Position){0, 2};
        case '4': return (Position){1, 0};
        case '5': return (Position){1, 1};
        case '6': return (Position){1, 2};
        case '1': return (Position){2, 0};
        case '2': return (Position){2, 1};
        case '3': return (Position){2, 2};
        case '0': return (Position){3, 1};
        case 'A': return (Position){3, 2};
    }
    return (Position){-1, -1};
}

Position directional_keypad(char c) {
    switch(c) {
        case '^': return (Position){0, 1};
        case 'A': return (Position){0, 2};
        case '<': return (Position){1, 0};
        case 'v': return (Position){1, 1};
        case '>': return (Position){1, 2};
    }
    return (Position){-1, -1};
}

// Cache structure for memoization
typedef struct {
    char from;
    char to;
    int depth;
    int is_numeric;
    int64_t value;
    int valid;
} CacheEntry;

#define CACHE_SIZE 100000
CacheEntry cache[CACHE_SIZE];

void init_cache() {
    memset(cache, 0, sizeof(cache));
}

int64_t get_cached(char from, char to, int depth, int is_numeric) {
    unsigned int hash = ((unsigned char)from * 31 + (unsigned char)to) * 31 + depth;
    hash = hash * 31 + is_numeric;
    hash = hash % CACHE_SIZE;

    for (int i = 0; i < 100; i++) {
        int idx = (hash + i) % CACHE_SIZE;
        if (cache[idx].valid &&
            cache[idx].from == from &&
            cache[idx].to == to &&
            cache[idx].depth == depth &&
            cache[idx].is_numeric == is_numeric) {
            return cache[idx].value;
        }
        if (!cache[idx].valid) {
            return -1;
        }
    }
    return -1;
}

void set_cached(char from, char to, int depth, int is_numeric, int64_t value) {
    unsigned int hash = ((unsigned char)from * 31 + (unsigned char)to) * 31 + depth;
    hash = hash * 31 + is_numeric;
    hash = hash % CACHE_SIZE;

    for (int i = 0; i < 100; i++) {
        int idx = (hash + i) % CACHE_SIZE;
        if (!cache[idx].valid) {
            cache[idx].from = from;
            cache[idx].to = to;
            cache[idx].depth = depth;
            cache[idx].is_numeric = is_numeric;
            cache[idx].value = value;
            cache[idx].valid = 1;
            return;
        }
    }
}

// Find all shortest paths using DFS
void find_paths_dfs(int r, int c, int er, int ec, Position gap, char *path, int path_len,
                    Path *paths, int *num_paths, int min_len) {
    if (r == gap.row && c == gap.col) {
        return;
    }
    if (r == er && c == ec) {
        if (path_len <= min_len && *num_paths < MAX_PATHS) {
            strcpy(paths[*num_paths].path, path);
            paths[*num_paths].len = path_len;
            (*num_paths)++;
        }
        return;
    }
    if (path_len > min_len) {
        return;
    }

    // Try vertical moves
    if (r < er) {
        path[path_len] = 'v';
        path[path_len + 1] = '\0';
        find_paths_dfs(r + 1, c, er, ec, gap, path, path_len + 1, paths, num_paths, min_len);
    } else if (r > er) {
        path[path_len] = '^';
        path[path_len + 1] = '\0';
        find_paths_dfs(r - 1, c, er, ec, gap, path, path_len + 1, paths, num_paths, min_len);
    }

    // Try horizontal moves
    if (c < ec) {
        path[path_len] = '>';
        path[path_len + 1] = '\0';
        find_paths_dfs(r, c + 1, er, ec, gap, path, path_len + 1, paths, num_paths, min_len);
    } else if (c > ec) {
        path[path_len] = '<';
        path[path_len + 1] = '\0';
        find_paths_dfs(r, c - 1, er, ec, gap, path, path_len + 1, paths, num_paths, min_len);
    }
}

int shortest_paths(char start, char end, int is_numeric, Path *paths) {
    Position s, e, gap;

    if (is_numeric) {
        s = numeric_keypad(start);
        e = numeric_keypad(end);
        gap = (Position){3, 0};
    } else {
        s = directional_keypad(start);
        e = directional_keypad(end);
        gap = (Position){0, 0};
    }

    int num_paths = 0;
    char path[MAX_PATH_LEN] = "";
    int min_dist = abs(s.row - e.row) + abs(s.col - e.col);

    find_paths_dfs(s.row, s.col, e.row, e.col, gap, path, 0, paths, &num_paths, min_dist);

    if (num_paths == 0) {
        // Same position
        paths[0].path[0] = '\0';
        paths[0].len = 0;
        num_paths = 1;
    }

    return num_paths;
}

// Forward declaration
int64_t min_presses_for_move(char from, char to, int depth, int is_numeric);

int64_t min_presses_for_move(char from, char to, int depth, int is_numeric) {
    // Check cache
    int64_t cached = get_cached(from, to, depth, is_numeric);
    if (cached >= 0) {
        return cached;
    }

    Path paths[MAX_PATHS];
    int num_paths = shortest_paths(from, to, is_numeric, paths);

    if (depth == 0) {
        // Human level - return minimum path length + 1 for 'A' press
        int min_len = INT_MAX;
        for (int i = 0; i < num_paths; i++) {
            if (paths[i].len < min_len) {
                min_len = paths[i].len;
            }
        }
        int64_t result = min_len + 1;
        set_cached(from, to, depth, is_numeric, result);
        return result;
    }

    int64_t best = LLONG_MAX;
    for (int i = 0; i < num_paths; i++) {
        // Need to type path + 'A' on directional keypad
        char sequence[MAX_PATH_LEN + 2];
        strcpy(sequence, paths[i].path);
        strcat(sequence, "A");

        int64_t cost = 0;
        char current = 'A';
        for (int j = 0; sequence[j] != '\0'; j++) {
            cost += min_presses_for_move(current, sequence[j], depth - 1, 0);
            current = sequence[j];
        }

        if (cost < best) {
            best = cost;
        }
    }

    set_cached(from, to, depth, is_numeric, best);
    return best;
}

int64_t solve_code(const char *code, int depth) {
    int64_t total = 0;
    char current = 'A';

    for (int i = 0; code[i] != '\0'; i++) {
        total += min_presses_for_move(current, code[i], depth, 1);
        current = code[i];
    }

    return total;
}

int64_t complexity(const char *code, int64_t length) {
    // Extract numeric part
    int numeric = 0;
    for (int i = 0; code[i] != '\0'; i++) {
        if (code[i] >= '0' && code[i] <= '9') {
            numeric = numeric * 10 + (code[i] - '0');
        }
    }
    return length * numeric;
}

int64_t part1(char codes[][MAX_CODE_LEN], int num_codes) {
    int64_t total = 0;
    for (int i = 0; i < num_codes; i++) {
        int64_t length = solve_code(codes[i], 2);
        total += complexity(codes[i], length);
    }
    return total;
}

int64_t part2(char codes[][MAX_CODE_LEN], int num_codes) {
    int64_t total = 0;
    for (int i = 0; i < num_codes; i++) {
        int64_t length = solve_code(codes[i], 25);
        total += complexity(codes[i], length);
    }
    return total;
}

int main() {
    FILE *f = fopen("../input.txt", "r");
    if (!f) {
        perror("Error opening input.txt");
        return 1;
    }

    char codes[MAX_CODES][MAX_CODE_LEN];
    int num_codes = 0;

    while (fgets(codes[num_codes], MAX_CODE_LEN, f) && num_codes < MAX_CODES) {
        // Remove newline
        codes[num_codes][strcspn(codes[num_codes], "\n")] = 0;
        if (strlen(codes[num_codes]) > 0) {
            num_codes++;
        }
    }
    fclose(f);

    init_cache();
    printf("Part 1: %lld\n", part1(codes, num_codes));

    // Clear cache for part 2 (different depth, might help)
    init_cache();
    printf("Part 2: %lld\n", part2(codes, num_codes));

    return 0;
}
