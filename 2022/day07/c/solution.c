#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_DIRS 1000
#define MAX_PATH_LEN 1024
#define MAX_DEPTH 100

typedef struct {
    char path[MAX_PATH_LEN];
    long size;
} Directory;

static Directory dirs[MAX_DIRS];
static int dir_count = 0;

static char *path_stack[MAX_DEPTH];
static int path_depth = 0;

static void build_path(char *out) {
    if (path_depth == 0 || (path_depth == 1 && strcmp(path_stack[0], "/") == 0)) {
        strcpy(out, "/");
        return;
    }
    out[0] = '\0';
    for (int i = 0; i < path_depth; i++) {
        if (i == 0 && strcmp(path_stack[0], "/") == 0) {
            strcat(out, "/");
        } else {
            if (i > 0 && !(i == 1 && strcmp(path_stack[0], "/") == 0)) {
                strcat(out, "/");
            }
            strcat(out, path_stack[i]);
        }
    }
}

static int find_or_create_dir(const char *path) {
    for (int i = 0; i < dir_count; i++) {
        if (strcmp(dirs[i].path, path) == 0) {
            return i;
        }
    }
    strcpy(dirs[dir_count].path, path);
    dirs[dir_count].size = 0;
    return dir_count++;
}

static void add_file_size(long size) {
    char path[MAX_PATH_LEN];
    for (int depth = 1; depth <= path_depth; depth++) {
        path[0] = '\0';
        for (int i = 0; i < depth; i++) {
            if (i == 0 && strcmp(path_stack[0], "/") == 0) {
                strcpy(path, "/");
            } else {
                if (i > 0 && !(i == 1 && strcmp(path_stack[0], "/") == 0)) {
                    strcat(path, "/");
                }
                strcat(path, path_stack[i]);
            }
        }
        int idx = find_or_create_dir(path);
        dirs[idx].size += size;
    }
}

int main(void) {
    FILE *f = fopen("../input.txt", "r");
    if (!f) {
        perror("Failed to open input.txt");
        return 1;
    }

    char line[MAX_PATH_LEN];
    while (fgets(line, sizeof(line), f)) {
        size_t len = strlen(line);
        if (len > 0 && line[len - 1] == '\n') {
            line[len - 1] = '\0';
        }

        if (strncmp(line, "$ cd ", 5) == 0) {
            char *target = line + 5;
            if (strcmp(target, "/") == 0) {
                path_depth = 0;
                path_stack[path_depth++] = strdup("/");
            } else if (strcmp(target, "..") == 0) {
                if (path_depth > 0) {
                    free(path_stack[--path_depth]);
                }
            } else {
                path_stack[path_depth++] = strdup(target);
            }
        } else if (strncmp(line, "$ ls", 4) == 0) {
            continue;
        } else if (strncmp(line, "dir ", 4) == 0) {
            continue;
        } else if (line[0] >= '0' && line[0] <= '9') {
            long size = atol(line);
            add_file_size(size);
        }
    }

    fclose(f);

    /* Part 1: Sum of sizes of directories <= 100000 */
    long part1 = 0;
    for (int i = 0; i < dir_count; i++) {
        if (dirs[i].size <= 100000) {
            part1 += dirs[i].size;
        }
    }

    /* Part 2: Find smallest directory to delete */
    long total_space = 70000000;
    long needed_space = 30000000;
    long used_space = 0;
    for (int i = 0; i < dir_count; i++) {
        if (strcmp(dirs[i].path, "/") == 0) {
            used_space = dirs[i].size;
            break;
        }
    }
    long free_space = total_space - used_space;
    long need_to_free = needed_space - free_space;

    long part2 = used_space;
    for (int i = 0; i < dir_count; i++) {
        if (dirs[i].size >= need_to_free && dirs[i].size < part2) {
            part2 = dirs[i].size;
        }
    }

    printf("Part 1: %ld\n", part1);
    printf("Part 2: %ld\n", part2);

    /* Cleanup */
    for (int i = 0; i < path_depth; i++) {
        free(path_stack[i]);
    }

    return 0;
}
