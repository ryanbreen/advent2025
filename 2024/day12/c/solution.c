#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_SIZE 200
#define MAX_QUEUE 50000

typedef struct {
    int r;
    int c;
} Point;

typedef struct {
    Point points[MAX_SIZE * MAX_SIZE];
    int size;
} Region;

char grid[MAX_SIZE][MAX_SIZE];
bool visited[MAX_SIZE][MAX_SIZE];
int rows = 0;
int cols = 0;

// Direction vectors: right, left, down, up
const int dr[] = {0, 0, 1, -1};
const int dc[] = {1, -1, 0, 0};

void read_input() {
    FILE *file = fopen("../input.txt", "r");
    if (!file) {
        perror("Error opening file");
        exit(1);
    }

    char line[MAX_SIZE];
    rows = 0;
    while (fgets(line, sizeof(line), file)) {
        // Remove newline
        line[strcspn(line, "\n")] = 0;
        if (strlen(line) == 0) break;

        cols = strlen(line);
        for (int c = 0; c < cols; c++) {
            grid[rows][c] = line[c];
        }
        rows++;
    }
    fclose(file);
}

Region find_region(int start_r, int start_c) {
    Region region;
    region.size = 0;

    char plant = grid[start_r][start_c];
    Point queue[MAX_QUEUE];
    int head = 0, tail = 0;

    queue[tail++] = (Point){start_r, start_c};

    while (head < tail) {
        Point curr = queue[head++];
        int cr = curr.r;
        int cc = curr.c;

        if (visited[cr][cc]) continue;
        if (cr < 0 || cr >= rows || cc < 0 || cc >= cols) continue;
        if (grid[cr][cc] != plant) continue;

        visited[cr][cc] = true;
        region.points[region.size++] = (Point){cr, cc};

        for (int d = 0; d < 4; d++) {
            int nr = cr + dr[d];
            int nc = cc + dc[d];
            if (nr >= 0 && nr < rows && nc >= 0 && nc < cols && !visited[nr][nc]) {
                queue[tail++] = (Point){nr, nc};
            }
        }
    }

    return region;
}

bool in_region(Region *region, int r, int c) {
    for (int i = 0; i < region->size; i++) {
        if (region->points[i].r == r && region->points[i].c == c) {
            return true;
        }
    }
    return false;
}

int calculate_perimeter(Region *region) {
    int perimeter = 0;
    for (int i = 0; i < region->size; i++) {
        int r = region->points[i].r;
        int c = region->points[i].c;

        for (int d = 0; d < 4; d++) {
            int nr = r + dr[d];
            int nc = c + dc[d];
            if (!in_region(region, nr, nc)) {
                perimeter++;
            }
        }
    }
    return perimeter;
}

int count_sides(Region *region) {
    int corners = 0;

    for (int i = 0; i < region->size; i++) {
        int r = region->points[i].r;
        int c = region->points[i].c;

        // Check all 8 neighbors
        bool up = in_region(region, r - 1, c);
        bool down = in_region(region, r + 1, c);
        bool left = in_region(region, r, c - 1);
        bool right = in_region(region, r, c + 1);
        bool up_left = in_region(region, r - 1, c - 1);
        bool up_right = in_region(region, r - 1, c + 1);
        bool down_left = in_region(region, r + 1, c - 1);
        bool down_right = in_region(region, r + 1, c + 1);

        // Top-left corner
        if (!up && !left) {  // convex
            corners++;
        } else if (up && left && !up_left) {  // concave
            corners++;
        }

        // Top-right corner
        if (!up && !right) {  // convex
            corners++;
        } else if (up && right && !up_right) {  // concave
            corners++;
        }

        // Bottom-left corner
        if (!down && !left) {  // convex
            corners++;
        } else if (down && left && !down_left) {  // concave
            corners++;
        }

        // Bottom-right corner
        if (!down && !right) {  // convex
            corners++;
        } else if (down && right && !down_right) {  // concave
            corners++;
        }
    }

    return corners;
}

long part1() {
    memset(visited, false, sizeof(visited));
    long total = 0;

    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            if (!visited[r][c]) {
                Region region = find_region(r, c);
                int area = region.size;
                int perimeter = calculate_perimeter(&region);
                total += (long)area * perimeter;
            }
        }
    }

    return total;
}

long part2() {
    memset(visited, false, sizeof(visited));
    long total = 0;

    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            if (!visited[r][c]) {
                Region region = find_region(r, c);
                int area = region.size;
                int sides = count_sides(&region);
                total += (long)area * sides;
            }
        }
    }

    return total;
}

int main() {
    read_input();
    printf("Part 1: %ld\n", part1());
    printf("Part 2: %ld\n", part2());
    return 0;
}
