#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define WIDTH 101
#define HEIGHT 103
#define MAX_ROBOTS 1000

typedef struct {
    int px, py;
    int vx, vy;
} Robot;

typedef struct {
    int x, y;
} Position;

// Parse robots from input file
int parse_robots(const char *filename, Robot *robots) {
    FILE *file = fopen(filename, "r");
    if (!file) {
        perror("Error opening file");
        return -1;
    }

    int count = 0;
    char line[256];

    while (fgets(line, sizeof(line), file)) {
        int px, py, vx, vy;
        if (sscanf(line, "p=%d,%d v=%d,%d", &px, &py, &vx, &vy) == 4) {
            robots[count].px = px;
            robots[count].py = py;
            robots[count].vx = vx;
            robots[count].vy = vy;
            count++;
        }
    }

    fclose(file);
    return count;
}

// Simulate robot movement for given seconds
void simulate(const Robot *robots, int num_robots, int seconds, Position *positions) {
    for (int i = 0; i < num_robots; i++) {
        // Calculate new position with wrapping
        int new_x = (robots[i].px + robots[i].vx * seconds) % WIDTH;
        int new_y = (robots[i].py + robots[i].vy * seconds) % HEIGHT;

        // Handle negative modulo
        if (new_x < 0) new_x += WIDTH;
        if (new_y < 0) new_y += HEIGHT;

        positions[i].x = new_x;
        positions[i].y = new_y;
    }
}

// Count robots in each quadrant
void count_quadrants(const Position *positions, int num_robots,
                     int *q1, int *q2, int *q3, int *q4) {
    int mid_x = WIDTH / 2;   // 50
    int mid_y = HEIGHT / 2;  // 51

    *q1 = *q2 = *q3 = *q4 = 0;

    for (int i = 0; i < num_robots; i++) {
        int x = positions[i].x;
        int y = positions[i].y;

        // Skip robots on middle lines
        if (x == mid_x || y == mid_y) {
            continue;
        }

        if (x < mid_x && y < mid_y) {
            (*q1)++;  // Top-left
        } else if (x > mid_x && y < mid_y) {
            (*q2)++;  // Top-right
        } else if (x < mid_x && y > mid_y) {
            (*q3)++;  // Bottom-left
        } else {
            (*q4)++;  // Bottom-right
        }
    }
}

// Part 1: Safety factor after 100 seconds
long part1(const Robot *robots, int num_robots) {
    Position *positions = malloc(num_robots * sizeof(Position));
    simulate(robots, num_robots, 100, positions);

    int q1, q2, q3, q4;
    count_quadrants(positions, num_robots, &q1, &q2, &q3, &q4);

    long safety_factor = (long)q1 * q2 * q3 * q4;

    free(positions);
    return safety_factor;
}

// Part 2: Find when robots form a Christmas tree pattern
int part2(const Robot *robots, int num_robots) {
    Position *positions = malloc(num_robots * sizeof(Position));

    // Look for a frame with a long horizontal line of robots
    for (int seconds = 1; seconds <= WIDTH * HEIGHT; seconds++) {
        simulate(robots, num_robots, seconds, positions);

        // Create grid to check for consecutive robots
        bool grid[HEIGHT][WIDTH];
        memset(grid, 0, sizeof(grid));

        for (int i = 0; i < num_robots; i++) {
            grid[positions[i].y][positions[i].x] = true;
        }

        // Look for a horizontal line of at least 20 consecutive robots
        for (int y = 0; y < HEIGHT; y++) {
            int max_consecutive = 0;
            int consecutive = 0;

            for (int x = 0; x < WIDTH; x++) {
                if (grid[y][x]) {
                    consecutive++;
                    if (consecutive > max_consecutive) {
                        max_consecutive = consecutive;
                    }
                } else {
                    consecutive = 0;
                }
            }

            if (max_consecutive >= 20) {
                free(positions);
                return seconds;
            }
        }
    }

    free(positions);
    return -1;
}

int main(void) {
    Robot *robots = malloc(MAX_ROBOTS * sizeof(Robot));

    int num_robots = parse_robots("../input.txt", robots);
    if (num_robots < 0) {
        free(robots);
        return 1;
    }

    printf("Part 1: %ld\n", part1(robots, num_robots));
    printf("Part 2: %d\n", part2(robots, num_robots));

    free(robots);
    return 0;
}
