#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h>

#define MAX_RACES 10
#define MAX_LINE 256

long long count_ways_to_win(long long time, long long record) {
    /*
     * Count the number of ways to beat the record.
     *
     * If we hold the button for t ms, we travel t * (time - t) mm.
     * We need: t * (time - t) > record
     * Solving: -t^2 + time*t - record > 0
     * Roots: t = (time +/- sqrt(time^2 - 4*record)) / 2
     */
    double discriminant = (double)time * time - 4.0 * record;
    if (discriminant <= 0) {
        return 0;
    }

    double sqrt_d = sqrt(discriminant);
    double t_low = (time - sqrt_d) / 2.0;
    double t_high = (time + sqrt_d) / 2.0;

    /* We need integer values strictly between the roots */
    long long first = (long long)floor(t_low) + 1;
    long long last = (long long)ceil(t_high) - 1;

    if (last < first) {
        return 0;
    }
    return last - first + 1;
}

int parse_numbers(const char *line, long long *nums, int max_nums) {
    int count = 0;
    const char *p = strchr(line, ':');
    if (!p) return 0;
    p++; /* skip colon */

    while (*p && count < max_nums) {
        while (*p && isspace(*p)) p++;
        if (!*p) break;

        long long num = 0;
        while (*p && isdigit(*p)) {
            num = num * 10 + (*p - '0');
            p++;
        }
        nums[count++] = num;
    }
    return count;
}

long long concat_numbers(long long *nums, int count) {
    /* Concatenate all numbers into one by converting to string representation */
    char buffer[128] = "";
    for (int i = 0; i < count; i++) {
        char temp[32];
        sprintf(temp, "%lld", nums[i]);
        strcat(buffer, temp);
    }
    return atoll(buffer);
}

int main(void) {
    FILE *fp = fopen("../input.txt", "r");
    if (!fp) {
        perror("Failed to open input.txt");
        return 1;
    }

    char line1[MAX_LINE], line2[MAX_LINE];
    if (!fgets(line1, sizeof(line1), fp) || !fgets(line2, sizeof(line2), fp)) {
        fprintf(stderr, "Failed to read input lines\n");
        fclose(fp);
        return 1;
    }
    fclose(fp);

    long long times[MAX_RACES], distances[MAX_RACES];
    int num_races = parse_numbers(line1, times, MAX_RACES);
    int num_distances = parse_numbers(line2, distances, MAX_RACES);

    if (num_races != num_distances || num_races == 0) {
        fprintf(stderr, "Invalid input: mismatched race count\n");
        return 1;
    }

    /* Part 1: Multiply ways to win for each race */
    long long part1 = 1;
    for (int i = 0; i < num_races; i++) {
        long long ways = count_ways_to_win(times[i], distances[i]);
        part1 *= ways;
    }

    /* Part 2: Concatenate times and distances, count ways for single race */
    long long total_time = concat_numbers(times, num_races);
    long long total_distance = concat_numbers(distances, num_races);
    long long part2 = count_ways_to_win(total_time, total_distance);

    printf("Part 1: %lld\n", part1);
    printf("Part 2: %lld\n", part2);

    return 0;
}
