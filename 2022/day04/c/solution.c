#include <stdio.h>
#include <stdlib.h>

int fully_contains(int a1, int b1, int a2, int b2) {
    // Check if one range fully contains the other
    return (a1 <= a2 && b1 >= b2) || (a2 <= a1 && b2 >= b1);
}

int overlaps(int a1, int b1, int a2, int b2) {
    // Check if ranges overlap at all
    return a1 <= b2 && a2 <= b1;
}

int main(void) {
    FILE *fp = fopen("../input.txt", "r");
    if (!fp) {
        perror("Failed to open input file");
        return 1;
    }

    int part1 = 0;
    int part2 = 0;
    int a1, b1, a2, b2;

    while (fscanf(fp, "%d-%d,%d-%d", &a1, &b1, &a2, &b2) == 4) {
        if (fully_contains(a1, b1, a2, b2)) {
            part1++;
        }
        if (overlaps(a1, b1, a2, b2)) {
            part2++;
        }
    }

    fclose(fp);

    printf("Part 1: %d\n", part1);
    printf("Part 2: %d\n", part2);

    return 0;
}
