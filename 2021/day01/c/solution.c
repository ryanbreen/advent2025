#include <stdio.h>
#include <stdlib.h>

#define MAX_DEPTHS 2048

int depths[MAX_DEPTHS];
int count = 0;

void read_input(const char *filename) {
    FILE *fp = fopen(filename, "r");
    if (!fp) {
        perror("Failed to open input file");
        exit(1);
    }

    int value;
    while (fscanf(fp, "%d", &value) == 1 && count < MAX_DEPTHS) {
        depths[count++] = value;
    }

    fclose(fp);
}

int part1(void) {
    int increases = 0;
    for (int i = 1; i < count; i++) {
        if (depths[i] > depths[i - 1]) {
            increases++;
        }
    }
    return increases;
}

int part2(void) {
    int increases = 0;
    /* Compare sliding windows of size 3
     * Window at i: depths[i] + depths[i+1] + depths[i+2]
     * Window at i+1: depths[i+1] + depths[i+2] + depths[i+3]
     * The middle two terms cancel, so we just compare depths[i+3] > depths[i]
     */
    for (int i = 0; i < count - 3; i++) {
        if (depths[i + 3] > depths[i]) {
            increases++;
        }
    }
    return increases;
}

int main(void) {
    read_input("../input.txt");

    printf("Part 1: %d\n", part1());
    printf("Part 2: %d\n", part2());

    return 0;
}
