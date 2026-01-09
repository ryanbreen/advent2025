#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#define MAX_MONKEYS 10
#define MAX_ITEMS 100

typedef struct {
    uint64_t items[MAX_ITEMS];
    int item_count;
    char op;           // '+' or '*'
    int operand;       // -1 means "old"
    int divisor;
    int if_true;
    int if_false;
    uint64_t inspections;
} Monkey;

int parse_monkeys(FILE *f, Monkey *monkeys) {
    char line[256];
    int monkey_count = 0;

    while (fgets(line, sizeof(line), f)) {
        if (strncmp(line, "Monkey", 6) == 0) {
            Monkey *m = &monkeys[monkey_count];
            m->item_count = 0;
            m->inspections = 0;

            // Starting items
            fgets(line, sizeof(line), f);
            char *p = strchr(line, ':') + 1;
            while (*p) {
                while (*p && (*p == ' ' || *p == ',')) p++;
                if (*p >= '0' && *p <= '9') {
                    m->items[m->item_count++] = (uint64_t)atoi(p);
                    while (*p >= '0' && *p <= '9') p++;
                } else {
                    break;
                }
            }

            // Operation
            fgets(line, sizeof(line), f);
            p = strstr(line, "old ");
            if (p) {
                p += 4;
                m->op = *p;
                p += 2;
                if (strncmp(p, "old", 3) == 0) {
                    m->operand = -1;
                } else {
                    m->operand = atoi(p);
                }
            }

            // Divisor
            fgets(line, sizeof(line), f);
            p = strstr(line, "by ");
            m->divisor = atoi(p + 3);

            // If true
            fgets(line, sizeof(line), f);
            p = strstr(line, "monkey ");
            m->if_true = atoi(p + 7);

            // If false
            fgets(line, sizeof(line), f);
            p = strstr(line, "monkey ");
            m->if_false = atoi(p + 7);

            monkey_count++;
        }
    }
    return monkey_count;
}

uint64_t apply_operation(uint64_t old, char op, int operand) {
    uint64_t val = (operand == -1) ? old : (uint64_t)operand;
    if (op == '+') {
        return old + val;
    } else {
        return old * val;
    }
}

void simulate(Monkey *monkeys, int count, int rounds, int relief_divisor, int use_modulo) {
    // Calculate product of all divisors for modulo
    uint64_t mod_value = 1;
    if (use_modulo) {
        for (int i = 0; i < count; i++) {
            mod_value *= monkeys[i].divisor;
        }
    }

    for (int round = 0; round < rounds; round++) {
        for (int i = 0; i < count; i++) {
            Monkey *m = &monkeys[i];
            while (m->item_count > 0) {
                uint64_t item = m->items[0];
                // Remove first item by shifting
                for (int j = 0; j < m->item_count - 1; j++) {
                    m->items[j] = m->items[j + 1];
                }
                m->item_count--;
                m->inspections++;

                // Apply operation
                uint64_t new_val = apply_operation(item, m->op, m->operand);

                // Apply relief
                if (relief_divisor > 1) {
                    new_val /= relief_divisor;
                }

                // Apply modulo
                if (use_modulo) {
                    new_val %= mod_value;
                }

                // Test and throw
                int target;
                if (new_val % m->divisor == 0) {
                    target = m->if_true;
                } else {
                    target = m->if_false;
                }
                monkeys[target].items[monkeys[target].item_count++] = new_val;
            }
        }
    }
}

uint64_t monkey_business(Monkey *monkeys, int count) {
    // Find top 2 inspection counts
    uint64_t top1 = 0, top2 = 0;
    for (int i = 0; i < count; i++) {
        if (monkeys[i].inspections > top1) {
            top2 = top1;
            top1 = monkeys[i].inspections;
        } else if (monkeys[i].inspections > top2) {
            top2 = monkeys[i].inspections;
        }
    }
    return top1 * top2;
}

void reset_monkeys(Monkey *dest, Monkey *src, int count) {
    for (int i = 0; i < count; i++) {
        dest[i] = src[i];
        dest[i].inspections = 0;
    }
}

int main() {
    FILE *f = fopen("../input.txt", "r");
    if (!f) {
        perror("Cannot open input.txt");
        return 1;
    }

    Monkey original[MAX_MONKEYS];
    int count = parse_monkeys(f, original);
    fclose(f);

    // Part 1
    Monkey monkeys[MAX_MONKEYS];
    reset_monkeys(monkeys, original, count);
    simulate(monkeys, count, 20, 3, 0);
    printf("Part 1: %llu\n", monkey_business(monkeys, count));

    // Part 2
    reset_monkeys(monkeys, original, count);
    simulate(monkeys, count, 10000, 1, 1);
    printf("Part 2: %llu\n", monkey_business(monkeys, count));

    return 0;
}
