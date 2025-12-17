/**
 * Day 17: Chronospatial Computer - 3-bit VM emulator
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>

#define MAX_PROGRAM 32
#define MAX_OUTPUT 64

typedef struct {
    int64_t a, b, c;
    int program[MAX_PROGRAM];
    int program_len;
} VM;

/**
 * Get combo operand value based on operand type.
 */
static int64_t combo(int operand, int64_t a, int64_t b, int64_t c) {
    switch (operand) {
        case 0: case 1: case 2: case 3:
            return operand;
        case 4:
            return a;
        case 5:
            return b;
        case 6:
            return c;
        default:
            fprintf(stderr, "Invalid combo operand: %d\n", operand);
            exit(1);
    }
}

/**
 * Execute the 3-bit computer program and return output.
 * Returns the number of output values.
 */
static int run_program(int64_t a, int64_t b, int64_t c,
                       const int *program, int program_len,
                       int *output) {
    int ip = 0;
    int output_len = 0;

    while (ip < program_len) {
        int opcode = program[ip];
        int operand = program[ip + 1];

        switch (opcode) {
            case 0:  /* adv - A = A >> combo */
                a = a >> combo(operand, a, b, c);
                break;
            case 1:  /* bxl - B = B XOR literal */
                b = b ^ operand;
                break;
            case 2:  /* bst - B = combo % 8 */
                b = combo(operand, a, b, c) & 7;
                break;
            case 3:  /* jnz - jump if A != 0 */
                if (a != 0) {
                    ip = operand;
                    continue;
                }
                break;
            case 4:  /* bxc - B = B XOR C */
                b = b ^ c;
                break;
            case 5:  /* out - output combo % 8 */
                output[output_len++] = combo(operand, a, b, c) & 7;
                break;
            case 6:  /* bdv - B = A >> combo */
                b = a >> combo(operand, a, b, c);
                break;
            case 7:  /* cdv - C = A >> combo */
                c = a >> combo(operand, a, b, c);
                break;
        }

        ip += 2;
    }

    return output_len;
}

/**
 * Parse input file and populate VM structure.
 */
static void parse_input(const char *filename, VM *vm) {
    FILE *f = fopen(filename, "r");
    if (!f) {
        perror("Failed to open input file");
        exit(1);
    }

    char line[256];

    /* Read Register A */
    if (fgets(line, sizeof(line), f)) {
        sscanf(line, "Register A: %lld", &vm->a);
    }

    /* Read Register B */
    if (fgets(line, sizeof(line), f)) {
        sscanf(line, "Register B: %lld", &vm->b);
    }

    /* Read Register C */
    if (fgets(line, sizeof(line), f)) {
        sscanf(line, "Register C: %lld", &vm->c);
    }

    /* Skip blank line */
    fgets(line, sizeof(line), f);

    /* Read Program */
    if (fgets(line, sizeof(line), f)) {
        char *ptr = strstr(line, "Program: ");
        if (ptr) {
            ptr += 9;  /* Skip "Program: " */
            vm->program_len = 0;
            while (*ptr && *ptr != '\n') {
                if (*ptr >= '0' && *ptr <= '9') {
                    vm->program[vm->program_len++] = *ptr - '0';
                }
                ptr++;
            }
        }
    }

    fclose(f);
}

/**
 * Part 1: Run the program and return comma-separated output.
 */
static void part1(VM *vm, char *result) {
    int output[MAX_OUTPUT];
    int output_len = run_program(vm->a, vm->b, vm->c,
                                  vm->program, vm->program_len, output);

    char *ptr = result;
    for (int i = 0; i < output_len; i++) {
        if (i > 0) {
            *ptr++ = ',';
        }
        *ptr++ = '0' + output[i];
    }
    *ptr = '\0';
}

/**
 * Check if output matches the suffix of the program starting at target_idx.
 */
static bool matches_suffix(const int *output, int output_len,
                           const int *program, int program_len, int target_idx) {
    int expected_len = program_len - target_idx;
    if (output_len != expected_len) {
        return false;
    }
    for (int i = 0; i < expected_len; i++) {
        if (output[i] != program[target_idx + i]) {
            return false;
        }
    }
    return true;
}

/**
 * Recursively search for A value that produces program output.
 */
static int64_t search(VM *vm, int target_idx, int64_t current_a) {
    if (target_idx < 0) {
        return current_a;
    }

    int output[MAX_OUTPUT];

    /* Try all 8 possible 3-bit values for this position */
    for (int bits = 0; bits < 8; bits++) {
        int64_t candidate_a = (current_a << 3) | bits;

        /* A can't be 0 at start (would halt immediately without output) */
        if (candidate_a == 0 && target_idx == vm->program_len - 1) {
            continue;
        }

        int output_len = run_program(candidate_a, vm->b, vm->c,
                                     vm->program, vm->program_len, output);

        /* Check if output matches the suffix of the program */
        if (matches_suffix(output, output_len, vm->program, vm->program_len, target_idx)) {
            int64_t result = search(vm, target_idx - 1, candidate_a);
            if (result != -1) {
                return result;
            }
        }
    }

    return -1;
}

/**
 * Part 2: Find initial A value that makes program output itself.
 */
static int64_t part2(VM *vm) {
    return search(vm, vm->program_len - 1, 0);
}

int main(void) {
    VM vm;
    parse_input("../input.txt", &vm);

    char result1[256];
    part1(&vm, result1);
    printf("Part 1: %s\n", result1);

    int64_t result2 = part2(&vm);
    printf("Part 2: %lld\n", result2);

    return 0;
}
