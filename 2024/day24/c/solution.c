#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define MAX_WIRES 1000
#define MAX_GATES 500
#define MAX_NAME 4

typedef enum {
    AND,
    OR,
    XOR
} Operation;

typedef struct {
    char in1[MAX_NAME];
    char in2[MAX_NAME];
    Operation op;
    char out[MAX_NAME];
} Gate;

typedef struct {
    char name[MAX_NAME];
    int value;
    bool has_value;
} Wire;

Wire wires[MAX_WIRES];
int wire_count = 0;
Gate gates[MAX_GATES];
int gate_count = 0;

int find_wire(const char *name) {
    for (int i = 0; i < wire_count; i++) {
        if (strcmp(wires[i].name, name) == 0) {
            return i;
        }
    }
    return -1;
}

int get_or_create_wire(const char *name) {
    int idx = find_wire(name);
    if (idx >= 0) return idx;

    if (wire_count >= MAX_WIRES) {
        fprintf(stderr, "Error: Too many wires (max %d)\n", MAX_WIRES);
        exit(1);
    }

    strncpy(wires[wire_count].name, name, MAX_NAME - 1);
    wires[wire_count].name[MAX_NAME - 1] = '\0';
    wires[wire_count].value = 0;
    wires[wire_count].has_value = false;
    return wire_count++;
}

void parse_input(const char *filename) {
    FILE *f = fopen(filename, "r");
    if (!f) {
        perror("Failed to open input file");
        exit(1);
    }

    char line[256];
    bool reading_gates = false;

    while (fgets(line, sizeof(line), f)) {
        // Remove newline
        line[strcspn(line, "\n")] = 0;

        if (strlen(line) == 0) {
            reading_gates = true;
            continue;
        }

        if (!reading_gates) {
            // Parse initial wire values: "x00: 1"
            char name[MAX_NAME];
            int value;
            sscanf(line, "%3s: %d", name, &value);
            int idx = get_or_create_wire(name);
            wires[idx].value = value;
            wires[idx].has_value = true;
        } else {
            // Parse gates: "x00 AND y00 -> z00"
            char in1[MAX_NAME], op_str[4], in2[MAX_NAME], out[MAX_NAME];
            sscanf(line, "%3s %3s %3s -> %3s", in1, op_str, in2, out);

            if (gate_count >= MAX_GATES) {
                fprintf(stderr, "Error: Too many gates (max %d)\n", MAX_GATES);
                exit(1);
            }

            strncpy(gates[gate_count].in1, in1, MAX_NAME - 1);
            gates[gate_count].in1[MAX_NAME - 1] = '\0';
            strncpy(gates[gate_count].in2, in2, MAX_NAME - 1);
            gates[gate_count].in2[MAX_NAME - 1] = '\0';
            strncpy(gates[gate_count].out, out, MAX_NAME - 1);
            gates[gate_count].out[MAX_NAME - 1] = '\0';

            if (strcmp(op_str, "AND") == 0) {
                gates[gate_count].op = AND;
            } else if (strcmp(op_str, "OR") == 0) {
                gates[gate_count].op = OR;
            } else {
                gates[gate_count].op = XOR;
            }

            // Ensure all wires exist
            get_or_create_wire(in1);
            get_or_create_wire(in2);
            get_or_create_wire(out);

            gate_count++;
        }
    }

    fclose(f);
}

void simulate() {
    bool made_progress;
    do {
        made_progress = false;

        for (int i = 0; i < gate_count; i++) {
            int in1_idx = find_wire(gates[i].in1);
            int in2_idx = find_wire(gates[i].in2);
            int out_idx = find_wire(gates[i].out);

            if (wires[in1_idx].has_value && wires[in2_idx].has_value && !wires[out_idx].has_value) {
                int v1 = wires[in1_idx].value;
                int v2 = wires[in2_idx].value;
                int result;

                switch (gates[i].op) {
                    case AND:
                        result = v1 & v2;
                        break;
                    case OR:
                        result = v1 | v2;
                        break;
                    case XOR:
                        result = v1 ^ v2;
                        break;
                }

                wires[out_idx].value = result;
                wires[out_idx].has_value = true;
                made_progress = true;
            }
        }
    } while (made_progress);
}

long long get_z_value() {
    // Collect all z wires
    char z_wires[100][MAX_NAME];
    int z_count = 0;

    for (int i = 0; i < wire_count; i++) {
        if (wires[i].name[0] == 'z') {
            strcpy(z_wires[z_count++], wires[i].name);
        }
    }

    // Sort z wires in descending order (z45, z44, ..., z00)
    for (int i = 0; i < z_count - 1; i++) {
        for (int j = i + 1; j < z_count; j++) {
            if (strcmp(z_wires[i], z_wires[j]) < 0) {
                char temp[MAX_NAME];
                strcpy(temp, z_wires[i]);
                strcpy(z_wires[i], z_wires[j]);
                strcpy(z_wires[j], temp);
            }
        }
    }

    // Build the number
    long long result = 0;
    for (int i = 0; i < z_count; i++) {
        int idx = find_wire(z_wires[i]);
        result = (result << 1) | wires[idx].value;
    }

    return result;
}

int find_gate_by_output(const char *out) {
    for (int i = 0; i < gate_count; i++) {
        if (strcmp(gates[i].out, out) == 0) {
            return i;
        }
    }
    return -1;
}

int find_gate_by_inputs_op(const char *a, const char *b, Operation op) {
    for (int i = 0; i < gate_count; i++) {
        if (gates[i].op == op) {
            if ((strcmp(gates[i].in1, a) == 0 && strcmp(gates[i].in2, b) == 0) ||
                (strcmp(gates[i].in1, b) == 0 && strcmp(gates[i].in2, a) == 0)) {
                return i;
            }
        }
    }
    return -1;
}

bool is_input_to_gate_with_op(const char *wire, Operation op) {
    for (int i = 0; i < gate_count; i++) {
        if (gates[i].op == op) {
            if (strcmp(gates[i].in1, wire) == 0 || strcmp(gates[i].in2, wire) == 0) {
                return true;
            }
        }
    }
    return false;
}

bool starts_with(const char *str, const char *prefix) {
    return strncmp(str, prefix, strlen(prefix)) == 0;
}

char *part2() {
    static char result[256];
    char swapped[32][MAX_NAME];  // Increased to handle more potential swaps
    int swap_count = 0;

    // Find the highest z bit
    int max_bit = 0;
    for (int i = 0; i < gate_count; i++) {
        if (gates[i].out[0] == 'z') {
            int bit = atoi(gates[i].out + 1);
            if (bit > max_bit) max_bit = bit;
        }
    }

    char max_z[MAX_NAME];
    sprintf(max_z, "z%02d", max_bit);

    for (int i = 0; i < gate_count; i++) {
        Gate *g = &gates[i];

        // Rule: XOR gates that don't take x,y as input should output to z
        if (g->op == XOR) {
            bool is_xy_xor = (starts_with(g->in1, "x") || starts_with(g->in1, "y")) &&
                            (starts_with(g->in2, "x") || starts_with(g->in2, "y"));

            if (!is_xy_xor) {
                // This is a second-level XOR, should output to z
                if (!starts_with(g->out, "z")) {
                    if (swap_count < 32) {
                        strcpy(swapped[swap_count++], g->out);
                    }
                }
            }
        }

        // Rule: z outputs (except final carry) should come from XOR
        if (starts_with(g->out, "z") && strcmp(g->out, max_z) != 0) {
            if (g->op != XOR) {
                if (swap_count < 32) {
                    strcpy(swapped[swap_count++], g->out);
                }
            }
        }

        // Rule: AND gates (except x00 AND y00) should feed into OR
        if (g->op == AND) {
            bool is_first_bit = (strcmp(g->in1, "x00") == 0 && strcmp(g->in2, "y00") == 0) ||
                               (strcmp(g->in1, "y00") == 0 && strcmp(g->in2, "x00") == 0);

            if (!is_first_bit) {
                // This AND output should be input to an OR gate
                if (!is_input_to_gate_with_op(g->out, OR)) {
                    if (swap_count < 32) {
                        strcpy(swapped[swap_count++], g->out);
                    }
                }
            }
        }

        // Rule: XOR of x,y should feed into XOR and AND
        if (g->op == XOR) {
            bool is_xy_xor = (starts_with(g->in1, "x") || starts_with(g->in1, "y")) &&
                            (starts_with(g->in2, "x") || starts_with(g->in2, "y"));

            bool is_z00 = (strcmp(g->in1, "x00") == 0 && strcmp(g->in2, "y00") == 0) ||
                         (strcmp(g->in1, "y00") == 0 && strcmp(g->in2, "x00") == 0);

            if (is_xy_xor && !is_z00) {
                bool used_by_xor = is_input_to_gate_with_op(g->out, XOR);
                bool used_by_and = is_input_to_gate_with_op(g->out, AND);

                if (!(used_by_xor && used_by_and)) {
                    if (swap_count < 32) {
                        strcpy(swapped[swap_count++], g->out);
                    }
                }
            }
        }
    }

    // Remove duplicates
    for (int i = 0; i < swap_count - 1; i++) {
        for (int j = i + 1; j < swap_count; j++) {
            if (strcmp(swapped[i], swapped[j]) == 0) {
                // Remove duplicate
                for (int k = j; k < swap_count - 1; k++) {
                    strcpy(swapped[k], swapped[k + 1]);
                }
                swap_count--;
                j--;
            }
        }
    }

    // Sort alphabetically
    for (int i = 0; i < swap_count - 1; i++) {
        for (int j = i + 1; j < swap_count; j++) {
            if (strcmp(swapped[i], swapped[j]) > 0) {
                char temp[MAX_NAME];
                strcpy(temp, swapped[i]);
                strcpy(swapped[i], swapped[j]);
                strcpy(swapped[j], temp);
            }
        }
    }

    // Build comma-separated result
    result[0] = '\0';
    for (int i = 0; i < swap_count; i++) {
        if (i > 0) strcat(result, ",");
        strcat(result, swapped[i]);
    }

    return result;
}

int main() {
    parse_input("../input.txt");

    // Part 1: Simulate the circuit
    simulate();
    long long z_value = get_z_value();
    printf("Part 1: %lld\n", z_value);

    // Part 2: Find swapped wires
    char *swapped_wires = part2();
    printf("Part 2: %s\n", swapped_wires);

    return 0;
}
