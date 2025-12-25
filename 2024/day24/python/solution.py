#!/usr/bin/env python3

def parse_input(filename):
    """Parse initial wire values and gates."""
    with open(filename) as f:
        content = f.read()

    parts = content.strip().split('\n\n')

    # Parse initial values
    wires = {}
    for line in parts[0].split('\n'):
        name, val = line.split(': ')
        wires[name] = int(val)

    # Parse gates
    gates = []
    for line in parts[1].split('\n'):
        # Format: "x00 AND y00 -> z00"
        parts_line = line.split(' ')
        in1 = parts_line[0]
        op = parts_line[1]
        in2 = parts_line[2]
        out = parts_line[4]
        gates.append((in1, op, in2, out))

    return wires, gates

def simulate(wires, gates):
    """Simulate the circuit until all outputs are computed."""
    wires = dict(wires)  # Make a copy
    remaining = list(gates)

    while remaining:
        made_progress = False
        new_remaining = []

        for in1, op, in2, out in remaining:
            if in1 in wires and in2 in wires:
                v1, v2 = wires[in1], wires[in2]
                if op == 'AND':
                    wires[out] = v1 & v2
                elif op == 'OR':
                    wires[out] = v1 | v2
                elif op == 'XOR':
                    wires[out] = v1 ^ v2
                made_progress = True
            else:
                new_remaining.append((in1, op, in2, out))

        remaining = new_remaining
        if not made_progress and remaining:
            raise ValueError("Circuit stuck - missing inputs")

    return wires

def get_z_value(wires):
    """Extract the number from z wires."""
    z_wires = sorted([k for k in wires if k.startswith('z')], reverse=True)
    result = 0
    for z in z_wires:
        result = (result << 1) | wires[z]
    return result

def part1(wires, gates):
    """Simulate and get the z output."""
    final_wires = simulate(wires, gates)
    return get_z_value(final_wires)

def part2(gates):
    """Find the 8 swapped wires in the adder circuit.

    A correct ripple-carry adder has this structure:
    - Bit 0: z00 = x00 XOR y00, carry0 = x00 AND y00
    - Bit i: z[i] = (x[i] XOR y[i]) XOR carry[i-1]
             carry[i] = (x[i] AND y[i]) OR ((x[i] XOR y[i]) AND carry[i-1])

    We check structural rules to find violations.
    """
    swapped = set()

    # Build lookup: output -> (in1, op, in2)
    gate_by_output = {}
    for in1, op, in2, out in gates:
        gate_by_output[out] = (in1, op, in2)

    # Build lookup: (inputs_frozenset, op) -> output
    gate_by_inputs_op = {}
    for in1, op, in2, out in gates:
        key = (frozenset([in1, in2]), op)
        gate_by_inputs_op[key] = out

    def find_gate(a, b, op):
        """Find gate with inputs a,b and operation op."""
        key = (frozenset([a, b]), op)
        return gate_by_inputs_op.get(key)

    # Find the highest bit number
    max_bit = max(int(g[3][1:]) for g in gates if g[3].startswith('z'))

    # Rules for a correct adder:
    # 1. XOR gates should output to z (except first-level XOR of x,y)
    # 2. z outputs (except last) must come from XOR gates
    # 3. AND gates should not output directly to z (except possibly carry-out)
    # 4. OR gates should not output directly to z (except carry-out to z45)

    for in1, op, in2, out in gates:
        # Rule: XOR gates that don't take x,y as input should output to z
        if op == 'XOR':
            is_xy_xor = (in1.startswith('x') or in1.startswith('y')) and \
                        (in2.startswith('x') or in2.startswith('y'))
            if not is_xy_xor:
                # This is a second-level XOR (sum XOR carry), should output to z
                if not out.startswith('z'):
                    swapped.add(out)

        # Rule: z outputs (except z45) should come from XOR
        if out.startswith('z') and out != f'z{max_bit:02d}':
            if op != 'XOR':
                swapped.add(out)

        # Rule: AND gates (except x00 AND y00) should feed into OR
        if op == 'AND':
            is_first_bit = 'x00' in [in1, in2] and 'y00' in [in1, in2]
            if not is_first_bit:
                # This AND output should be input to an OR gate
                used_by_or = False
                for in1b, op2, in2b, out2 in gates:
                    if op2 == 'OR' and out in [in1b, in2b]:
                        used_by_or = True
                        break
                if not used_by_or:
                    swapped.add(out)

        # Rule: XOR of x,y should feed into another XOR (for z output) or AND (for carry)
        if op == 'XOR':
            is_xy_xor = (in1.startswith('x') or in1.startswith('y')) and \
                        (in2.startswith('x') or in2.startswith('y'))
            # Skip z00 which is x00 XOR y00 directly
            is_z00 = 'x00' in [in1, in2] and 'y00' in [in1, in2]
            if is_xy_xor and not is_z00:
                # Should be used by XOR and AND
                used_by_xor = False
                used_by_and = False
                for in1b, op2, in2b, out2 in gates:
                    if out in [in1b, in2b]:
                        if op2 == 'XOR':
                            used_by_xor = True
                        elif op2 == 'AND':
                            used_by_and = True
                if not (used_by_xor and used_by_and):
                    swapped.add(out)

    return ','.join(sorted(swapped))

def main():
    wires, gates = parse_input('../input.txt')

    print('Part 1:', part1(wires, gates))
    print('Part 2:', part2(gates))

if __name__ == '__main__':
    main()
