import { readFileSync } from 'fs';

function parseInput(filename) {
    const content = readFileSync(filename, 'utf8').trim();
    const [valuesSection, gatesSection] = content.split('\n\n');

    // Parse initial values
    const wires = new Map();
    for (const line of valuesSection.split('\n')) {
        const [name, val] = line.split(': ');
        wires.set(name, parseInt(val));
    }

    // Parse gates
    const gates = [];
    for (const line of gatesSection.split('\n')) {
        const parts = line.split(' ');
        gates.push({
            in1: parts[0],
            op: parts[1],
            in2: parts[2],
            out: parts[4]
        });
    }

    return { wires, gates };
}

function simulate(initialWires, gates) {
    const wires = new Map(initialWires);
    let remaining = [...gates];

    while (remaining.length > 0) {
        let madeProgress = false;
        const newRemaining = [];

        for (const gate of remaining) {
            if (wires.has(gate.in1) && wires.has(gate.in2)) {
                const v1 = wires.get(gate.in1);
                const v2 = wires.get(gate.in2);
                let result;

                if (gate.op === 'AND') {
                    result = v1 & v2;
                } else if (gate.op === 'OR') {
                    result = v1 | v2;
                } else if (gate.op === 'XOR') {
                    result = v1 ^ v2;
                }

                wires.set(gate.out, result);
                madeProgress = true;
            } else {
                newRemaining.push(gate);
            }
        }

        remaining = newRemaining;
        if (!madeProgress && remaining.length > 0) {
            throw new Error("Circuit stuck - missing inputs");
        }
    }

    return wires;
}

function getZValue(wires) {
    const zWires = [...wires.keys()]
        .filter(k => k.startsWith('z'))
        .sort()
        .reverse();

    let result = 0n;
    for (const z of zWires) {
        result = (result << 1n) | BigInt(wires.get(z));
    }
    return result;
}

function part1(wires, gates) {
    const finalWires = simulate(wires, gates);
    return getZValue(finalWires);
}

function part2(gates) {
    const swapped = new Set();

    // Find the highest bit number
    const maxBit = Math.max(...gates
        .filter(g => g.out.startsWith('z'))
        .map(g => parseInt(g.out.slice(1))));

    for (const gate of gates) {
        const { in1, op, in2, out } = gate;

        // Rule: XOR gates that don't take x,y as input should output to z
        if (op === 'XOR') {
            const isXyXor = (in1.startsWith('x') || in1.startsWith('y')) &&
                           (in2.startsWith('x') || in2.startsWith('y'));
            if (!isXyXor) {
                if (!out.startsWith('z')) {
                    swapped.add(out);
                }
            }
        }

        // Rule: z outputs (except last) should come from XOR
        if (out.startsWith('z') && out !== `z${maxBit.toString().padStart(2, '0')}`) {
            if (op !== 'XOR') {
                swapped.add(out);
            }
        }

        // Rule: AND gates (except x00 AND y00) should feed into OR
        if (op === 'AND') {
            const isFirstBit = [in1, in2].includes('x00') && [in1, in2].includes('y00');
            if (!isFirstBit) {
                const usedByOr = gates.some(g =>
                    g.op === 'OR' && [g.in1, g.in2].includes(out));
                if (!usedByOr) {
                    swapped.add(out);
                }
            }
        }

        // Rule: XOR of x,y (except bit 0) should feed into XOR and AND
        if (op === 'XOR') {
            const isXyXor = (in1.startsWith('x') || in1.startsWith('y')) &&
                           (in2.startsWith('x') || in2.startsWith('y'));
            const isZ00 = [in1, in2].includes('x00') && [in1, in2].includes('y00');
            if (isXyXor && !isZ00) {
                const usedByXor = gates.some(g =>
                    g.op === 'XOR' && [g.in1, g.in2].includes(out));
                const usedByAnd = gates.some(g =>
                    g.op === 'AND' && [g.in1, g.in2].includes(out));
                if (!(usedByXor && usedByAnd)) {
                    swapped.add(out);
                }
            }
        }
    }

    return [...swapped].sort().join(',');
}

const { wires, gates } = parseInput('../input.txt');
console.log('Part 1:', part1(wires, gates).toString());
console.log('Part 2:', part2(gates));
