<cfscript>
// Advent of Code 2024 - Day 24: Crossed Wires

function parseInput(filename) {
    var content = fileRead(filename);
    var sections = listToArray(trim(content), chr(10) & chr(10));

    // Parse initial wire values
    var wires = {};
    var initialLines = listToArray(sections[1], chr(10));
    for (var line in initialLines) {
        var parts = listToArray(line, ': ');
        wires[parts[1]] = val(parts[2]);
    }

    // Parse gates
    var gates = [];
    var gateLines = listToArray(sections[2], chr(10));
    for (var line in gateLines) {
        var parts = listToArray(line, ' ');
        arrayAppend(gates, {
            'in1': parts[1],
            'op': parts[2],
            'in2': parts[3],
            'out': parts[5]
        });
    }

    return {
        'wires': wires,
        'gates': gates
    };
}

function simulate(initialWires, gates) {
    // Make a copy of wires
    var wires = duplicate(initialWires);
    var remaining = duplicate(gates);

    while (arrayLen(remaining) > 0) {
        var madeProgress = false;
        var newRemaining = [];

        for (var gate in remaining) {
            var in1 = gate.in1;
            var in2 = gate.in2;
            var op = gate.op;
            var out = gate.out;

            if (structKeyExists(wires, in1) && structKeyExists(wires, in2)) {
                var v1 = wires[in1];
                var v2 = wires[in2];

                if (op == 'AND') {
                    wires[out] = bitAnd(v1, v2);
                } else if (op == 'OR') {
                    wires[out] = bitOr(v1, v2);
                } else if (op == 'XOR') {
                    wires[out] = bitXor(v1, v2);
                }

                madeProgress = true;
            } else {
                arrayAppend(newRemaining, gate);
            }
        }

        remaining = newRemaining;

        if (!madeProgress && arrayLen(remaining) > 0) {
            throw("Circuit stuck - missing inputs");
        }
    }

    return wires;
}

function getZValue(wires) {
    // Get all z wires and sort them in descending order
    var zWires = [];
    for (var key in wires) {
        if (left(key, 1) == 'z') {
            arrayAppend(zWires, key);
        }
    }
    arraySort(zWires, 'textnocase', 'desc');

    // Build the number from MSB to LSB
    var result = 0;
    for (var z in zWires) {
        result = bitSHLN(result, 1);
        result = bitOr(result, wires[z]);
    }

    return result;
}

function part1(wires, gates) {
    var finalWires = simulate(wires, gates);
    return getZValue(finalWires);
}

function part2(gates) {
    var swapped = {};

    // Build lookup: output -> gate
    var gateByOutput = {};
    for (var gate in gates) {
        gateByOutput[gate.out] = gate;
    }

    // Build lookup: (inputs_set, op) -> output
    var gateByInputsOp = {};
    for (var gate in gates) {
        var inputs = [gate.in1, gate.in2];
        arraySort(inputs, 'text');
        var key = inputs[1] & '|' & inputs[2] & '|' & gate.op;
        gateByInputsOp[key] = gate.out;
    }

    function findGate(a, b, op) {
        var inputs = [a, b];
        arraySort(inputs, 'text');
        var key = inputs[1] & '|' & inputs[2] & '|' & op;
        if (structKeyExists(gateByInputsOp, key)) {
            return gateByInputsOp[key];
        }
        return '';
    }

    // Find the highest bit number
    var maxBit = 0;
    for (var gate in gates) {
        if (left(gate.out, 1) == 'z') {
            var bitNum = val(mid(gate.out, 2, 2));
            if (bitNum > maxBit) {
                maxBit = bitNum;
            }
        }
    }

    for (var gate in gates) {
        var in1 = gate.in1;
        var in2 = gate.in2;
        var op = gate.op;
        var out = gate.out;

        // Rule: XOR gates that don't take x,y as input should output to z
        if (op == 'XOR') {
            var isXyXor = (left(in1, 1) == 'x' || left(in1, 1) == 'y') &&
                          (left(in2, 1) == 'x' || left(in2, 1) == 'y');

            if (!isXyXor) {
                // This is a second-level XOR (sum XOR carry), should output to z
                if (left(out, 1) != 'z') {
                    swapped[out] = true;
                }
            }
        }

        // Rule: z outputs (except highest bit) should come from XOR
        if (left(out, 1) == 'z') {
            var outBitNum = val(mid(out, 2, 2));
            if (outBitNum != maxBit) {
                if (op != 'XOR') {
                    swapped[out] = true;
                }
            }
        }

        // Rule: AND gates (except x00 AND y00) should feed into OR
        if (op == 'AND') {
            var isFirstBit = (in1 == 'x00' && in2 == 'y00') ||
                             (in1 == 'y00' && in2 == 'x00');

            if (!isFirstBit) {
                // This AND output should be input to an OR gate
                var usedByOr = false;
                for (var g2 in gates) {
                    if (g2.op == 'OR' && (g2.in1 == out || g2.in2 == out)) {
                        usedByOr = true;
                        break;
                    }
                }
                if (!usedByOr) {
                    swapped[out] = true;
                }
            }
        }

        // Rule: XOR of x,y should feed into another XOR (for z output) or AND (for carry)
        if (op == 'XOR') {
            var isXyXor = (left(in1, 1) == 'x' || left(in1, 1) == 'y') &&
                          (left(in2, 1) == 'x' || left(in2, 1) == 'y');

            // Skip z00 which is x00 XOR y00 directly
            var isZ00 = (in1 == 'x00' && in2 == 'y00') ||
                        (in1 == 'y00' && in2 == 'x00');

            if (isXyXor && !isZ00) {
                // Should be used by XOR and AND
                var usedByXor = false;
                var usedByAnd = false;

                for (var g2 in gates) {
                    if (g2.in1 == out || g2.in2 == out) {
                        if (g2.op == 'XOR') {
                            usedByXor = true;
                        } else if (g2.op == 'AND') {
                            usedByAnd = true;
                        }
                    }
                }

                if (!(usedByXor && usedByAnd)) {
                    swapped[out] = true;
                }
            }
        }
    }

    // Sort and join the swapped wires
    var swappedList = structKeyArray(swapped);
    arraySort(swappedList, 'text');
    return arrayToList(swappedList, ',');
}

// Main execution
var inputFile = expandPath('../input.txt');
var parsed = parseInput(inputFile);
var wires = parsed.wires;
var gates = parsed.gates;

writeOutput('Part 1: ' & part1(wires, gates) & chr(10));
writeOutput('Part 2: ' & part2(gates) & chr(10));
</cfscript>
