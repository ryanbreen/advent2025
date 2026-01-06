component {

    function run() {
        // Read input file
        var inputPath = getCurrentTemplatePath();
        var inputDir = getDirectoryFromPath(inputPath);
        var inputFile = inputDir & "../input.txt";
        var inputText = fileRead(inputFile).trim();

        // Parse the input
        var parsed = parseInput(inputText);
        var instructions = parsed.instructions;
        var network = parsed.network;

        // Solve Part 1
        var part1Result = part1(instructions, network);
        writeOutput("Part 1: " & part1Result & chr(10));

        // Solve Part 2
        var part2Result = part2(instructions, network);
        writeOutput("Part 2: " & part2Result & chr(10));
    }

    function parseInput(text) {
        var lines = listToArray(text, chr(10));
        // Note: listToArray skips empty lines, so line 2 is already first network entry
        var instructions = lines[1];

        var network = {};
        for (var i = 2; i <= arrayLen(lines); i++) {
            var line = trim(lines[i]);
            if (len(line) == 0) continue;

            // Parse: AAA = (BBB, CCC)
            var parts = listToArray(line, "=");
            if (arrayLen(parts) < 2) continue;

            var node = trim(parts[1]);
            var rest = trim(parts[2]);

            // Remove parentheses
            rest = replace(rest, "(", "", "all");
            rest = replace(rest, ")", "", "all");

            var leftRight = listToArray(rest, ",");
            if (arrayLen(leftRight) < 2) continue;

            var leftNode = trim(leftRight[1]);
            var rightNode = trim(leftRight[2]);

            network[node] = {left: leftNode, right: rightNode};
        }

        return {instructions: instructions, network: network};
    }

    function traverse(startNode, endCondition, instructions, network) {
        // Navigate from startNode until endCondition is met
        var currentNode = startNode;
        var stepCount = 0;
        var instructionLen = len(instructions);

        while (!endCondition(currentNode)) {
            var instructionIdx = (stepCount mod instructionLen) + 1;
            var direction = mid(instructions, instructionIdx, 1);

            if (direction == "L") {
                currentNode = network[currentNode].left;
            } else {
                currentNode = network[currentNode].right;
            }
            stepCount++;
        }

        return stepCount;
    }

    function part1(instructions, network) {
        // Navigate from AAA to ZZZ following L/R instructions
        var isEndNode = function(node) {
            return node == "ZZZ";
        };

        return traverse("AAA", isEndNode, instructions, network);
    }

    function part2(instructions, network) {
        // Find all starting nodes (ending in A) using arrayFilter
        var startNodes = arrayFilter(structKeyArray(network), function(node) {
            return right(node, 1) == "A";
        });

        // Define end condition for Part 2 (node ends in Z)
        var isEndNode = function(node) {
            return right(node, 1) == "Z";
        };

        // For each starting node, find the cycle length to reach a Z node
        var cycleLengths = [];
        for (var nodeIdx = 1; nodeIdx <= arrayLen(startNodes); nodeIdx++) {
            var cycleLength = traverse(startNodes[nodeIdx], isEndNode, instructions, network);
            arrayAppend(cycleLengths, cycleLength);
        }

        // Find LCM of all cycle lengths
        var result = cycleLengths[1];
        for (var cycleIdx = 2; cycleIdx <= arrayLen(cycleLengths); cycleIdx++) {
            result = lcm(result, cycleLengths[cycleIdx]);
        }

        return result;
    }

    function gcd(a, b) {
        // Greatest common divisor using Euclidean algorithm
        while (b != 0) {
            var temp = b;
            b = a mod b;
            a = temp;
        }
        return a;
    }

    function lcm(a, b) {
        // Least common multiple
        return (a / gcd(a, b)) * b;
    }
}
