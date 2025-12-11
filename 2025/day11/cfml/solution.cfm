<cfscript>
// Parse input into a graph (adjacency list)
function parseInput(filename) {
    var graph = {};
    var fileContent = fileRead(filename);
    var lines = listToArray(fileContent, chr(10) & chr(13));

    for (var line in lines) {
        line = trim(line);
        if (len(line) == 0) continue;

        var parts = listToArray(line, ':');
        if (arrayLen(parts) < 2) continue;

        var node = trim(parts[1]);
        var neighborsStr = trim(parts[2]);
        var neighbors = [];

        if (len(neighborsStr) > 0) {
            neighbors = listToArray(neighborsStr, ' ');
        }

        graph[node] = neighbors;
    }

    return graph;
}

// Helper function to count paths from each node to a target with memoization
function countPathsToTarget(target, graph) {
    var memo = {};

    function countPaths(node) {
        // Check memo cache
        if (structKeyExists(memo, node)) {
            return memo[node];
        }

        // Base case: reached target
        if (node == target) {
            return 1;
        }

        // Base case: node not in graph
        if (!structKeyExists(graph, node)) {
            return 0;
        }

        // Recursive case: sum paths from all neighbors
        var total = 0;
        var neighbors = graph[node];
        for (var neighbor in neighbors) {
            total += countPaths(neighbor);
        }

        // Cache and return
        memo[node] = total;
        return total;
    }

    // Return a struct with the counting function
    return {
        count: function(node) {
            return countPaths(node);
        }
    };
}

// Helper function to convert a number to BigInteger
function toBigInteger(value) {
    var BigInteger = createObject("java", "java.math.BigInteger");
    return BigInteger.init(toString(value));
}

// Part 1: Count all paths from 'you' to 'out' using memoization
function part1(graph) {
    var pathCounter = countPathsToTarget('out', graph);
    return pathCounter.count('you');
}

// Part 2: Count paths from 'svr' to 'out' that visit both 'dac' and 'fft'
function part2(graph) {
    // Create path counters for each target (memos are reused across calls)
    var pathsToOut = countPathsToTarget('out', graph);
    var pathsToDac = countPathsToTarget('dac', graph);
    var pathsToFft = countPathsToTarget('fft', graph);

    // Paths that visit dac before fft: svr -> dac -> fft -> out
    var dacBeforeFft = toBigInteger(pathsToDac.count('svr'))
        .multiply(toBigInteger(pathsToFft.count('dac')))
        .multiply(toBigInteger(pathsToOut.count('fft')));

    // Paths that visit fft before dac: svr -> fft -> dac -> out
    var fftBeforeDac = toBigInteger(pathsToFft.count('svr'))
        .multiply(toBigInteger(pathsToDac.count('fft')))
        .multiply(toBigInteger(pathsToOut.count('dac')));

    var result = dacBeforeFft.add(fftBeforeDac);

    return result.toString();
}

// Main execution
var inputFile = '../input.txt';
var graph = parseInput(inputFile);

var answer1 = part1(graph);
var answer2 = part2(graph);

writeOutput('Part 1: ' & answer1 & chr(10));
writeOutput('Part 2: ' & answer2 & chr(10));
</cfscript>
