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

// Part 1: Count all paths from 'you' to 'out' using memoization
function part1(graph) {
    var memo = {};

    function countPaths(node, memo, graph) {
        // Check memo cache
        if (structKeyExists(memo, node)) {
            return memo[node];
        }

        // Base case: reached destination
        if (node == 'out') {
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
            total += countPaths(neighbor, memo, graph);
        }

        // Cache and return
        memo[node] = total;
        return total;
    }

    return countPaths('you', memo, graph);
}

// Part 2: Count paths from 'svr' to 'out' that visit both 'dac' and 'fft'
function part2(graph) {
    // Helper function to count paths from each node to a target
    function countPathsToTarget(node, target, memo, graph) {
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
            total += countPathsToTarget(neighbor, target, memo, graph);
        }

        // Cache and return
        memo[node] = total;
        return total;
    }

    // Create separate memos for each target
    var memoOut = {};
    var memoDac = {};
    var memoFft = {};

    // Count paths to each target
    var svrToOut = countPathsToTarget('svr', 'out', memoOut, graph);
    var svrToDac = countPathsToTarget('svr', 'dac', memoDac, graph);
    var svrToFft = countPathsToTarget('svr', 'fft', memoFft, graph);

    // Reset memos for next calculations
    memoDac = {};
    memoFft = {};
    memoOut = {};

    var dacToFft = countPathsToTarget('dac', 'fft', memoFft, graph);
    var dacToOut = countPathsToTarget('dac', 'out', memoOut, graph);

    // Reset memos
    memoDac = {};
    memoOut = {};

    var fftToDac = countPathsToTarget('fft', 'dac', memoDac, graph);
    var fftToOut = countPathsToTarget('fft', 'out', memoOut, graph);

    // Paths that visit dac before fft: svr -> dac -> fft -> out
    // Paths that visit fft before dac: svr -> fft -> dac -> out
    // Need to use java.math.BigInteger for large numbers
    var BigInteger = createObject("java", "java.math.BigInteger");

    var svrToDacBig = BigInteger.init(toString(svrToDac));
    var dacToFftBig = BigInteger.init(toString(dacToFft));
    var fftToOutBig = BigInteger.init(toString(fftToOut));

    var dacBeforeFft = svrToDacBig.multiply(dacToFftBig).multiply(fftToOutBig);

    var svrToFftBig = BigInteger.init(toString(svrToFft));
    var fftToDacBig = BigInteger.init(toString(fftToDac));
    var dacToOutBig = BigInteger.init(toString(dacToOut));

    var fftBeforeDac = svrToFftBig.multiply(fftToDacBig).multiply(dacToOutBig);

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
