<cfscript>
// Read input file
inputPath = getDirectoryFromPath(getCurrentTemplatePath()) & "../input.txt";
inputText = fileRead(inputPath).trim();
lines = inputText.listToArray(chr(10));

function part1() {
    var total = 0;

    for (var line in lines) {
        var n = len(line);

        // Precompute max suffix: max_suffix[i] = max digit from position i to end
        var maxSuffix = [];
        arrayResize(maxSuffix, n);
        maxSuffix[n] = val(mid(line, n, 1));

        for (var i = n - 1; i >= 1; i--) {
            var currentDigit = val(mid(line, i, 1));
            maxSuffix[i] = max(currentDigit, maxSuffix[i + 1]);
        }

        var maxJoltage = 0;

        // For each possible first battery position
        for (var i = 1; i <= n - 1; i++) {
            var firstDigit = val(mid(line, i, 1));
            // The maximum second digit is the max from position i+1 onwards
            var maxSecond = maxSuffix[i + 1];
            var joltage = firstDigit * 10 + maxSecond;
            maxJoltage = max(maxJoltage, joltage);
        }

        total += maxJoltage;
    }

    return total;
}

function part2() {
    var total = precisionEvaluate(0);

    for (var line in lines) {
        var n = len(line);
        var k = 12; // Select exactly 12 batteries

        // Greedy algorithm to select k digits that form the maximum number
        var result = [];
        var currentPos = 1; // ColdFusion is 1-indexed

        for (var i = 1; i <= k; i++) {
            // How many digits we still need to select after this one
            var remainingNeeded = k - i;
            // Latest position we can start searching from
            var searchEnd = n - remainingNeeded;

            // Find the maximum digit in the valid range
            var maxDigit = -1;
            var maxPos = currentPos;

            for (var j = currentPos; j <= searchEnd; j++) {
                var digit = val(mid(line, j, 1));
                if (digit > maxDigit) {
                    maxDigit = digit;
                    maxPos = j;
                }
            }

            arrayAppend(result, maxDigit);
            currentPos = maxPos + 1;
        }

        // Convert array to string and then to number using precisionEvaluate
        var joltageStr = result.toList("");
        var joltage = precisionEvaluate(joltageStr);
        total = precisionEvaluate(total + joltage);
    }

    return total;
}

// Output results
writeOutput("Part 1: " & part1() & chr(10));
writeOutput("Part 2: " & part2() & chr(10));
</cfscript>
