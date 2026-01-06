<cfscript>
// Day 13: Point of Incidence - CFML Solution

function parseInput(required string text) {
    var blocks = text.trim().split("\r?\n\r?\n");
    var patterns = [];

    for (var block in blocks) {
        var lines = block.split("\r?\n");
        arrayAppend(patterns, lines);
    }

    return patterns;
}

function findVerticalReflection(required array pattern) {
    if (arrayLen(pattern) == 0) {
        return 0;
    }

    var width = len(pattern[1]);

    for (var col = 1; col < width; col++) {
        var isReflection = true;

        for (var row in pattern) {
            var leftPart = reverse(left(row, col));
            var rightPart = mid(row, col + 1, len(row));
            var minLen = min(len(leftPart), len(rightPart));

            if (left(leftPart, minLen) != left(rightPart, minLen)) {
                isReflection = false;
                break;
            }
        }

        if (isReflection) {
            return col;
        }
    }

    return 0;
}

function findHorizontalReflection(required array pattern) {
    if (arrayLen(pattern) == 0) {
        return 0;
    }

    var height = arrayLen(pattern);

    for (var row = 1; row < height; row++) {
        var isReflection = true;
        var topPart = arraySlice(pattern, 1, row);
        var bottomPart = arraySlice(pattern, row + 1, height - row);

        topPart = arrayReverse(topPart);

        var minLen = min(arrayLen(topPart), arrayLen(bottomPart));

        for (var i = 1; i <= minLen; i++) {
            if (topPart[i] != bottomPart[i]) {
                isReflection = false;
                break;
            }
        }

        if (isReflection) {
            return row;
        }
    }

    return 0;
}

function summarizePattern(required array pattern) {
    var v = findVerticalReflection(pattern);
    if (v > 0) {
        return v;
    }
    var h = findHorizontalReflection(pattern);
    return h * 100;
}

function part1(required array patterns) {
    var total = 0;
    for (var pattern in patterns) {
        total += summarizePattern(pattern);
    }
    return total;
}

function countDifferences(required string s1, required string s2) {
    var count = 0;
    var minLen = min(len(s1), len(s2));

    for (var i = 1; i <= minLen; i++) {
        if (mid(s1, i, 1) != mid(s2, i, 1)) {
            count++;
        }
    }

    return count;
}

function findVerticalReflectionWithSmudge(required array pattern) {
    if (arrayLen(pattern) == 0) {
        return 0;
    }

    var width = len(pattern[1]);

    for (var col = 1; col < width; col++) {
        var totalDiff = 0;

        for (var row in pattern) {
            var leftPart = reverse(left(row, col));
            var rightPart = mid(row, col + 1, len(row));
            var minLen = min(len(leftPart), len(rightPart));

            totalDiff += countDifferences(left(leftPart, minLen), left(rightPart, minLen));

            if (totalDiff > 1) {
                break;
            }
        }

        if (totalDiff == 1) {
            return col;
        }
    }

    return 0;
}

function findHorizontalReflectionWithSmudge(required array pattern) {
    if (arrayLen(pattern) == 0) {
        return 0;
    }

    var height = arrayLen(pattern);

    for (var row = 1; row < height; row++) {
        var totalDiff = 0;
        var topPart = arraySlice(pattern, 1, row);
        var bottomPart = arraySlice(pattern, row + 1, height - row);

        topPart = arrayReverse(topPart);

        var minLen = min(arrayLen(topPart), arrayLen(bottomPart));

        for (var i = 1; i <= minLen; i++) {
            totalDiff += countDifferences(topPart[i], bottomPart[i]);

            if (totalDiff > 1) {
                break;
            }
        }

        if (totalDiff == 1) {
            return row;
        }
    }

    return 0;
}

function summarizePatternWithSmudge(required array pattern) {
    var v = findVerticalReflectionWithSmudge(pattern);
    if (v > 0) {
        return v;
    }
    var h = findHorizontalReflectionWithSmudge(pattern);
    return h * 100;
}

function part2(required array patterns) {
    var total = 0;
    for (var pattern in patterns) {
        total += summarizePatternWithSmudge(pattern);
    }
    return total;
}

// Main execution
var scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
var inputPath = scriptDir & "../input.txt";
var inputText = fileRead(inputPath);
var patterns = parseInput(inputText);

writeOutput("Part 1: " & part1(patterns) & chr(10));
writeOutput("Part 2: " & part2(patterns) & chr(10));
</cfscript>
