<cfscript>
// Parse input file into array of reports (each report is an array of numbers)
function parseInput(filename) {
    var inputFile = fileRead(filename);
    var lines = listToArray(inputFile, chr(10));
    var reports = [];

    for (var line in lines) {
        line = trim(line);
        if (len(line) > 0) {
            // Split on whitespace and convert to numbers
            var parts = reMatch("[0-9]+", line);
            var levels = [];
            for (var part in parts) {
                arrayAppend(levels, val(part));
            }
            arrayAppend(reports, levels);
        }
    }

    return reports;
}

// Check if all adjacent pairs are increasing by 1-3
function allIncreasing(levels) {
    for (var i = 1; i <= arrayLen(levels) - 1; i++) {
        var diff = levels[i + 1] - levels[i];
        if (diff < 1 || diff > 3) {
            return false;
        }
    }
    return true;
}

// Check if all adjacent pairs are decreasing by 1-3
function allDecreasing(levels) {
    for (var i = 1; i <= arrayLen(levels) - 1; i++) {
        var diff = levels[i] - levels[i + 1];
        if (diff < 1 || diff > 3) {
            return false;
        }
    }
    return true;
}

// Check if a report is safe
function isSafe(levels) {
    return allIncreasing(levels) || allDecreasing(levels);
}

// Remove element at given index (1-based indexing)
function removeAt(levels, index) {
    var result = [];
    for (var i = 1; i <= arrayLen(levels); i++) {
        if (i != index) {
            arrayAppend(result, levels[i]);
        }
    }
    return result;
}

// Check if a report is safe or can be made safe by removing one level
function isSafeWithDampener(levels) {
    if (isSafe(levels)) {
        return true;
    }

    // Try removing each level one at a time
    for (var i = 1; i <= arrayLen(levels); i++) {
        var modified = removeAt(levels, i);
        if (isSafe(modified)) {
            return true;
        }
    }

    return false;
}

// Part 1: Count safe reports
function part1(reports) {
    var count = 0;
    for (var report in reports) {
        if (isSafe(report)) {
            count++;
        }
    }
    return count;
}

// Part 2: Count reports that are safe or can be made safe with Problem Dampener
function part2(reports) {
    var count = 0;
    for (var report in reports) {
        if (isSafeWithDampener(report)) {
            count++;
        }
    }
    return count;
}

// Main execution
var scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
var inputPath = scriptDir & "../input.txt";
var reports = parseInput(inputPath);

writeOutput("Part 1: " & part1(reports) & chr(10));
writeOutput("Part 2: " & part2(reports) & chr(10));
</cfscript>
