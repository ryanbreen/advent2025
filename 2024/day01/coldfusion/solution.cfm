<cfscript>
// Parse input file into two arrays
function parseInput(filename) {
    var inputFile = fileRead(filename);
    var lines = listToArray(inputFile, chr(10));
    var leftList = [];
    var rightList = [];

    for (var line in lines) {
        line = trim(line);
        if (len(line) > 0) {
            // Split on whitespace and filter empty strings
            var parts = reMatch("[0-9]+", line);
            if (arrayLen(parts) >= 2) {
                arrayAppend(leftList, val(parts[1]));
                arrayAppend(rightList, val(parts[2]));
            }
        }
    }

    return {"left": leftList, "right": rightList};
}

// Part 1: Sort both lists and calculate total distance
function part1(leftList, rightList) {
    var sortedLeft = duplicate(leftList);
    var sortedRight = duplicate(rightList);

    arraySort(sortedLeft, "numeric");
    arraySort(sortedRight, "numeric");

    var totalDistance = 0;
    for (var i = 1; i <= arrayLen(sortedLeft); i++) {
        totalDistance += abs(sortedLeft[i] - sortedRight[i]);
    }

    return totalDistance;
}

// Part 2: Calculate similarity score
function part2(leftList, rightList) {
    var similarityScore = 0;

    for (var num in leftList) {
        var count = 0;
        for (var rightNum in rightList) {
            if (rightNum == num) {
                count++;
            }
        }
        similarityScore += num * count;
    }

    return similarityScore;
}

// Main execution
var scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
var inputPath = scriptDir & "../input.txt";
var data = parseInput(inputPath);

writeOutput("Part 1: " & part1(data.left, data.right) & chr(10));
writeOutput("Part 2: " & part2(data.left, data.right) & chr(10));
</cfscript>
