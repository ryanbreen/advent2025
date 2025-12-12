<cfscript>
// Read input file
var scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
var inputPath = scriptDir & "../input.txt";
var inputText = fileRead(inputPath).trim();

function parseInput(text) {
    // Split by double newlines using replace + listToArray
    var marker = "<<<SECTION_DELIMITER>>>";
    var markedText = text.replace(chr(10) & chr(10), marker, "all");
    var sections = listToArray(markedText, marker);

    var shapes = {};
    var regions = [];

    for (var section in sections) {
        var lines = listToArray(section.trim(), chr(10));

        if (arrayLen(lines) > 0) {
            var firstLine = lines[1];

            // Check if it's a shape definition (has : but not x)
            if (firstLine.find(":") > 0 && !firstLine.find("x")) {
                // Shape definition
                var idx = val(firstLine.replace(":", "", "all"));
                var cellCount = 0;

                // Count # characters in shape lines (skip first line with index)
                for (var i = 2; i <= arrayLen(lines); i++) {
                    var line = lines[i];
                    // Count occurrences of # by measuring length difference after removal
                    var originalLen = line.len();
                    var withoutHash = line.replace(chr(35), "", "all");
                    cellCount += originalLen - withoutHash.len();
                }

                shapes[idx] = cellCount;
            } else {
                // Region definitions
                for (var line in lines) {
                    if (line.find("x") > 0) {
                        var parts = listToArray(line, ":");
                        var dims = parts[1].trim();
                        var dimParts = listToArray(dims, "x");
                        var width = val(dimParts[1]);
                        var height = val(dimParts[2]);

                        var countsStr = parts[2].trim();
                        var counts = listToArray(countsStr, " ");

                        regions.append({
                            width: width,
                            height: height,
                            counts: counts
                        });
                    }
                }
            }
        }
    }

    return {shapes: shapes, regions: regions};
}

function canFitRegion(width, height, counts, shapes) {
    var totalCellsNeeded = 0;

    for (var i = 1; i <= arrayLen(counts); i++) {
        var count = val(counts[i]);
        var shapeIdx = i - 1; // Shape indices start at 0

        if (shapes.keyExists(shapeIdx)) {
            totalCellsNeeded += count * shapes[shapeIdx];
        }
    }

    var available = width * height;
    return totalCellsNeeded <= available;
}

function part1(shapes, regions) {
    var count = 0;

    for (var region in regions) {
        if (canFitRegion(region.width, region.height, region.counts, shapes)) {
            count++;
        }
    }

    return count;
}

function part2() {
    // Part 2 is just a button click - no computation needed
    return 0;
}

// Parse input
var parsed = parseInput(inputText);
var shapes = parsed.shapes;
var regions = parsed.regions;

// Run both parts
writeOutput("Part 1: " & part1(shapes, regions) & chr(10));
writeOutput("Part 2: " & part2() & chr(10));
</cfscript>
