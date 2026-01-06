<cfscript>
// Day 15: Lens Library - CFML Solution

function hashAlgorithm(required string s) {
    var current = 0;
    for (var i = 1; i <= len(s); i++) {
        var c = mid(s, i, 1);
        current = ((current + asc(c)) * 17) mod 256;
    }
    return current;
}

function part1(required array steps) {
    var total = 0;
    for (var step in steps) {
        total += hashAlgorithm(step);
    }
    return total;
}

function part2(required array steps) {
    // Create 256 boxes, each holding an array of lenses (label, focal)
    var boxes = [];
    for (var i = 1; i <= 256; i++) {
        arrayAppend(boxes, []);
    }

    for (var step in steps) {
        if (find("=", step) > 0) {
            // Operation: add or replace lens
            var parts = step.split("=");
            var label = parts[1];
            var focal = val(parts[2]);
            var boxNum = hashAlgorithm(label) + 1;  // CFML arrays are 1-indexed

            // Check if lens with this label already exists
            var found = false;
            for (var i = 1; i <= arrayLen(boxes[boxNum]); i++) {
                if (boxes[boxNum][i].label == label) {
                    boxes[boxNum][i].focal = focal;
                    found = true;
                    break;
                }
            }

            // If not found, add new lens
            if (!found) {
                arrayAppend(boxes[boxNum], {label: label, focal: focal});
            }
        } else {
            // Operation: remove lens (ends with '-')
            var label = left(step, len(step) - 1);
            var boxNum = hashAlgorithm(label) + 1;  // CFML arrays are 1-indexed

            // Remove lens with this label if it exists
            var newBox = [];
            for (var lens in boxes[boxNum]) {
                if (lens.label != label) {
                    arrayAppend(newBox, lens);
                }
            }
            boxes[boxNum] = newBox;
        }
    }

    // Calculate focusing power
    var total = 0;
    for (var boxNum = 1; boxNum <= 256; boxNum++) {
        for (var slot = 1; slot <= arrayLen(boxes[boxNum]); slot++) {
            var lens = boxes[boxNum][slot];
            total += boxNum * slot * lens.focal;
        }
    }

    return total;
}

// Main execution
var scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
var inputPath = scriptDir & "../input.txt";
var inputText = fileRead(inputPath).trim().replace(chr(10), "").replace(chr(13), "");
var steps = inputText.split(",");

writeOutput("Part 1: " & part1(steps) & chr(10));
writeOutput("Part 2: " & part2(steps) & chr(10));
</cfscript>
