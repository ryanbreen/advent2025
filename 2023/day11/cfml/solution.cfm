<cfscript>
/**
 * Advent of Code 2023 - Day 11: Cosmic Expansion
 * ColdFusion/CFScript Solution
 *
 * Note: In CFML, "##" is the escape sequence for a literal "#" character.
 * The galaxy character in the input is "#", so we use "##" in string comparisons.
 */

// Read input file - get directory of this script
scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
inputPath = scriptDir & "../input.txt";
fileContent = fileRead(inputPath);
lines = listToArray(fileContent, chr(10));

// Remove trailing empty lines and carriage returns
while (arrayLen(lines) > 0 && trim(lines[arrayLen(lines)]) == "") {
    arrayDeleteAt(lines, arrayLen(lines));
}
// Clean up any carriage returns
for (i = 1; i <= arrayLen(lines); i++) {
    lines[i] = replace(lines[i], chr(13), "", "all");
}

/**
 * Parse the grid and find all galaxy positions.
 * Galaxies are marked with "#" (written as "##" in CFML to escape the special character).
 */
function parseGrid(lines) {
    var galaxies = [];
    for (var r = 1; r <= arrayLen(lines); r++) {
        var line = lines[r];
        for (var c = 1; c <= len(line); c++) {
            // "##" is CFML escape for literal "#" (galaxy marker)
            if (mid(line, c, 1) == "##") {
                arrayAppend(galaxies, {row: r, col: c});
            }
        }
    }
    return galaxies;
}

/**
 * Find rows and columns that contain no galaxies.
 * Returns a struct with "rows" and "cols" keys, each containing a struct
 * where keys are the indices of empty rows/columns.
 */
function findEmptyRowsAndCols(lines) {
    var rows = arrayLen(lines);
    var cols = rows > 0 ? len(lines[1]) : 0;

    var emptyRows = {};
    var emptyCols = {};

    // Find empty rows (no "#" galaxy character in the line)
    for (var r = 1; r <= rows; r++) {
        // "##" is CFML escape for literal "#"
        if (find("##", lines[r]) == 0) {
            emptyRows[r] = true;
        }
    }

    // Find empty columns (no galaxy in any row at this column)
    for (var c = 1; c <= cols; c++) {
        var hasGalaxy = false;
        for (var r = 1; r <= rows; r++) {
            // "##" is CFML escape for literal "#"
            if (mid(lines[r], c, 1) == "##") {
                hasGalaxy = true;
                break;
            }
        }
        if (!hasGalaxy) {
            emptyCols[c] = true;
        }
    }

    return {rows: emptyRows, cols: emptyCols};
}

/**
 * Calculate sum of Manhattan distances between all pairs of galaxies.
 * Empty rows/columns are expanded by the expansion factor.
 */
function calculateDistances(galaxies, emptyRows, emptyCols, expansionFactor) {
    var total = 0;
    var numGalaxies = arrayLen(galaxies);

    // Iterate through all unique pairs
    for (var i = 1; i <= numGalaxies - 1; i++) {
        for (var j = i + 1; j <= numGalaxies; j++) {
            var g1 = galaxies[i];
            var g2 = galaxies[j];

            var r1 = g1.row;
            var c1 = g1.col;
            var r2 = g2.row;
            var c2 = g2.col;

            // Calculate row distance with expansion
            var minR = min(r1, r2);
            var maxR = max(r1, r2);
            var rowDist = maxR - minR;
            for (var r = minR; r < maxR; r++) {
                if (emptyRows.keyExists(r)) {
                    rowDist += expansionFactor - 1;
                }
            }

            // Calculate column distance with expansion
            var minC = min(c1, c2);
            var maxC = max(c1, c2);
            var colDist = maxC - minC;
            for (var c = minC; c < maxC; c++) {
                if (emptyCols.keyExists(c)) {
                    colDist += expansionFactor - 1;
                }
            }

            total += rowDist + colDist;
        }
    }

    return total;
}

/**
 * Part 1: Expansion factor of 2 (each empty row/col becomes 2)
 */
function part1(galaxies, emptyRows, emptyCols) {
    return calculateDistances(galaxies, emptyRows, emptyCols, 2);
}

/**
 * Part 2: Expansion factor of 1,000,000 (each empty row/col becomes 1 million)
 */
function part2(galaxies, emptyRows, emptyCols) {
    return calculateDistances(galaxies, emptyRows, emptyCols, 1000000);
}

// Parse grid and find empty rows/cols once (avoid redundant computation)
galaxies = parseGrid(lines);
empty = findEmptyRowsAndCols(lines);

// Run the solution
writeOutput("Part 1: " & part1(galaxies, empty.rows, empty.cols) & chr(10));
writeOutput("Part 2: " & part2(galaxies, empty.rows, empty.cols) & chr(10));
</cfscript>
