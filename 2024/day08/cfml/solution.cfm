<cfscript>
function parseInput(required string filename) struct {
    var fileContent = fileRead(filename);
    var lines = listToArray(fileContent, chr(10));

    // Remove any trailing empty lines
    while (arrayLen(lines) > 0 && trim(lines[arrayLen(lines)]) == "") {
        arrayDeleteAt(lines, arrayLen(lines));
    }

    var rows = arrayLen(lines);
    var cols = rows > 0 ? len(lines[1]) : 0;

    // Group antenna positions by frequency
    // IMPORTANT: Use an array to store antennas grouped by frequency
    // Cannot use a struct because CFML structs are case-insensitive by default,
    // but 'A' and 'a' are different frequencies in this problem
    var antennas = [];

    for (var r = 1; r <= rows; r++) {
        var row = lines[r];
        for (var c = 1; c <= len(row); c++) {
            var ch = mid(row, c, 1);
            if (ch != ".") {
                // Find existing group for this frequency (case-sensitive compare)
                var found = false;
                for (var i = 1; i <= arrayLen(antennas); i++) {
                    if (compare(antennas[i].freq, ch) == 0) {
                        arrayAppend(antennas[i].positions, {r: r-1, c: c-1});
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    arrayAppend(antennas, {freq: ch, positions: [{r: r-1, c: c-1}]});
                }
            }
        }
    }

    return {rows: rows, cols: cols, antennas: antennas};
}

function coordKey(required numeric r, required numeric c) string {
    return r & "," & c;
}

function part1(required struct data) numeric {
    var rows = data.rows;
    var cols = data.cols;
    var antennas = data.antennas;

    var antinodes = {};

    // For each frequency group
    for (var g = 1; g <= arrayLen(antennas); g++) {
        var positions = antennas[g].positions;
        var numPos = arrayLen(positions);

        // For each pair of antennas with same frequency
        for (var i = 1; i <= numPos; i++) {
            for (var j = i + 1; j <= numPos; j++) {
                var pos1 = positions[i];
                var pos2 = positions[j];

                var r1 = pos1.r;
                var c1 = pos1.c;
                var r2 = pos2.r;
                var c2 = pos2.c;

                // Calculate the two antinodes
                // Antinode beyond antenna 1 (away from antenna 2)
                var ar1 = 2 * r1 - r2;
                var ac1 = 2 * c1 - c2;

                // Antinode beyond antenna 2 (away from antenna 1)
                var ar2 = 2 * r2 - r1;
                var ac2 = 2 * c2 - c1;

                // Add if within bounds
                if (ar1 >= 0 && ar1 < rows && ac1 >= 0 && ac1 < cols) {
                    var key1 = coordKey(ar1, ac1);
                    antinodes[key1] = true;
                }
                if (ar2 >= 0 && ar2 < rows && ac2 >= 0 && ac2 < cols) {
                    var key2 = coordKey(ar2, ac2);
                    antinodes[key2] = true;
                }
            }
        }
    }

    return structCount(antinodes);
}

function part2(required struct data) numeric {
    var rows = data.rows;
    var cols = data.cols;
    var antennas = data.antennas;

    var antinodes = {};

    // For each frequency group
    for (var g = 1; g <= arrayLen(antennas); g++) {
        var positions = antennas[g].positions;
        var numPos = arrayLen(positions);

        // For each pair of antennas with same frequency
        for (var i = 1; i <= numPos; i++) {
            for (var j = i + 1; j <= numPos; j++) {
                var pos1 = positions[i];
                var pos2 = positions[j];

                var r1 = pos1.r;
                var c1 = pos1.c;
                var r2 = pos2.r;
                var c2 = pos2.c;

                var dr = r2 - r1;
                var dc = c2 - c1;

                // Extend in both directions along the line
                // Direction 1: from antenna 1 towards and beyond antenna 2
                var r = r1;
                var c = c1;
                while (r >= 0 && r < rows && c >= 0 && c < cols) {
                    var key = coordKey(r, c);
                    antinodes[key] = true;
                    r += dr;
                    c += dc;
                }

                // Direction 2: from antenna 1 away from antenna 2
                r = r1 - dr;
                c = c1 - dc;
                while (r >= 0 && r < rows && c >= 0 && c < cols) {
                    var key = coordKey(r, c);
                    antinodes[key] = true;
                    r -= dr;
                    c -= dc;
                }
            }
        }
    }

    return structCount(antinodes);
}

// Main execution
var data = parseInput("../input.txt");
writeOutput("Part 1: " & part1(data));
writeOutput(chr(10));
writeOutput("Part 2: " & part2(data));
writeOutput(chr(10));
</cfscript>
