<cfscript>
// Parse input file into grid
function parseInput(filename) {
    var inputFile = fileRead(filename);
    var lines = listToArray(inputFile, chr(10));
    var grid = [];

    for (var line in lines) {
        line = trim(line);
        if (len(line) > 0) {
            arrayAppend(grid, listToArray(line, ""));
        }
    }

    return grid;
}

// Find all connected regions using BFS
function findRegions(grid) {
    var rows = arrayLen(grid);
    var cols = arrayLen(grid[1]);
    var visited = {};
    var regions = [];

    for (var r = 1; r <= rows; r++) {
        for (var c = 1; c <= cols; c++) {
            var key = r & "," & c;
            if (structKeyExists(visited, key)) {
                continue;
            }

            // BFS to find all cells in this region
            var plant = grid[r][c];
            var region = {};
            var queue = [];
            arrayAppend(queue, {"r": r, "c": c});

            while (arrayLen(queue) > 0) {
                var pos = queue[1];
                arrayDeleteAt(queue, 1);
                var cr = pos.r;
                var cc = pos.c;
                var cellKey = cr & "," & cc;

                if (structKeyExists(visited, cellKey)) {
                    continue;
                }
                if (cr < 1 || cr > rows || cc < 1 || cc > cols) {
                    continue;
                }
                if (grid[cr][cc] != plant) {
                    continue;
                }

                visited[cellKey] = true;
                region[cellKey] = true;

                // Add neighbors
                var directions = [
                    {"dr": 0, "dc": 1},
                    {"dr": 0, "dc": -1},
                    {"dr": 1, "dc": 0},
                    {"dr": -1, "dc": 0}
                ];

                for (var dir in directions) {
                    var nr = cr + dir.dr;
                    var nc = cc + dir.dc;
                    var neighborKey = nr & "," & nc;
                    if (!structKeyExists(visited, neighborKey)) {
                        arrayAppend(queue, {"r": nr, "c": nc});
                    }
                }
            }

            arrayAppend(regions, region);
        }
    }

    return regions;
}

// Calculate perimeter of a region
function calculatePerimeter(region) {
    var perimeter = 0;
    var directions = [
        {"dr": 0, "dc": 1},
        {"dr": 0, "dc": -1},
        {"dr": 1, "dc": 0},
        {"dr": -1, "dc": 0}
    ];

    for (var cellKey in region) {
        var parts = listToArray(cellKey, ",");
        var r = val(parts[1]);
        var c = val(parts[2]);

        for (var dir in directions) {
            var nr = r + dir.dr;
            var nc = c + dir.dc;
            var neighborKey = nr & "," & nc;
            if (!structKeyExists(region, neighborKey)) {
                perimeter++;
            }
        }
    }

    return perimeter;
}

// Count number of sides (corners) in a region
function countSides(region) {
    var corners = 0;

    for (var cellKey in region) {
        var parts = listToArray(cellKey, ",");
        var r = val(parts[1]);
        var c = val(parts[2]);

        // Check all 8 neighbors
        var up = structKeyExists(region, (r - 1) & "," & c);
        var down = structKeyExists(region, (r + 1) & "," & c);
        var left = structKeyExists(region, r & "," & (c - 1));
        var right = structKeyExists(region, r & "," & (c + 1));
        var upLeft = structKeyExists(region, (r - 1) & "," & (c - 1));
        var upRight = structKeyExists(region, (r - 1) & "," & (c + 1));
        var downLeft = structKeyExists(region, (r + 1) & "," & (c - 1));
        var downRight = structKeyExists(region, (r + 1) & "," & (c + 1));

        // Top-left corner: convex (both out) or concave (both in, diagonal out)
        if (!up && !left) {
            corners++;
        } else if (up && left && !upLeft) {
            corners++;
        }

        // Top-right corner
        if (!up && !right) {
            corners++;
        } else if (up && right && !upRight) {
            corners++;
        }

        // Bottom-left corner
        if (!down && !left) {
            corners++;
        } else if (down && left && !downLeft) {
            corners++;
        }

        // Bottom-right corner
        if (!down && !right) {
            corners++;
        } else if (down && right && !downRight) {
            corners++;
        }
    }

    return corners;
}

// Part 1: Calculate total fencing cost using area * perimeter
function part1(grid) {
    var regions = findRegions(grid);
    var total = 0;

    for (var region in regions) {
        var area = structCount(region);
        var perimeter = calculatePerimeter(region);
        total += area * perimeter;
    }

    return total;
}

// Part 2: Calculate total fencing cost using area * sides
function part2(grid) {
    var regions = findRegions(grid);
    var total = 0;

    for (var region in regions) {
        var area = structCount(region);
        var sides = countSides(region);
        total += area * sides;
    }

    return total;
}

// Main execution
var scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
var inputPath = scriptDir & "../input.txt";
var grid = parseInput(inputPath);

writeOutput("Part 1: " & part1(grid) & chr(10));
writeOutput("Part 2: " & part2(grid) & chr(10));
</cfscript>
