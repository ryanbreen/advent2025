<cfscript>
/**
 * Day 21: Step Counter
 * BFS to count garden plots reachable in exactly N steps.
 * Part 2 uses quadratic interpolation for the infinite tiled grid.
 */

// Read input file
scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
inputPath = scriptDir & "../input.txt";
inputContent = fileRead(inputPath);
grid = listToArray(inputContent, chr(10));

// Remove empty lines
cleanGrid = [];
for (line in grid) {
    if (len(trim(line)) > 0) {
        arrayAppend(cleanGrid, line);
    }
}
grid = cleanGrid;

rows = arrayLen(grid);
cols = len(grid[1]);

/**
 * Find starting position (S)
 */
function findStart(grid) {
    for (var r = 1; r <= arrayLen(grid); r++) {
        var idx = find("S", grid[r]);
        if (idx > 0) {
            return {"r": r, "c": idx};
        }
    }
    return {"r": 0, "c": 0};
}

/**
 * Count cells reachable in exactly 'steps' steps (Part 1 - bounded grid)
 */
function countReachable(grid, startR, startC, steps) {
    var rows = arrayLen(grid);
    var cols = len(grid[1]);

    // BFS to find minimum steps to each cell
    // Using struct with "r,c" as key
    var visited = {};
    var queue = [];
    var queueIdx = 1;

    var startKey = startR & "," & startC;
    visited[startKey] = 0;
    arrayAppend(queue, {"r": startR, "c": startC, "dist": 0});

    var directions = [
        {"dr": -1, "dc": 0},
        {"dr": 1, "dc": 0},
        {"dr": 0, "dc": -1},
        {"dr": 0, "dc": 1}
    ];

    while (queueIdx <= arrayLen(queue)) {
        var current = queue[queueIdx];
        queueIdx++;

        var r = current.r;
        var c = current.c;
        var dist = current.dist;

        if (dist >= steps) {
            continue;
        }

        for (var dir in directions) {
            var nr = r + dir.dr;
            var nc = c + dir.dc;

            if (nr >= 1 && nr <= rows && nc >= 1 && nc <= cols) {
                var ch = mid(grid[nr], nc, 1);
                if (ch != chr(35)) {
                    var key = nr & "," & nc;
                    if (!structKeyExists(visited, key)) {
                        visited[key] = dist + 1;
                        arrayAppend(queue, {"r": nr, "c": nc, "dist": dist + 1});
                    }
                }
            }
        }
    }

    // Count cells reachable in exactly 'steps' steps
    // A cell reachable in d steps can be reached in d+2, d+4, ... steps
    var targetParity = steps mod 2;
    var count = 0;

    for (var key in visited) {
        var d = visited[key];
        if (d <= steps && (d mod 2) == targetParity) {
            count++;
        }
    }

    return count;
}

/**
 * Proper modulo that handles negative numbers (CFML mod can return negative)
 */
function properMod(n, m) {
    var result = n mod m;
    if (result < 0) {
        result = result + m;
    }
    return result;
}

/**
 * Count cells reachable on infinite tiled grid using BFS
 */
function countReachableInfiniteBFS(grid, startR, startC, steps) {
    var rows = arrayLen(grid);
    var cols = len(grid[1]);

    // BFS tracking virtual coordinates
    var visited = {};
    var queue = [];
    var queueIdx = 1;

    var startKey = startR & "," & startC;
    visited[startKey] = 0;
    arrayAppend(queue, {"r": startR, "c": startC, "dist": 0});

    var directions = [
        {"dr": -1, "dc": 0},
        {"dr": 1, "dc": 0},
        {"dr": 0, "dc": -1},
        {"dr": 0, "dc": 1}
    ];

    while (queueIdx <= arrayLen(queue)) {
        var current = queue[queueIdx];
        queueIdx++;

        var r = current.r;
        var c = current.c;
        var dist = current.dist;

        if (dist >= steps) {
            continue;
        }

        for (var dir in directions) {
            var nr = r + dir.dr;
            var nc = c + dir.dc;

            // Map to grid coordinates (infinite tiling)
            // CFML arrays are 1-indexed, so we need to handle this carefully
            var gr = properMod(nr - 1, rows) + 1;
            var gc = properMod(nc - 1, cols) + 1;

            var ch = mid(grid[gr], gc, 1);
            if (ch != chr(35)) {
                var key = nr & "," & nc;
                if (!structKeyExists(visited, key)) {
                    visited[key] = dist + 1;
                    arrayAppend(queue, {"r": nr, "c": nc, "dist": dist + 1});
                }
            }
        }
    }

    // Count cells reachable in exactly 'steps' steps
    var targetParity = steps mod 2;
    var count = 0;

    for (var key in visited) {
        var d = visited[key];
        if (d <= steps && (d mod 2) == targetParity) {
            count++;
        }
    }

    return count;
}

/**
 * Part 1: Count plots reachable in exactly 64 steps
 */
function part1(grid) {
    var start = findStart(grid);
    return countReachable(grid, start.r, start.c, 64);
}

/**
 * Part 2: Count plots reachable in exactly 26501365 steps on infinite grid
 * Uses quadratic interpolation based on the pattern:
 * - Grid is 131x131 with S at center (65,65 in 0-indexed, 66,66 in 1-indexed)
 * - 26501365 = 65 + 202300 * 131
 * - The count follows f(n) = an^2 + bn + c
 */
function part2(grid) {
    var start = findStart(grid);
    var rows = arrayLen(grid);
    var size = rows;

    // Steps to reach edge from center (half of grid size)
    var half = int(size / 2);  // 65

    // Calculate reachable counts for n=0, 1, 2
    var y0 = countReachableInfiniteBFS(grid, start.r, start.c, half);
    var y1 = countReachableInfiniteBFS(grid, start.r, start.c, half + size);
    var y2 = countReachableInfiniteBFS(grid, start.r, start.c, half + 2 * size);

    // Solve for a, b, c using finite differences
    // a = (y2 - 2*y1 + y0) / 2
    // b = y1 - y0 - a
    // c = y0
    var a = int((y2 - 2 * y1 + y0) / 2);
    var b = y1 - y0 - a;
    var c = y0;

    // n = (26501365 - 65) / 131 = 202300
    var n = 202300;

    // Use Java BigInteger for large number arithmetic
    var BigInteger = createObject("java", "java.math.BigInteger");

    var bigN = BigInteger.valueOf(javacast("long", n));
    var bigA = BigInteger.valueOf(javacast("long", a));
    var bigB = BigInteger.valueOf(javacast("long", b));
    var bigC = BigInteger.valueOf(javacast("long", c));

    // result = a*n^2 + b*n + c
    var nSquared = bigN.multiply(bigN);
    var term1 = bigA.multiply(nSquared);
    var term2 = bigB.multiply(bigN);
    var result = term1.add(term2).add(bigC);

    return result.toString();
}

// Main execution
writeOutput("Part 1: " & part1(grid) & chr(10));
writeOutput("Part 2: " & part2(grid) & chr(10));
</cfscript>
