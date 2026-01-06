<cfscript>
// Parse input into grid of integers
function parseInput(filename) {
    var inputText = fileRead(filename);
    var lines = listToArray(inputText.trim(), chr(10), false, true);
    var grid = [];

    for (var line in lines) {
        var row = [];
        for (var i = 1; i <= len(line); i++) {
            arrayAppend(row, val(mid(line, i, 1)));
        }
        arrayAppend(grid, row);
    }

    return grid;
}

// Dijkstra's algorithm with movement constraints
// State: (row, col, direction, consecutive_steps)
// Directions: 0=right, 1=down, 2=left, 3=up
function dijkstra(grid, minStraight, maxStraight) {
    var rows = arrayLen(grid);
    var cols = arrayLen(grid[1]);

    // Direction deltas
    var dr = [0, 1, 0, -1];
    var dc = [1, 0, -1, 0];

    // Priority queue as array of structs: {heat, r, c, d, consec}
    // We'll manually sort after each insert
    var pq = [];
    arrayAppend(pq, {heat: 0, r: 1, c: 1, d: -1, consec: 0});

    // Visited states
    var visited = {};

    while (arrayLen(pq) > 0) {
        // Get minimum heat state
        var current = pq[1];
        arrayDeleteAt(pq, 1);

        var heat = current.heat;
        var r = current.r;
        var c = current.c;
        var d = current.d;
        var consec = current.consec;

        // Check if we reached the goal
        if (r == rows && c == cols) {
            if (minStraight == 0 || consec >= minStraight) {
                return heat;
            }
        }

        // Check if state already visited
        var stateKey = r & "," & c & "," & d & "," & consec;
        if (structKeyExists(visited, stateKey)) {
            continue;
        }
        visited[stateKey] = true;

        // Try all four directions
        for (var nd = 0; nd <= 3; nd++) {
            // Can't reverse direction
            if (d != -1 && nd == ((d + 2) mod 4)) {
                continue;
            }

            var nr = r + dr[nd + 1];
            var nc = c + dc[nd + 1];

            // Bounds check
            if (nr < 1 || nr > rows || nc < 1 || nc > cols) {
                continue;
            }

            // Check consecutive constraints
            var newConsec = 0;
            if (nd == d) {
                // Continuing in same direction
                newConsec = consec + 1;
                if (newConsec > maxStraight) {
                    continue;
                }
            } else {
                // Turning - must have gone minStraight first
                if (d != -1 && consec < minStraight) {
                    continue;
                }
                newConsec = 1;
            }

            var newHeat = heat + grid[nr][nc];
            var newStateKey = nr & "," & nc & "," & nd & "," & newConsec;

            if (!structKeyExists(visited, newStateKey)) {
                // Insert into priority queue maintaining sorted order
                var inserted = false;
                for (var i = 1; i <= arrayLen(pq); i++) {
                    if (newHeat < pq[i].heat) {
                        arrayInsertAt(pq, i, {heat: newHeat, r: nr, c: nc, d: nd, consec: newConsec});
                        inserted = true;
                        break;
                    }
                }
                if (!inserted) {
                    arrayAppend(pq, {heat: newHeat, r: nr, c: nc, d: nd, consec: newConsec});
                }
            }
        }
    }

    return -1; // No path found
}

// Part 1: Normal crucible, max 3 consecutive blocks
function part1(grid) {
    return dijkstra(grid, 0, 3);
}

// Part 2: Ultra crucible, min 4 and max 10 consecutive blocks
function part2(grid) {
    return dijkstra(grid, 4, 10);
}

// Main execution
var scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
var inputPath = scriptDir & "../input.txt";
var grid = parseInput(inputPath);

writeOutput("Part 1: " & part1(grid) & chr(10));
writeOutput("Part 2: " & part2(grid) & chr(10));
</cfscript>
