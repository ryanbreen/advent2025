<cfscript>
// Parse input into grid array
function parseInput(filename) {
    var inputText = fileRead(filename);
    var lines = listToArray(inputText.trim(), chr(10), false, true);
    var grid = [];

    for (var line in lines) {
        arrayAppend(grid, line);
    }

    return grid;
}

// Count energized tiles from a starting position and direction
// Directions: 0=right, 1=down, 2=left, 3=up
function countEnergized(grid, startRow, startCol, startDir) {
    var rows = arrayLen(grid);
    var cols = len(grid[1]);

    // Direction deltas
    var dr = [0, 1, 0, -1];
    var dc = [1, 0, -1, 0];

    // Track visited states as "row,col,dir"
    var visited = {};

    // BFS queue - array of structs
    var queue = [];
    arrayAppend(queue, {r: startRow, c: startCol, d: startDir});

    while (arrayLen(queue) > 0) {
        var current = queue[1];
        arrayDeleteAt(queue, 1);

        var r = current.r;
        var c = current.c;
        var d = current.d;

        // Bounds check (1-based indexing)
        if (r < 1 || r > rows || c < 1 || c > cols) {
            continue;
        }

        // Check if state already visited
        var stateKey = r & "," & c & "," & d;
        if (structKeyExists(visited, stateKey)) {
            continue;
        }
        visited[stateKey] = true;

        // Get current cell
        var cell = mid(grid[r], c, 1);

        // Determine next directions based on cell type
        var nextDirs = [];

        if (cell == ".") {
            nextDirs = [d];
        } else if (cell == "/") {
            // Mirror: right->up, down->left, left->down, up->right
            var slashMap = [3, 2, 1, 0];
            nextDirs = [slashMap[d + 1]];
        } else if (cell == "\") {
            // Mirror: right->down, down->right, left->up, up->left
            var backslashMap = [1, 0, 3, 2];
            nextDirs = [backslashMap[d + 1]];
        } else if (cell == "|") {
            // Vertical splitter
            if (d == 0 || d == 2) {
                // Horizontal beam splits
                nextDirs = [1, 3];
            } else {
                // Vertical beam passes through
                nextDirs = [d];
            }
        } else if (cell == "-") {
            // Horizontal splitter
            if (d == 1 || d == 3) {
                // Vertical beam splits
                nextDirs = [0, 2];
            } else {
                // Horizontal beam passes through
                nextDirs = [d];
            }
        }

        // Add next states to queue
        for (var nd in nextDirs) {
            var newR = r + dr[nd + 1];
            var newC = c + dc[nd + 1];
            arrayAppend(queue, {r: newR, c: newC, d: nd});
        }
    }

    // Count unique tiles (ignore direction)
    var tiles = {};
    for (var key in visited) {
        var parts = listToArray(key, ",");
        var tileKey = parts[1] & "," & parts[2];
        tiles[tileKey] = true;
    }

    return structCount(tiles);
}

// Part 1: Beam starts at top-left heading right
function part1(grid) {
    return countEnergized(grid, 1, 1, 0);
}

// Part 2: Find maximum energized tiles from any edge position
function part2(grid) {
    var rows = arrayLen(grid);
    var cols = len(grid[1]);
    var maxEnergized = 0;

    // Top row, heading down
    for (var c = 1; c <= cols; c++) {
        maxEnergized = max(maxEnergized, countEnergized(grid, 1, c, 1));
    }

    // Bottom row, heading up
    for (var c = 1; c <= cols; c++) {
        maxEnergized = max(maxEnergized, countEnergized(grid, rows, c, 3));
    }

    // Left column, heading right
    for (var r = 1; r <= rows; r++) {
        maxEnergized = max(maxEnergized, countEnergized(grid, r, 1, 0));
    }

    // Right column, heading left
    for (var r = 1; r <= rows; r++) {
        maxEnergized = max(maxEnergized, countEnergized(grid, r, cols, 2));
    }

    return maxEnergized;
}

// Main execution
var scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
var inputPath = scriptDir & "../input.txt";
var grid = parseInput(inputPath);

writeOutput("Part 1: " & part1(grid) & chr(10));
writeOutput("Part 2: " & part2(grid) & chr(10));
</cfscript>
