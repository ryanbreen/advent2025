<cfscript>
// Read input file
scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
inputPath = scriptDir & "../input.txt";
inputText = fileRead(inputPath).trim();

// Parse input into grid
lines = listToArray(inputText, chr(10));
grid = [];
for (line in lines) {
    arrayAppend(grid, listToArray(line, ""));
}

// Global variables
rows = arrayLen(grid);
cols = arrayLen(grid[1]);
startRow = 0;
startCol = 0;
startDir = 0;

// Direction vectors: up, right, down, left
dirs = [
    {dr: -1, dc: 0},  // up
    {dr: 0, dc: 1},   // right
    {dr: 1, dc: 0},   // down
    {dr: 0, dc: -1}   // left
];

// Find starting position and direction
for (var r = 1; r <= rows; r++) {
    for (var c = 1; c <= cols; c++) {
        var cell = grid[r][c];
        if (listFind("^,v,<,>", cell)) {
            startRow = r;
            startCol = c;
            switch (cell) {
                case "^":
                    startDir = 0; // up
                    break;
                case ">":
                    startDir = 1; // right
                    break;
                case "v":
                    startDir = 2; // down
                    break;
                case "<":
                    startDir = 3; // left
                    break;
            }
            break;
        }
    }
    if (startRow > 0) break;
}

function part1() {
    // Track visited positions
    var visited = {};
    var visitedKey = startRow & ',' & startCol;
    visited[visitedKey] = true;

    var row = startRow;
    var col = startCol;
    var dir = startDir;

    while (true) {
        // Calculate next position
        var dirVector = dirs[dir + 1]; // CF arrays are 1-indexed
        var nextRow = row + dirVector.dr;
        var nextCol = col + dirVector.dc;

        // Check if next position is out of bounds
        if (nextRow < 1 || nextRow > rows || nextCol < 1 || nextCol > cols) {
            break;
        }

        // Check if there's an obstacle ahead
        var nextCell = grid[nextRow][nextCol];
        if (nextCell eq chr(35)) {
            // Turn right 90 degrees
            dir = (dir + 1) % 4;
        } else {
            // Move forward
            row = nextRow;
            col = nextCol;
            visitedKey = row & ',' & col;
            visited[visitedKey] = true;
        }
    }

    return structCount(visited);
}

function simulatePatrol(testGrid) {
    var visited = {};
    var visitedKey = startRow & ',' & startCol;
    visited[visitedKey] = true;

    var row = startRow;
    var col = startCol;
    var dir = startDir;

    while (true) {
        var dirVector = dirs[dir + 1];
        var nextRow = row + dirVector.dr;
        var nextCol = col + dirVector.dc;

        if (nextRow < 1 || nextRow > rows || nextCol < 1 || nextCol > cols) {
            break;
        }

        var nextCell = testGrid[nextRow][nextCol];
        if (nextCell eq chr(35)) {
            dir = (dir + 1) % 4;
        } else {
            row = nextRow;
            col = nextCol;
            visitedKey = row & ',' & col;
            visited[visitedKey] = true;
        }
    }

    return visited;
}

function detectLoop(testGrid) {
    var states = {};
    var row = startRow;
    var col = startCol;
    var dir = startDir;

    while (true) {
        // Create state key (position + direction)
        var stateKey = row & ',' & col & ',' & dir;
        if (structKeyExists(states, stateKey)) {
            return true; // Loop detected
        }
        states[stateKey] = true;

        var dirVector = dirs[dir + 1];
        var nextRow = row + dirVector.dr;
        var nextCol = col + dirVector.dc;

        if (nextRow < 1 || nextRow > rows || nextCol < 1 || nextCol > cols) {
            return false; // Guard left the area
        }

        var nextCell = testGrid[nextRow][nextCol];
        if (nextCell eq chr(35)) {
            dir = (dir + 1) % 4;
        } else {
            row = nextRow;
            col = nextCol;
        }
    }
}

function part2() {
    // First, get all positions visited in normal patrol
    var visitedPositions = simulatePatrol(grid);

    // Count positions where adding obstruction creates a loop
    var loopCount = 0;

    for (var posKey in visitedPositions) {
        var coords = listToArray(posKey, ',');
        var testRow = coords[1];
        var testCol = coords[2];

        // Can't place obstruction at starting position
        if (testRow == startRow && testCol == startCol) {
            continue;
        }

        // Create a copy of the grid with the obstruction
        var testGrid = [];
        for (var i = 1; i <= rows; i++) {
            var newRow = [];
            for (var j = 1; j <= cols; j++) {
                arrayAppend(newRow, grid[i][j]);
            }
            arrayAppend(testGrid, newRow);
        }

        // Place the obstruction
        testGrid[testRow][testCol] = chr(35);

        // Check if this creates a loop
        if (detectLoop(testGrid)) {
            loopCount++;
        }
    }

    return loopCount;
}

writeOutput("Part 1: " & part1() & chr(10));
writeOutput("Part 2: " & part2() & chr(10));
</cfscript>
