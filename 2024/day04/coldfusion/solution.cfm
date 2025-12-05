<cfscript>
// Read input file
inputFile = getDirectoryFromPath(getCurrentTemplatePath()) & "../input.txt";
inputText = fileRead(inputFile).trim();

// Parse input into grid
grid = listToArray(inputText, chr(10));
rows = arrayLen(grid);
cols = len(grid[1]);

// 8 directions: right, left, down, up, and 4 diagonals
directions = [
    [0, 1],    // right
    [0, -1],   // left
    [1, 0],    // down
    [-1, 0],   // up
    [1, 1],    // down-right
    [1, -1],   // down-left
    [-1, 1],   // up-right
    [-1, -1]   // up-left
];

/**
 * Part 1: Find all occurrences of "XMAS" in any direction
 */
function part1() {
    var target = "XMAS";
    var count = 0;

    for (var r = 1; r <= rows; r++) {
        for (var c = 1; c <= cols; c++) {
            // Try each direction from this position
            for (var dir in directions) {
                var dr = dir[1];
                var dc = dir[2];
                var found = true;

                // Check if XMAS fits in this direction
                for (var i = 0; i < len(target); i++) {
                    var nr = r + dr * i;
                    var nc = c + dc * i;

                    if (nr < 1 || nr > rows || nc < 1 || nc > cols) {
                        found = false;
                        break;
                    }

                    if (mid(grid[nr], nc, 1) != mid(target, i + 1, 1)) {
                        found = false;
                        break;
                    }
                }

                if (found) {
                    count++;
                }
            }
        }
    }

    return count;
}

/**
 * Part 2: Find X-MAS patterns (two MAS forming an X with A in center)
 */
function part2() {
    var count = 0;

    // Check each possible center point (A must be in the middle)
    for (var r = 2; r <= rows - 1; r++) {
        for (var c = 2; c <= cols - 1; c++) {
            if (mid(grid[r], c, 1) != 'A') {
                continue;
            }

            // Get the four corners
            var topLeft = mid(grid[r - 1], c - 1, 1);
            var topRight = mid(grid[r - 1], c + 1, 1);
            var bottomLeft = mid(grid[r + 1], c - 1, 1);
            var bottomRight = mid(grid[r + 1], c + 1, 1);

            // Check diagonal 1 (top-left to bottom-right): MAS or SAM
            var diag1Ok = (topLeft == 'M' && bottomRight == 'S') || (topLeft == 'S' && bottomRight == 'M');

            // Check diagonal 2 (top-right to bottom-left): MAS or SAM
            var diag2Ok = (topRight == 'M' && bottomLeft == 'S') || (topRight == 'S' && bottomLeft == 'M');

            if (diag1Ok && diag2Ok) {
                count++;
            }
        }
    }

    return count;
}

// Execute and output results
writeOutput("Part 1: " & part1() & chr(10));
writeOutput("Part 2: " & part2() & chr(10));
</cfscript>
