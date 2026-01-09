component {

    function run() {
        // Read input file
        var inputPath = getCurrentTemplatePath();
        var inputDir = getDirectoryFromPath(inputPath);
        var inputFile = inputDir & "../input.txt";
        var inputText = fileRead(inputFile).trim();

        // Parse the grid
        var grid = parseGrid(inputText);

        // Solve both parts
        var part1Result = part1(grid);
        var part2Result = part2(grid);

        systemOutput("Part 1: " & part1Result, true);
        systemOutput("Part 2: " & part2Result, true);
    }

    /**
     * Parse the input into a 2D array of tree heights
     */
    function parseGrid(required string input) {
        var lines = listToArray(arguments.input, chr(10));
        var grid = [];

        for (var line in lines) {
            line = trim(line);
            if (len(line) == 0) continue;

            var row = [];
            for (var i = 1; i <= len(line); i++) {
                arrayAppend(row, val(mid(line, i, 1)));
            }
            arrayAppend(grid, row);
        }

        return grid;
    }

    /**
     * Check if a tree at (row, col) is visible from outside the grid
     */
    function isVisible(required array grid, required numeric row, required numeric col) {
        var rows = arrayLen(arguments.grid);
        var cols = arrayLen(arguments.grid[1]);
        var height = arguments.grid[row][col];

        // Check from left
        var visibleLeft = true;
        for (var c = 1; c < arguments.col; c++) {
            if (arguments.grid[row][c] >= height) {
                visibleLeft = false;
                break;
            }
        }
        if (visibleLeft) return true;

        // Check from right
        var visibleRight = true;
        for (var c = arguments.col + 1; c <= cols; c++) {
            if (arguments.grid[row][c] >= height) {
                visibleRight = false;
                break;
            }
        }
        if (visibleRight) return true;

        // Check from top
        var visibleTop = true;
        for (var r = 1; r < arguments.row; r++) {
            if (arguments.grid[r][col] >= height) {
                visibleTop = false;
                break;
            }
        }
        if (visibleTop) return true;

        // Check from bottom
        var visibleBottom = true;
        for (var r = arguments.row + 1; r <= rows; r++) {
            if (arguments.grid[r][col] >= height) {
                visibleBottom = false;
                break;
            }
        }

        return visibleBottom;
    }

    /**
     * Calculate the scenic score for a tree at (row, col)
     */
    function scenicScore(required array grid, required numeric row, required numeric col) {
        var rows = arrayLen(arguments.grid);
        var cols = arrayLen(arguments.grid[1]);
        var height = arguments.grid[row][col];

        // Count trees visible looking left
        var leftCount = 0;
        for (var c = arguments.col - 1; c >= 1; c--) {
            leftCount++;
            if (arguments.grid[row][c] >= height) break;
        }

        // Count trees visible looking right
        var rightCount = 0;
        for (var c = arguments.col + 1; c <= cols; c++) {
            rightCount++;
            if (arguments.grid[row][c] >= height) break;
        }

        // Count trees visible looking up
        var upCount = 0;
        for (var r = arguments.row - 1; r >= 1; r--) {
            upCount++;
            if (arguments.grid[r][col] >= height) break;
        }

        // Count trees visible looking down
        var downCount = 0;
        for (var r = arguments.row + 1; r <= rows; r++) {
            downCount++;
            if (arguments.grid[r][col] >= height) break;
        }

        return leftCount * rightCount * upCount * downCount;
    }

    /**
     * Part 1: Count trees visible from outside the grid
     */
    function part1(required array grid) {
        var rows = arrayLen(arguments.grid);
        var cols = arrayLen(arguments.grid[1]);
        var count = 0;

        for (var r = 1; r <= rows; r++) {
            for (var c = 1; c <= cols; c++) {
                if (isVisible(arguments.grid, r, c)) {
                    count++;
                }
            }
        }

        return count;
    }

    /**
     * Part 2: Find maximum scenic score
     */
    function part2(required array grid) {
        var rows = arrayLen(arguments.grid);
        var cols = arrayLen(arguments.grid[1]);
        var maxScore = 0;

        for (var r = 1; r <= rows; r++) {
            for (var c = 1; c <= cols; c++) {
                var score = scenicScore(arguments.grid, r, c);
                if (score > maxScore) {
                    maxScore = score;
                }
            }
        }

        return maxScore;
    }

}
