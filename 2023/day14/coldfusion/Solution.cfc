component {

    function run() {
        // Get the directory containing this CFC file
        var scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
        var inputPath = scriptDir & "../input.txt";
        var inputText = fileRead(inputPath);
        var grid = parseInput(inputText);

        writeOutput("Part 1: " & part1(grid) & chr(10));
        writeOutput("Part 2: " & part2(grid) & chr(10));
    }

    function parseInput(required string text) {
        // Split by newlines to get rows
        var lines = text.trim().split("\r?\n");
        var grid = [];

        for (var line in lines) {
            // Convert each line to an array of characters
            var row = [];
            for (var i = 1; i <= len(line); i++) {
                arrayAppend(row, mid(line, i, 1));
            }
            arrayAppend(grid, row);
        }

        return grid;
    }

    function copyGrid(required array grid) {
        var newGrid = [];
        for (var row in grid) {
            arrayAppend(newGrid, duplicate(row));
        }
        return newGrid;
    }

    function tiltNorth(required array grid) {
        var rows = arrayLen(grid);
        var cols = arrayLen(grid[1]);

        for (var col = 1; col <= cols; col++) {
            var writePos = 1;
            for (var row = 1; row <= rows; row++) {
                if (grid[row][col] == chr(35)) {
                    writePos = row + 1;
                } else if (grid[row][col] == "O") {
                    grid[row][col] = ".";
                    grid[writePos][col] = "O";
                    writePos++;
                }
            }
        }
    }

    function tiltSouth(required array grid) {
        var rows = arrayLen(grid);
        var cols = arrayLen(grid[1]);

        for (var col = 1; col <= cols; col++) {
            var writePos = rows;
            for (var row = rows; row >= 1; row--) {
                if (grid[row][col] == chr(35)) {
                    writePos = row - 1;
                } else if (grid[row][col] == "O") {
                    grid[row][col] = ".";
                    grid[writePos][col] = "O";
                    writePos--;
                }
            }
        }
    }

    function tiltWest(required array grid) {
        var rows = arrayLen(grid);
        var cols = arrayLen(grid[1]);

        for (var row = 1; row <= rows; row++) {
            var writePos = 1;
            for (var col = 1; col <= cols; col++) {
                if (grid[row][col] == chr(35)) {
                    writePos = col + 1;
                } else if (grid[row][col] == "O") {
                    grid[row][col] = ".";
                    grid[row][writePos] = "O";
                    writePos++;
                }
            }
        }
    }

    function tiltEast(required array grid) {
        var rows = arrayLen(grid);
        var cols = arrayLen(grid[1]);

        for (var row = 1; row <= rows; row++) {
            var writePos = cols;
            for (var col = cols; col >= 1; col--) {
                if (grid[row][col] == chr(35)) {
                    writePos = col - 1;
                } else if (grid[row][col] == "O") {
                    grid[row][col] = ".";
                    grid[row][writePos] = "O";
                    writePos--;
                }
            }
        }
    }

    function spinCycle(required array grid) {
        tiltNorth(grid);
        tiltWest(grid);
        tiltSouth(grid);
        tiltEast(grid);
    }

    function gridToString(required array grid) {
        var result = "";
        for (var row in grid) {
            result &= arrayToList(row, "");
        }
        return result;
    }

    function calculateLoad(required array grid) {
        var rows = arrayLen(grid);
        var total = 0;

        for (var row = 1; row <= rows; row++) {
            for (var cell in grid[row]) {
                if (cell == "O") {
                    total += (rows - row + 1);
                }
            }
        }

        return total;
    }

    function part1(required array grid) {
        var workGrid = copyGrid(grid);
        tiltNorth(workGrid);
        return calculateLoad(workGrid);
    }

    function part2(required array grid) {
        var workGrid = copyGrid(grid);
        var target = 1000000000;

        var seen = {};
        var cycleNum = 0;

        while (cycleNum < target) {
            var state = gridToString(workGrid);

            if (structKeyExists(seen, state)) {
                var cycleStart = seen[state];
                var cycleLength = cycleNum - cycleStart;
                var remaining = (target - cycleNum) mod cycleLength;

                for (var i = 1; i <= remaining; i++) {
                    spinCycle(workGrid);
                }

                return calculateLoad(workGrid);
            }

            seen[state] = cycleNum;
            spinCycle(workGrid);
            cycleNum++;
        }

        return calculateLoad(workGrid);
    }

}
