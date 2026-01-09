component {

    function run() {
        // Read input file
        var inputPath = getCurrentTemplatePath();
        var inputDir = getDirectoryFromPath(inputPath);
        var inputFile = inputDir & "../input.txt";
        var inputText = fileRead(inputFile).trim();

        // Solve both parts
        var part1Result = part1(inputText);
        var part2Result = part2(inputText);

        systemOutput("Part 1: " & part1Result, true);
        systemOutput("Part 2: " & part2Result, true);
    }

    /**
     * Parse the grid and find start/end positions
     * Returns struct with grid (2D array), start position, end position
     */
    function parseGrid(required string input) {
        var lines = listToArray(arguments.input, chr(10));
        var grid = [];
        var startPos = {};
        var endPos = {};
        var gridRow = 0;

        for (var i = 1; i <= arrayLen(lines); i++) {
            var line = trim(lines[i]);
            if (len(line) == 0) continue;

            gridRow++;
            var row = [];
            for (var c = 1; c <= len(line); c++) {
                var ch = mid(line, c, 1);

                if (compare(ch, "S") == 0) {
                    startPos = {row: gridRow, col: c};
                    ch = "a";
                } else if (compare(ch, "E") == 0) {
                    endPos = {row: gridRow, col: c};
                    ch = "z";
                }

                arrayAppend(row, ch);
            }
            arrayAppend(grid, row);
        }

        return {
            grid: grid,
            start: startPos,
            end: endPos
        };
    }

    /**
     * BFS to find shortest path from any start position to end
     * starts: array of {row, col} positions
     * end: {row, col} position
     * grid: 2D array of height characters
     */
    function bfs(required array grid, required array starts, required struct endPos) {
        var rows = arrayLen(arguments.grid);
        var cols = arrayLen(arguments.grid[1]);

        // Use struct as visited set (key = "row,col")
        var visited = {};

        // Queue: array of structs {row, col, dist}
        var queue = [];

        // Add all start positions to queue
        for (var start in arguments.starts) {
            var key = start.row & "," & start.col;
            visited[key] = true;
            arrayAppend(queue, {row: start.row, col: start.col, dist: 0});
        }

        // Direction deltas: up, down, left, right
        var directions = [
            {dr: -1, dc: 0},
            {dr: 1, dc: 0},
            {dr: 0, dc: -1},
            {dr: 0, dc: 1}
        ];

        // BFS loop
        var queueIdx = 1;
        while (queueIdx <= arrayLen(queue)) {
            var current = queue[queueIdx];
            queueIdx++;

            var r = current.row;
            var c = current.col;
            var dist = current.dist;

            // Check if we reached the end
            if (r == arguments.endPos.row && c == arguments.endPos.col) {
                return dist;
            }

            var currentHeight = asc(arguments.grid[r][c]);

            // Explore neighbors
            for (var dir in directions) {
                var nr = r + dir.dr;
                var nc = c + dir.dc;

                // Check bounds
                if (nr >= 1 && nr <= rows && nc >= 1 && nc <= cols) {
                    var nKey = nr & "," & nc;

                    if (!structKeyExists(visited, nKey)) {
                        var nextHeight = asc(arguments.grid[nr][nc]);

                        // Can move if destination is at most 1 higher
                        if (nextHeight <= currentHeight + 1) {
                            visited[nKey] = true;
                            arrayAppend(queue, {row: nr, col: nc, dist: dist + 1});
                        }
                    }
                }
            }
        }

        return -1; // No path found
    }

    /**
     * Part 1: Find shortest path from S to E
     */
    function part1(required string input) {
        var parsed = parseGrid(arguments.input);
        var starts = [parsed.start];
        return bfs(parsed.grid, starts, parsed.end);
    }

    /**
     * Part 2: Find shortest path from any 'a' to E
     */
    function part2(required string input) {
        var parsed = parseGrid(arguments.input);

        // Find all cells with elevation 'a'
        var starts = [];
        for (var r = 1; r <= arrayLen(parsed.grid); r++) {
            for (var c = 1; c <= arrayLen(parsed.grid[r]); c++) {
                if (compare(parsed.grid[r][c], "a") == 0) {
                    arrayAppend(starts, {row: r, col: c});
                }
            }
        }

        return bfs(parsed.grid, starts, parsed.end);
    }

}
