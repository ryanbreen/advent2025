component {

    /**
     * Day 24: Blizzard Basin
     * BFS through a grid with moving blizzards that wrap around.
     */

    public void function run() {
        var inputPath = getDirectoryFromPath(getCurrentTemplatePath()) & "../input.txt";
        var text = fileRead(inputPath);

        writeOutput("Part 1: " & part1(text) & chr(10));
        writeOutput("Part 2: " & part2(text) & chr(10));
    }

    /**
     * Parse the map and extract blizzard positions, dimensions, start, and end.
     */
    private struct function parseInput(required string text) {
        var lines = listToArray(trim(arguments.text), chr(10));
        var height = arrayLen(lines);
        var width = len(lines[1]);

        // Inner dimensions (excluding walls)
        var innerH = height - 2;
        var innerW = width - 2;

        var blizzards = [];
        for (var r = 1; r <= height; r++) {
            var line = lines[r];
            for (var c = 1; c <= width; c++) {
                var ch = mid(line, c, 1);
                if (ch == "^" || ch == "v" || ch == "<" || ch == ">") {
                    arrayAppend(blizzards, {r: r, c: c, dir: ch});
                }
            }
        }

        // Find start position (only '.' in first row)
        var startCol = 0;
        for (var c = 1; c <= width; c++) {
            if (mid(lines[1], c, 1) == ".") {
                startCol = c;
                break;
            }
        }

        // Find end position (only '.' in last row)
        var endCol = 0;
        for (var c = 1; c <= width; c++) {
            if (mid(lines[height], c, 1) == ".") {
                endCol = c;
                break;
            }
        }

        return {
            blizzards: blizzards,
            height: height,
            width: width,
            innerH: innerH,
            innerW: innerW,
            startR: 1,
            startC: startCol,
            endR: height,
            endC: endCol
        };
    }

    /**
     * Compute GCD of two numbers.
     */
    private numeric function gcd(required numeric a, required numeric b) {
        var x = abs(arguments.a);
        var y = abs(arguments.b);
        while (y != 0) {
            var temp = y;
            y = x mod y;
            x = temp;
        }
        return x;
    }

    /**
     * Compute LCM of two numbers.
     */
    private numeric function lcm(required numeric a, required numeric b) {
        return (arguments.a * arguments.b) / gcd(arguments.a, arguments.b);
    }

    /**
     * Get all blizzard positions at a given time as a set (struct with keys).
     */
    private struct function getBlizzardPositions(required array blizzards, required numeric innerH, required numeric innerW, required numeric time) {
        var positions = {};

        for (var i = 1; i <= arrayLen(arguments.blizzards); i++) {
            var bliz = arguments.blizzards[i];
            // Convert to 0-based inner coordinates
            var ir = bliz.r - 2;  // -1 for wall, -1 for 0-based
            var ic = bliz.c - 2;

            var nr = ir;
            var nc = ic;

            if (bliz.dir == "^") {
                nr = ((ir - arguments.time) mod arguments.innerH + arguments.innerH) mod arguments.innerH;
                nc = ic;
            } else if (bliz.dir == "v") {
                nr = (ir + arguments.time) mod arguments.innerH;
                nc = ic;
            } else if (bliz.dir == "<") {
                nr = ir;
                nc = ((ic - arguments.time) mod arguments.innerW + arguments.innerW) mod arguments.innerW;
            } else if (bliz.dir == ">") {
                nr = ir;
                nc = (ic + arguments.time) mod arguments.innerW;
            }

            // Convert back to 1-based full coordinates
            var finalR = nr + 2;
            var finalC = nc + 2;
            positions[finalR & "," & finalC] = true;
        }

        return positions;
    }

    /**
     * BFS to find shortest path avoiding blizzards.
     */
    private numeric function bfs(
        required array blizzards,
        required numeric height,
        required numeric width,
        required numeric innerH,
        required numeric innerW,
        required numeric startR,
        required numeric startC,
        required numeric endR,
        required numeric endC,
        required numeric startTime
    ) {
        var period = lcm(arguments.innerH, arguments.innerW);

        // Precompute blizzard positions for all times in one period
        var blizzardCache = {};
        for (var t = 0; t < period; t++) {
            blizzardCache[t] = getBlizzardPositions(arguments.blizzards, arguments.innerH, arguments.innerW, t);
        }

        // BFS using array as queue
        // State: {time, r, c}
        var queue = [{time: arguments.startTime, r: arguments.startR, c: arguments.startC}];
        var queueHead = 1;

        var visited = {};
        var initialState = (arguments.startTime mod period) & "," & arguments.startR & "," & arguments.startC;
        visited[initialState] = true;

        // Directions: wait, up, down, left, right
        var directions = [[0, 0], [-1, 0], [1, 0], [0, -1], [0, 1]];

        while (queueHead <= arrayLen(queue)) {
            var current = queue[queueHead];
            queueHead++;

            var curTime = current.time;
            var curR = current.r;
            var curC = current.c;

            if (curR == arguments.endR && curC == arguments.endC) {
                return curTime;
            }

            var nextTime = curTime + 1;
            var nextBlizzards = blizzardCache[nextTime mod period];

            for (var d = 1; d <= arrayLen(directions); d++) {
                var dir = directions[d];
                var nr = curR + dir[1];
                var nc = curC + dir[2];

                // Check if position is valid
                var isValid = false;

                // Check if it's start or end position
                if ((nr == arguments.startR && nc == arguments.startC) ||
                    (nr == arguments.endR && nc == arguments.endC)) {
                    isValid = true;
                }
                // Check if it's within the inner grid
                else if (nr >= 2 && nr <= arguments.height - 1 && nc >= 2 && nc <= arguments.width - 1) {
                    isValid = true;
                }

                if (!isValid) {
                    continue;
                }

                // Check for blizzards
                var posKey = nr & "," & nc;
                if (structKeyExists(nextBlizzards, posKey)) {
                    continue;
                }

                var stateKey = (nextTime mod period) & "," & nr & "," & nc;
                if (!structKeyExists(visited, stateKey)) {
                    visited[stateKey] = true;
                    arrayAppend(queue, {time: nextTime, r: nr, c: nc});
                }
            }
        }

        return -1; // No path found
    }

    /**
     * Part 1: Find shortest path from start to end.
     */
    public numeric function part1(required string text) {
        var data = parseInput(arguments.text);
        return bfs(
            data.blizzards,
            data.height,
            data.width,
            data.innerH,
            data.innerW,
            data.startR,
            data.startC,
            data.endR,
            data.endC,
            0
        );
    }

    /**
     * Part 2: Find shortest path start -> end -> start -> end.
     */
    public numeric function part2(required string text) {
        var data = parseInput(arguments.text);

        // Trip 1: start to end
        var t1 = bfs(
            data.blizzards,
            data.height,
            data.width,
            data.innerH,
            data.innerW,
            data.startR,
            data.startC,
            data.endR,
            data.endC,
            0
        );

        // Trip 2: end to start
        var t2 = bfs(
            data.blizzards,
            data.height,
            data.width,
            data.innerH,
            data.innerW,
            data.endR,
            data.endC,
            data.startR,
            data.startC,
            t1
        );

        // Trip 3: start to end again
        var t3 = bfs(
            data.blizzards,
            data.height,
            data.width,
            data.innerH,
            data.innerW,
            data.startR,
            data.startC,
            data.endR,
            data.endC,
            t2
        );

        return t3;
    }

}
