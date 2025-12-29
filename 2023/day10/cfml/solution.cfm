<cfscript>
// Day 10: Pipe Maze

// Read input - get directory of this script
scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
inputPath = scriptDir & "../input.txt";
inputText = fileRead(inputPath).trim();
lines = listToArray(inputText, chr(10));

// Pipe connections: each pipe connects to certain directions
// Directions: N=(-1,0), S=(1,0), E=(0,1), W=(0,-1)
PIPE_CONNECTIONS = {
    "|": [{"dr": -1, "dc": 0}, {"dr": 1, "dc": 0}],   // N, S
    "-": [{"dr": 0, "dc": -1}, {"dr": 0, "dc": 1}],   // W, E
    "L": [{"dr": -1, "dc": 0}, {"dr": 0, "dc": 1}],   // N, E
    "J": [{"dr": -1, "dc": 0}, {"dr": 0, "dc": -1}],  // N, W
    "7": [{"dr": 1, "dc": 0}, {"dr": 0, "dc": -1}],   // S, W
    "F": [{"dr": 1, "dc": 0}, {"dr": 0, "dc": 1}]     // S, E
};

function findStart(grid) {
    for (var r = 1; r <= arrayLen(grid); r++) {
        var row = grid[r];
        for (var c = 1; c <= len(row); c++) {
            if (mid(row, c, 1) == "S") {
                return {"r": r, "c": c};
            }
        }
    }
    return {};
}

function posKey(r, c) {
    return r & "," & c;
}

function getNeighbors(grid, r, c) {
    var rows = arrayLen(grid);
    var cols = len(grid[1]);
    var ch = mid(grid[r], c, 1);
    var neighbors = [];
    var directions = [{"dr": -1, "dc": 0}, {"dr": 1, "dc": 0}, {"dr": 0, "dc": -1}, {"dr": 0, "dc": 1}];

    if (ch == "S") {
        // S can connect to any adjacent pipe that connects back to it
        for (var d in directions) {
            var nr = r + d.dr;
            var nc = c + d.dc;
            if (nr >= 1 && nr <= rows && nc >= 1 && nc <= cols) {
                var adjCh = mid(grid[nr], nc, 1);
                if (structKeyExists(PIPE_CONNECTIONS, adjCh)) {
                    // Check if adjacent pipe connects back to S
                    for (var adj in PIPE_CONNECTIONS[adjCh]) {
                        if (nr + adj.dr == r && nc + adj.dc == c) {
                            arrayAppend(neighbors, {"r": nr, "c": nc});
                            break;
                        }
                    }
                }
            }
        }
    } else if (structKeyExists(PIPE_CONNECTIONS, ch)) {
        for (var d in PIPE_CONNECTIONS[ch]) {
            var nr = r + d.dr;
            var nc = c + d.dc;
            if (nr >= 1 && nr <= rows && nc >= 1 && nc <= cols) {
                arrayAppend(neighbors, {"r": nr, "c": nc});
            }
        }
    }

    return neighbors;
}

function findLoop(grid, start) {
    // BFS to find the main loop and distances from start
    var distances = {};
    var startKey = posKey(start.r, start.c);
    distances[startKey] = 0;

    // Simple queue using array
    var queue = [{"r": start.r, "c": start.c}];
    var queueStart = 1;

    while (queueStart <= arrayLen(queue)) {
        var pos = queue[queueStart];
        queueStart++;

        var currentKey = posKey(pos.r, pos.c);
        var neighbors = getNeighbors(grid, pos.r, pos.c);

        for (var neighbor in neighbors) {
            var nKey = posKey(neighbor.r, neighbor.c);
            if (!structKeyExists(distances, nKey)) {
                distances[nKey] = distances[currentKey] + 1;
                arrayAppend(queue, neighbor);
            }
        }
    }

    return distances;
}

function determineStartPipe(grid, start, loopPositions) {
    var r = start.r;
    var c = start.c;
    var rows = arrayLen(grid);
    var cols = len(grid[1]);
    var directions = [{"dr": -1, "dc": 0}, {"dr": 1, "dc": 0}, {"dr": 0, "dc": -1}, {"dr": 0, "dc": 1}];

    var connections = [];

    for (var d in directions) {
        var nr = r + d.dr;
        var nc = c + d.dc;
        var nKey = posKey(nr, nc);

        if (structKeyExists(loopPositions, nKey) && nr >= 1 && nr <= rows && nc >= 1 && nc <= cols) {
            var adjCh = mid(grid[nr], nc, 1);
            if (structKeyExists(PIPE_CONNECTIONS, adjCh)) {
                // Check if this pipe connects back to S
                for (var adj in PIPE_CONNECTIONS[adjCh]) {
                    if (nr + adj.dr == r && nc + adj.dc == c) {
                        arrayAppend(connections, {"dr": d.dr, "dc": d.dc});
                        break;
                    }
                }
            }
        }
    }

    // Match connections to a pipe type
    for (var pipe in PIPE_CONNECTIONS) {
        var dirs = PIPE_CONNECTIONS[pipe];
        if (arrayLen(dirs) == arrayLen(connections) && arrayLen(connections) == 2) {
            var match1 = false;
            var match2 = false;
            for (var conn in connections) {
                if (conn.dr == dirs[1].dr && conn.dc == dirs[1].dc) match1 = true;
                if (conn.dr == dirs[2].dr && conn.dc == dirs[2].dc) match2 = true;
            }
            if (match1 && match2) {
                return pipe;
            }
        }
    }

    return "S";
}

function part1() {
    var start = findStart(lines);
    var distances = findLoop(lines, start);

    var maxDist = 0;
    for (var key in distances) {
        if (distances[key] > maxDist) {
            maxDist = distances[key];
        }
    }

    return maxDist;
}

function part2() {
    var start = findStart(lines);
    var distances = findLoop(lines, start);
    var loopPositions = distances;

    // Determine what pipe S actually is
    var startPipe = determineStartPipe(lines, start, loopPositions);

    // Create mutable grid copy
    var grid = [];
    for (var row in lines) {
        arrayAppend(grid, row);
    }

    // Replace S with its actual pipe type
    var rowStr = grid[start.r];
    grid[start.r] = left(rowStr, start.c - 1) & startPipe & mid(rowStr, start.c + 1, len(rowStr) - start.c);

    var rows = arrayLen(grid);
    var cols = len(grid[1]);
    var enclosed = 0;

    // Ray casting: scan each row, toggle inside when crossing north-connected pipes
    for (var r = 1; r <= rows; r++) {
        var inside = false;
        for (var c = 1; c <= cols; c++) {
            var key = posKey(r, c);
            if (structKeyExists(loopPositions, key)) {
                var ch = mid(grid[r], c, 1);
                // Count pipes that have a north connection: |, L, J
                if (ch == "|" || ch == "L" || ch == "J") {
                    inside = !inside;
                }
            } else {
                if (inside) {
                    enclosed++;
                }
            }
        }
    }

    return enclosed;
}

writeOutput("Part 1: " & part1() & chr(10));
writeOutput("Part 2: " & part2() & chr(10));
</cfscript>
