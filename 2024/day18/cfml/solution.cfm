<cfscript>
/**
 * Advent of Code 2024 - Day 18: RAM Run
 * ColdFusion (CFML) Solution
 */

// Get the directory of this script
scriptDir = getDirectoryFromPath(getCurrentTemplatePath());

// Read and parse input file
function parseInput(filepath) {
    var content = fileRead(filepath);
    var lines = listToArray(content, chr(10));
    var positions = [];

    for (var line in lines) {
        line = trim(line);
        if (len(line) > 0) {
            var parts = listToArray(line, ",");
            arrayAppend(positions, {
                "x": val(parts[1]),
                "y": val(parts[2])
            });
        }
    }

    return positions;
}

// Create a key for position in a set (struct)
function posKey(x, y) {
    return x & "," & y;
}

// BFS to find shortest path from (0,0) to (size-1, size-1)
function bfs(corrupted, size) {
    var startX = 0;
    var startY = 0;
    var goalX = size - 1;
    var goalY = size - 1;

    // Check if start or goal is corrupted
    if (structKeyExists(corrupted, posKey(startX, startY)) ||
        structKeyExists(corrupted, posKey(goalX, goalY))) {
        return -1;
    }

    // Queue: array of structs with x, y, steps
    var queue = [];
    arrayAppend(queue, {"x": startX, "y": startY, "steps": 0});
    var queueStart = 1;

    // Visited set (using struct for O(1) lookup)
    var visited = {};
    visited[posKey(startX, startY)] = true;

    // Directions: up, down, left, right
    var directions = [
        {"dx": 0, "dy": 1},
        {"dx": 0, "dy": -1},
        {"dx": 1, "dy": 0},
        {"dx": -1, "dy": 0}
    ];

    while (queueStart <= arrayLen(queue)) {
        var current = queue[queueStart];
        queueStart++;

        var x = current.x;
        var y = current.y;
        var steps = current.steps;

        // Check if reached goal
        if (x == goalX && y == goalY) {
            return steps;
        }

        // Explore neighbors
        for (var dir in directions) {
            var nx = x + dir.dx;
            var ny = y + dir.dy;
            var nkey = posKey(nx, ny);

            // Check bounds and if not visited and not corrupted
            if (nx >= 0 && nx < size && ny >= 0 && ny < size &&
                !structKeyExists(visited, nkey) &&
                !structKeyExists(corrupted, nkey)) {
                visited[nkey] = true;
                arrayAppend(queue, {"x": nx, "y": ny, "steps": steps + 1});
            }
        }
    }

    // No path found
    return -1;
}

// Part 1: Find shortest path after first 1024 bytes have fallen
function part1(positions, numBytes, size) {
    var corrupted = {};
    var limit = min(numBytes, arrayLen(positions));

    for (var i = 1; i <= limit; i++) {
        var pos = positions[i];
        corrupted[posKey(pos.x, pos.y)] = true;
    }

    return bfs(corrupted, size);
}

// Part 2: Binary search to find first byte that blocks all paths
function part2(positions, size) {
    var left = 0;
    var right = arrayLen(positions);

    while (left < right) {
        var mid = int((left + right) / 2);

        // Create corrupted set for positions 0 to mid (inclusive)
        var corrupted = {};
        for (var i = 1; i <= mid + 1; i++) {
            var pos = positions[i];
            corrupted[posKey(pos.x, pos.y)] = true;
        }

        if (bfs(corrupted, size) == -1) {
            right = mid;
        } else {
            left = mid + 1;
        }
    }

    var blockingPos = positions[left + 1];
    return blockingPos.x & "," & blockingPos.y;
}

// Main execution
inputFile = scriptDir & "../input.txt";
positions = parseInput(inputFile);

result1 = part1(positions, 1024, 71);
writeOutput("Part 1: " & result1 & chr(10));

result2 = part2(positions, 71);
writeOutput("Part 2: " & result2 & chr(10));
</cfscript>
