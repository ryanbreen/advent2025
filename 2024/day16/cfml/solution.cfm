<cfscript>
// Day 16: Reindeer Maze - Weighted shortest path with turn costs
// Implements Dijkstra's algorithm with bidirectional search

// Direction vectors: 0=East, 1=South, 2=West, 3=North (1-indexed)
DX = [1, 0, -1, 0];
DY = [0, 1, 0, -1];
WALL = chr(35);

// Create numeric key: x*10000*10 + y*10 + dir (supports grid up to 1000x1000)
function makeKey(x, y, d) {
    return x * 10000 + y * 10 + d;
}

// Create tile key (position only)
function makeTileKey(x, y) {
    return x * 10000 + y;
}

function parseInput(text) {
    var lines = listToArray(trim(text), chr(10));
    var grid = [];
    var startX = 0;
    var startY = 0;
    var endX = 0;
    var endY = 0;

    for (var y = 1; y <= arrayLen(lines); y++) {
        var row = listToArray(lines[y], "");
        arrayAppend(grid, row);

        for (var x = 1; x <= arrayLen(row); x++) {
            if (row[x] == "S") {
                startX = x;
                startY = y;
            } else if (row[x] == "E") {
                endX = x;
                endY = y;
            }
        }
    }

    return {
        grid: grid,
        startX: startX,
        startY: startY,
        endX: endX,
        endY: endY
    };
}

function pqPush(pq, cost, x, y, dir) {
    arrayAppend(pq, [cost, x, y, dir]);
    var i = arrayLen(pq);
    while (i > 1) {
        var parent = int(i / 2);
        if (pq[parent][1] <= pq[i][1]) break;
        var temp = pq[parent];
        pq[parent] = pq[i];
        pq[i] = temp;
        i = parent;
    }
}

function pqPop(pq) {
    if (arrayLen(pq) == 0) return [];

    var result = pq[1];
    var last = pq[arrayLen(pq)];
    arrayDeleteAt(pq, arrayLen(pq));

    if (arrayLen(pq) == 0) return result;

    pq[1] = last;
    var i = 1;
    while (true) {
        var left = i * 2;
        var right = i * 2 + 1;
        var smallest = i;

        if (left <= arrayLen(pq) && pq[left][1] < pq[smallest][1]) {
            smallest = left;
        }
        if (right <= arrayLen(pq) && pq[right][1] < pq[smallest][1]) {
            smallest = right;
        }

        if (smallest == i) break;

        var temp = pq[i];
        pq[i] = pq[smallest];
        pq[smallest] = temp;
        i = smallest;
    }

    return result;
}

function dijkstraForward(grid, startX, startY) {
    var pq = [];
    pqPush(pq, 0, startX, startY, 0);
    var dist = {};

    while (arrayLen(pq) > 0) {
        var current = pqPop(pq);
        var cost = current[1];
        var x = current[2];
        var y = current[3];
        var d = current[4];

        var stateKey = makeKey(x, y, d);
        if (structKeyExists(dist, stateKey)) {
            continue;
        }
        dist[stateKey] = cost;

        var nx = x + DX[d + 1];
        var ny = y + DY[d + 1];
        if (ny >= 1 && ny <= arrayLen(grid) &&
            nx >= 1 && nx <= arrayLen(grid[1]) &&
            grid[ny][nx] != WALL) {
            pqPush(pq, cost + 1, nx, ny, d);
        }

        var leftDir = ((d - 1) + 4) mod 4;
        pqPush(pq, cost + 1000, x, y, leftDir);

        var rightDir = (d + 1) mod 4;
        pqPush(pq, cost + 1000, x, y, rightDir);
    }

    return dist;
}

function dijkstraBackward(grid, endX, endY) {
    var pq = [];
    for (var d = 0; d < 4; d++) {
        pqPush(pq, 0, endX, endY, d);
    }
    var dist = {};

    while (arrayLen(pq) > 0) {
        var current = pqPop(pq);
        var cost = current[1];
        var x = current[2];
        var y = current[3];
        var d = current[4];

        var stateKey = makeKey(x, y, d);
        if (structKeyExists(dist, stateKey)) {
            continue;
        }
        dist[stateKey] = cost;

        var px = x - DX[d + 1];
        var py = y - DY[d + 1];
        if (py >= 1 && py <= arrayLen(grid) &&
            px >= 1 && px <= arrayLen(grid[1]) &&
            grid[py][px] != WALL) {
            pqPush(pq, cost + 1, px, py, d);
        }

        var leftDir = ((d - 1) + 4) mod 4;
        pqPush(pq, cost + 1000, x, y, leftDir);

        var rightDir = (d + 1) mod 4;
        pqPush(pq, cost + 1000, x, y, rightDir);
    }

    return dist;
}

function part1(grid, startX, startY, endX, endY) {
    var dist = dijkstraForward(grid, startX, startY);
    var minScore = 999999999;

    for (var d = 0; d < 4; d++) {
        var stateKey = makeKey(endX, endY, d);
        if (structKeyExists(dist, stateKey)) {
            minScore = min(minScore, dist[stateKey]);
        }
    }

    return minScore;
}

function part2(grid, startX, startY, endX, endY, bestScore) {
    var distFromStart = dijkstraForward(grid, startX, startY);
    var distToEnd = dijkstraBackward(grid, endX, endY);
    var tilesOnBestPath = {};

    for (var y = 1; y <= arrayLen(grid); y++) {
        for (var x = 1; x <= arrayLen(grid[y]); x++) {
            if (grid[y][x] == WALL) {
                continue;
            }

            for (var d = 0; d < 4; d++) {
                var stateKey = makeKey(x, y, d);
                var fromStart = structKeyExists(distFromStart, stateKey) ? distFromStart[stateKey] : 999999999;
                var toEnd = structKeyExists(distToEnd, stateKey) ? distToEnd[stateKey] : 999999999;

                if (fromStart + toEnd == bestScore) {
                    var tileKey = makeTileKey(x, y);
                    tilesOnBestPath[tileKey] = true;
                    break;
                }
            }
        }
    }

    return structCount(tilesOnBestPath);
}

basePath = getDirectoryFromPath(getCurrentTemplatePath());
inputPath = basePath & "../input.txt";
text = fileRead(inputPath);
parsed = parseInput(text);

answer1 = part1(parsed.grid, parsed.startX, parsed.startY, parsed.endX, parsed.endY);
writeOutput("Part 1: " & answer1 & chr(10));

answer2 = part2(parsed.grid, parsed.startX, parsed.startY, parsed.endX, parsed.endY, answer1);
writeOutput("Part 2: " & answer2 & chr(10));
</cfscript>
