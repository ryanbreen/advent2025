<cfscript>
// Day 14: Restroom Redoubt - ColdFusion solution

// Read input
currentDir = getDirectoryFromPath(getCurrentTemplatePath());
inputFile = currentDir & "../input.txt";
inputText = trim(fileRead(inputFile));

WIDTH = 101;
HEIGHT = 103;

/**
 * Custom modulo function that handles negative numbers correctly
 */
function customMod(n, m) {
    var result = n % m;
    if (result < 0) {
        result = result + m;
    }
    return result;
}

/**
 * Parse robots from input text
 */
function parseRobots(text) {
    var robots = [];
    var lines = listToArray(text, chr(10));

    for (var line in lines) {
        line = trim(line);
        if (len(line) == 0) continue;

        // Extract numbers using regex
        var match = reFind("p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)", line, 1, true);
        if (match.pos[1] > 0) {
            var px = val(mid(line, match.pos[2], match.len[2]));
            var py = val(mid(line, match.pos[3], match.len[3]));
            var vx = val(mid(line, match.pos[4], match.len[4]));
            var vy = val(mid(line, match.pos[5], match.len[5]));

            arrayAppend(robots, {
                px: px,
                py: py,
                vx: vx,
                vy: vy
            });
        }
    }

    return robots;
}

/**
 * Simulate robot movement for given seconds
 */
function simulate(robots, seconds) {
    var positions = [];

    for (var robot in robots) {
        var newX = customMod(robot.px + robot.vx * seconds, WIDTH);
        var newY = customMod(robot.py + robot.vy * seconds, HEIGHT);

        arrayAppend(positions, {x: newX, y: newY});
    }

    return positions;
}

/**
 * Count robots in each quadrant
 */
function countQuadrants(positions) {
    var midX = int(WIDTH / 2);  // 50
    var midY = int(HEIGHT / 2); // 51

    var q1 = 0;
    var q2 = 0;
    var q3 = 0;
    var q4 = 0;

    for (var pos in positions) {
        // Skip robots on middle lines
        if (pos.x == midX || pos.y == midY) {
            continue;
        }

        if (pos.x < midX && pos.y < midY) {
            q1++;  // Top-left
        } else if (pos.x > midX && pos.y < midY) {
            q2++;  // Top-right
        } else if (pos.x < midX && pos.y > midY) {
            q3++;  // Bottom-left
        } else {
            q4++;  // Bottom-right
        }
    }

    return q1 * q2 * q3 * q4;
}

/**
 * Part 1: Safety factor after 100 seconds
 */
function part1(robots) {
    var positions = simulate(robots, 100);
    return countQuadrants(positions);
}

/**
 * Part 2: Find when robots form a Christmas tree pattern
 */
function part2(robots) {
    var maxSeconds = WIDTH * HEIGHT;

    for (var seconds = 1; seconds <= maxSeconds; seconds++) {
        var positions = simulate(robots, seconds);

        // Create a set of positions for fast lookup
        var posSet = {};
        for (var pos in positions) {
            var key = toString(pos.x) & "," & toString(pos.y);
            posSet[key] = true;
        }

        // Look for a horizontal line of at least 20 consecutive robots
        for (var y = 0; y < HEIGHT; y++) {
            var maxConsecutive = 0;
            var consecutive = 0;

            for (var x = 0; x < WIDTH; x++) {
                var key = toString(x) & "," & toString(y);
                if (structKeyExists(posSet, key)) {
                    consecutive++;
                    if (consecutive > maxConsecutive) {
                        maxConsecutive = consecutive;
                    }
                } else {
                    consecutive = 0;
                }
            }

            if (maxConsecutive >= 20) {
                return seconds;
            }
        }
    }

    return -1;
}

// Main execution
robots = parseRobots(inputText);

writeOutput("Part 1: " & part1(robots) & chr(10));
writeOutput("Part 2: " & part2(robots) & chr(10));
</cfscript>
