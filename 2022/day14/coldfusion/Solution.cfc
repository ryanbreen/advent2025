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
     * Parse rock paths from input and return set of rock positions
     * Returns struct with:
     *   - rocks: struct (used as set) with keys "x,y"
     *   - maxY: maximum y coordinate found
     */
    function parsePaths(required string input) {
        var rocks = {};
        var maxY = 0;
        var lines = listToArray(arguments.input, chr(10));

        for (var line in lines) {
            line = trim(line);
            if (len(line) == 0) continue;

            // Split by " -> " to get points
            var points = listToArray(line, " -> ");

            for (var i = 1; i < arrayLen(points); i++) {
                // Parse coordinates
                var coords1 = listToArray(points[i], ",");
                var coords2 = listToArray(points[i + 1], ",");

                var x1 = val(coords1[1]);
                var y1 = val(coords1[2]);
                var x2 = val(coords2[1]);
                var y2 = val(coords2[2]);

                // Draw line from (x1, y1) to (x2, y2)
                if (x1 == x2) {
                    // Vertical line
                    var minY = min(y1, y2);
                    var maxYLine = max(y1, y2);
                    for (var y = minY; y <= maxYLine; y++) {
                        rocks[x1 & "," & y] = true;
                        if (y > maxY) maxY = y;
                    }
                } else {
                    // Horizontal line
                    var minX = min(x1, x2);
                    var maxX = max(x1, x2);
                    for (var x = minX; x <= maxX; x++) {
                        rocks[x & "," & y1] = true;
                        if (y1 > maxY) maxY = y1;
                    }
                }
            }
        }

        return {rocks: rocks, maxY: maxY};
    }

    /**
     * Simulate one unit of sand falling
     * Returns resting position as struct {x, y}, or empty struct if sand falls into abyss
     * Parameters:
     *   blocked: struct (set) of blocked positions
     *   maxY: maximum y of rocks
     *   floor: whether there is an infinite floor at maxY + 2
     */
    function simulateSand(required struct blocked, required numeric maxY, required boolean floor) {
        var x = 500;
        var y = 0;

        while (true) {
            // Check if sand has fallen below all rocks (into abyss) - Part 1 only
            if (!arguments.floor && y > arguments.maxY) {
                return {};
            }

            // Try to move down
            if (arguments.floor && y + 1 == arguments.maxY + 2) {
                // Hit the floor
                return {x: x, y: y};
            } else if (!structKeyExists(arguments.blocked, x & "," & (y + 1))) {
                y = y + 1;
            }
            // Try to move down-left
            else if (!structKeyExists(arguments.blocked, (x - 1) & "," & (y + 1))) {
                x = x - 1;
                y = y + 1;
            }
            // Try to move down-right
            else if (!structKeyExists(arguments.blocked, (x + 1) & "," & (y + 1))) {
                x = x + 1;
                y = y + 1;
            }
            // Sand comes to rest
            else {
                return {x: x, y: y};
            }
        }
    }

    /**
     * Part 1: Count sand units that come to rest before sand falls into abyss
     */
    function part1(required string input) {
        var parsed = parsePaths(arguments.input);
        var blocked = duplicate(parsed.rocks);
        var maxY = parsed.maxY;
        var count = 0;

        while (true) {
            var pos = simulateSand(blocked, maxY, false);
            if (structIsEmpty(pos)) {
                break;
            }
            blocked[pos.x & "," & pos.y] = true;
            count++;
        }

        return count;
    }

    /**
     * Part 2: Count sand units until source (500,0) is blocked (with floor)
     */
    function part2(required string input) {
        var parsed = parsePaths(arguments.input);
        var blocked = duplicate(parsed.rocks);
        var maxY = parsed.maxY;
        var count = 0;

        while (true) {
            var pos = simulateSand(blocked, maxY, true);
            blocked[pos.x & "," & pos.y] = true;
            count++;
            if (pos.x == 500 && pos.y == 0) {
                break;
            }
        }

        return count;
    }

}
