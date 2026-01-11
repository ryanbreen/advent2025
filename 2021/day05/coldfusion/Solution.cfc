component {

    /**
     * Parse input file and return array of line segments
     * Each segment is a struct with x1, y1, x2, y2
     */
    public array function parseInput(required string inputPath) {
        var content = fileRead(arguments.inputPath);
        var lines = listToArray(content, chr(10));
        var segments = [];

        for (var line in lines) {
            line = trim(line);
            if (len(line) == 0) continue;

            // Parse "x1,y1 -> x2,y2"
            var parts = listToArray(line, " -> ");
            var startCoords = listToArray(parts[1], ",");
            var endCoords = listToArray(parts[2], ",");

            arrayAppend(segments, {
                "x1": int(trim(startCoords[1])),
                "y1": int(trim(startCoords[2])),
                "x2": int(trim(endCoords[1])),
                "y2": int(trim(endCoords[2]))
            });
        }

        return segments;
    }

    /**
     * Get the sign of a number (-1, 0, or 1)
     */
    private numeric function sign(required numeric x) {
        if (x > 0) return 1;
        if (x < 0) return -1;
        return 0;
    }

    /**
     * Count overlapping points on the grid
     * If includeDiagonals is false, skip diagonal lines
     */
    private numeric function countOverlaps(required array segments, required boolean includeDiagonals) {
        var grid = {};

        for (var seg in segments) {
            var dx = sign(seg.x2 - seg.x1);
            var dy = sign(seg.y2 - seg.y1);

            // Skip diagonals in Part 1
            if (!includeDiagonals && dx != 0 && dy != 0) {
                continue;
            }

            var x = seg.x1;
            var y = seg.y1;

            while (true) {
                var key = x & "," & y;
                if (!structKeyExists(grid, key)) {
                    grid[key] = 0;
                }
                grid[key] = grid[key] + 1;

                if (x == seg.x2 && y == seg.y2) {
                    break;
                }
                x = x + dx;
                y = y + dy;
            }
        }

        // Count cells with 2 or more overlaps
        var count = 0;
        for (var key in grid) {
            if (grid[key] >= 2) {
                count = count + 1;
            }
        }

        return count;
    }

    /**
     * Part 1: Count overlaps considering only horizontal and vertical lines
     */
    public numeric function part1(required array segments) {
        return countOverlaps(segments, false);
    }

    /**
     * Part 2: Count overlaps considering all lines including diagonals
     */
    public numeric function part2(required array segments) {
        return countOverlaps(segments, true);
    }

}
