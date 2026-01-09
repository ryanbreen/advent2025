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
     * Parse sensor and beacon positions from input
     * Returns array of structs with sx, sy, bx, by, dist (Manhattan distance)
     */
    function parseSensors(required string input) {
        var sensors = [];
        var lines = listToArray(arguments.input, chr(10));
        var pattern = "Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)";

        for (var line in lines) {
            line = trim(line);
            if (len(line) == 0) continue;

            var match = reFind(pattern, line, 1, true);
            if (arrayLen(match.pos) >= 5 && match.pos[1] > 0) {
                var sx = val(mid(line, match.pos[2], match.len[2]));
                var sy = val(mid(line, match.pos[3], match.len[3]));
                var bx = val(mid(line, match.pos[4], match.len[4]));
                var by = val(mid(line, match.pos[5], match.len[5]));

                // Manhattan distance
                var dist = abs(sx - bx) + abs(sy - by);

                arrayAppend(sensors, {
                    sx: sx,
                    sy: sy,
                    bx: bx,
                    by: by,
                    dist: dist
                });
            }
        }

        return sensors;
    }

    /**
     * Get coverage ranges at a specific row from all sensors
     * Returns array of [start, end] ranges
     */
    function getCoverageAtRow(required array sensors, required numeric row) {
        var ranges = [];

        for (var sensor in arguments.sensors) {
            // How far is the row from the sensor?
            var rowDist = abs(sensor.sy - arguments.row);

            // If row is too far, sensor doesn't cover this row
            if (rowDist > sensor.dist) continue;

            // Calculate x-range covered at this row
            var xSpread = sensor.dist - rowDist;
            arrayAppend(ranges, [sensor.sx - xSpread, sensor.sx + xSpread]);
        }

        return mergeRanges(ranges);
    }

    /**
     * Merge overlapping ranges
     * Input: array of [start, end] arrays
     * Output: array of merged [start, end] arrays
     */
    function mergeRanges(required array ranges) {
        if (arrayLen(arguments.ranges) == 0) {
            return [];
        }

        // Sort ranges by start value
        arraySort(arguments.ranges, function(a, b) {
            return a[1] - b[1];
        });

        var merged = [arguments.ranges[1]];

        for (var i = 2; i <= arrayLen(arguments.ranges); i++) {
            var current = arguments.ranges[i];
            var last = merged[arrayLen(merged)];

            // Check if current range overlaps with or is adjacent to the last merged range
            if (current[1] <= last[2] + 1) {
                // Extend the last range if needed
                last[2] = max(last[2], current[2]);
            } else {
                // No overlap, add as new range
                arrayAppend(merged, current);
            }
        }

        return merged;
    }

    /**
     * Part 1: Count positions that cannot contain a beacon at row y=2000000
     */
    function part1(required string input) {
        var sensors = parseSensors(arguments.input);
        var targetRow = 2000000;

        var ranges = getCoverageAtRow(sensors, targetRow);

        // Count total coverage
        var total = 0;
        for (var range in ranges) {
            total += range[2] - range[1] + 1;
        }

        // Subtract beacons that are on this row (use struct as set)
        var beaconsOnRow = {};
        for (var sensor in sensors) {
            if (sensor.by == targetRow) {
                beaconsOnRow[sensor.bx] = true;
            }
        }

        return total - structCount(beaconsOnRow);
    }

    /**
     * Part 2: Find the distress beacon's tuning frequency
     * The beacon is at the one uncovered position in 0-4000000 range
     */
    function part2(required string input) {
        var sensors = parseSensors(arguments.input);
        var maxCoord = 4000000;

        // For each row, find if there's a gap in coverage
        for (var row = 0; row <= maxCoord; row++) {
            var ranges = getCoverageAtRow(sensors, row);

            // Clip ranges to search area
            var clipped = [];
            for (var range in ranges) {
                if (range[2] < 0 || range[1] > maxCoord) continue;
                arrayAppend(clipped, [max(0, range[1]), min(maxCoord, range[2])]);
            }

            clipped = mergeRanges(clipped);

            // Check if full row is covered
            if (arrayLen(clipped) == 1 && clipped[1][1] == 0 && clipped[1][2] == maxCoord) {
                continue;
            }

            // Found a gap - the beacon is in the gap
            var x = 0;
            if (arrayLen(clipped) > 1) {
                // Gap between ranges
                x = clipped[1][2] + 1;
            } else if (clipped[1][1] > 0) {
                x = 0;
            } else {
                x = clipped[1][2] + 1;
            }

            // Return tuning frequency: x * 4000000 + y
            // Use precisionEvaluate for large number arithmetic
            return precisionEvaluate(x * 4000000 + row);
        }

        return 0;
    }

}
