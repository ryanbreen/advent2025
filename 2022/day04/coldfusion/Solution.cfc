component {

    /**
     * Parse input file into array of range pairs
     * Each element is a struct with a1, b1 (first range) and a2, b2 (second range)
     */
    public array function parseInput(required string filepath) {
        var pairs = [];
        var content = fileRead(arguments.filepath);
        var lines = listToArray(content, chr(10));

        for (var line in lines) {
            line = trim(line);
            if (len(line) == 0) continue;

            var parts = listToArray(line, ",");
            var left = listToArray(parts[1], "-");
            var right = listToArray(parts[2], "-");

            arrayAppend(pairs, {
                a1: val(left[1]),
                b1: val(left[2]),
                a2: val(right[1]),
                b2: val(right[2])
            });
        }

        return pairs;
    }

    /**
     * Check if one range fully contains the other
     * Range 1 contains Range 2: a1 <= a2 AND b1 >= b2
     * Range 2 contains Range 1: a2 <= a1 AND b2 >= b1
     */
    public boolean function fullyContains(
        required numeric a1,
        required numeric b1,
        required numeric a2,
        required numeric b2
    ) {
        return (arguments.a1 <= arguments.a2 && arguments.b1 >= arguments.b2) ||
               (arguments.a2 <= arguments.a1 && arguments.b2 >= arguments.b1);
    }

    /**
     * Check if ranges overlap at all
     * Ranges overlap if: a1 <= b2 AND a2 <= b1
     */
    public boolean function overlaps(
        required numeric a1,
        required numeric b1,
        required numeric a2,
        required numeric b2
    ) {
        return arguments.a1 <= arguments.b2 && arguments.a2 <= arguments.b1;
    }

    /**
     * Part 1: Count pairs where one range fully contains the other
     */
    public numeric function part1(required array pairs) {
        var count = 0;

        for (var pair in arguments.pairs) {
            if (fullyContains(pair.a1, pair.b1, pair.a2, pair.b2)) {
                count++;
            }
        }

        return count;
    }

    /**
     * Part 2: Count pairs where ranges overlap at all
     */
    public numeric function part2(required array pairs) {
        var count = 0;

        for (var pair in arguments.pairs) {
            if (overlaps(pair.a1, pair.b1, pair.a2, pair.b2)) {
                count++;
            }
        }

        return count;
    }

}
