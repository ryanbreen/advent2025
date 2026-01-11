component {

    /**
     * Parse input file and return array of binary strings
     */
    public array function parseInput(required string inputPath) {
        var content = fileRead(arguments.inputPath);
        var lines = listToArray(content, chr(10) & chr(13));
        var result = [];

        for (var line in lines) {
            var trimmed = trim(line);
            if (len(trimmed) > 0) {
                arrayAppend(result, trimmed);
            }
        }

        return result;
    }

    /**
     * Part 1: Calculate power consumption (gamma * epsilon)
     */
    public numeric function part1(required array numbers) {
        var numBits = len(numbers[1]);
        var gamma = 0;

        for (var pos = 1; pos <= numBits; pos++) {
            var ones = countOnesAtPosition(numbers, pos);
            var zeros = arrayLen(numbers) - ones;

            if (ones >= zeros) {
                // Set bit at position (numBits - pos) from right
                gamma = bitOr(gamma, 2 ^ (numBits - pos));
            }
        }

        // epsilon is bitwise NOT of gamma within numBits
        var mask = (2 ^ numBits) - 1;
        var epsilon = bitXor(gamma, mask);

        return gamma * epsilon;
    }

    /**
     * Count how many numbers have a '1' at the given position
     */
    private numeric function countOnesAtPosition(required array numbers, required numeric pos) {
        var count = 0;
        for (var n in numbers) {
            if (mid(n, pos, 1) == "1") {
                count++;
            }
        }
        return count;
    }

    /**
     * Find rating by filtering based on bit criteria
     */
    private numeric function findRating(required array numbers, required boolean useMostCommon) {
        var numBits = len(numbers[1]);
        var candidates = duplicate(numbers);

        for (var pos = 1; pos <= numBits; pos++) {
            if (arrayLen(candidates) == 1) {
                break;
            }

            var ones = countOnesAtPosition(candidates, pos);
            var zeros = arrayLen(candidates) - ones;

            var target = "";
            if (useMostCommon) {
                target = (ones >= zeros) ? "1" : "0";
            } else {
                target = (zeros <= ones) ? "0" : "1";
            }

            var filtered = [];
            for (var n in candidates) {
                if (mid(n, pos, 1) == target) {
                    arrayAppend(filtered, n);
                }
            }
            candidates = filtered;
        }

        return binaryToDecimal(candidates[1]);
    }

    /**
     * Convert binary string to decimal
     */
    private numeric function binaryToDecimal(required string binary) {
        var result = 0;
        var length = len(binary);
        for (var i = 1; i <= length; i++) {
            if (mid(binary, i, 1) == "1") {
                result += 2 ^ (length - i);
            }
        }
        return result;
    }

    /**
     * Part 2: Calculate life support rating (oxygen * CO2)
     */
    public numeric function part2(required array numbers) {
        var oxygen = findRating(numbers, true);
        var co2 = findRating(numbers, false);
        return oxygen * co2;
    }

}
