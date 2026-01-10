component {

    /**
     * Parse the input file and return an array of depth measurements
     */
    public array function parseInput(required string inputPath) {
        var content = fileRead(arguments.inputPath);
        var lines = listToArray(trim(content), chr(10));
        var depths = [];

        for (var line in lines) {
            if (len(trim(line)) > 0) {
                arrayAppend(depths, val(trim(line)));
            }
        }

        return depths;
    }

    /**
     * Part 1: Count the number of times a depth measurement increases from the previous
     */
    public numeric function part1(required array depths) {
        var count = 0;

        for (var i = 2; i <= arrayLen(arguments.depths); i++) {
            if (arguments.depths[i] > arguments.depths[i - 1]) {
                count++;
            }
        }

        return count;
    }

    /**
     * Part 2: Count increases in 3-measurement sliding window sums
     */
    public numeric function part2(required array depths) {
        var windowSums = [];

        // Create sliding window sums of 3 consecutive measurements
        for (var i = 1; i <= arrayLen(arguments.depths) - 2; i++) {
            arrayAppend(windowSums, arguments.depths[i] + arguments.depths[i + 1] + arguments.depths[i + 2]);
        }

        // Count how many times the sum increases
        var count = 0;
        for (var i = 2; i <= arrayLen(windowSums); i++) {
            if (windowSums[i] > windowSums[i - 1]) {
                count++;
            }
        }

        return count;
    }

}
