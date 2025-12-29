<cfscript>
try {
    // Locate and validate input file
    currentDir = getDirectoryFromPath(getCurrentTemplatePath());
    inputPath = currentDir & "../input.txt";

    // Validate file exists before attempting to read
    if (!fileExists(inputPath)) {
        echo("Error: Input file not found at " & inputPath & chr(10));
        abort;
    }

    // Read input file
    inputText = fileRead(inputPath).trim();

    /**
     * Parse input lines into arrays of integers
     * Each line contains space-separated integers
     *
     * @param {array} lines - Array of input lines
     * @return {array} Array of arrays of integers (histories)
     */
    function parseInput(lines) {
        var histories = [];
        for (var line in lines) {
            var nums = [];
            var parts = listToArray(line, " ");
            for (var part in parts) {
                if (len(trim(part)) > 0) {
                    arrayAppend(nums, val(part));
                }
            }
            if (arrayLen(nums) > 0) {
                arrayAppend(histories, nums);
            }
        }
        return histories;
    }

    /**
     * Compute differences between consecutive elements
     *
     * @param {array} seq - Array of numbers
     * @return {array} Array of differences (length = input length - 1)
     */
    function getDifferences(seq) {
        var result = [];
        var n = arrayLen(seq);
        for (var i = 1; i < n; i++) {
            arrayAppend(result, seq[i + 1] - seq[i]);
        }
        return result;
    }

    /**
     * Check if all elements in array are zero
     *
     * @param {array} seq - Array of numbers
     * @return {boolean} True if all elements are zero
     */
    function allZeros(seq) {
        for (var x in seq) {
            if (x != 0) {
                return false;
            }
        }
        return true;
    }

    /**
     * Create a reversed copy of an array
     *
     * @param {array} arr - Array to reverse
     * @return {array} New array with elements in reverse order
     */
    function reverseArray(arr) {
        var result = [];
        var n = arrayLen(arr);
        for (var i = n; i >= 1; i--) {
            arrayAppend(result, arr[i]);
        }
        return result;
    }

    /**
     * Extrapolate the next value in the sequence
     * Build difference pyramid until all zeros, then sum the last values
     *
     * Key insight: The extrapolated next value is simply the sum of all
     * last values in the difference pyramid
     *
     * @param {array} seq - Original sequence
     * @return {numeric} The extrapolated next value
     */
    function extrapolateNext(seq) {
        var current = seq;
        var sum = 0;

        // Keep computing differences until we hit all zeros
        // Accumulate the last value at each level
        while (!allZeros(current)) {
            sum += current[arrayLen(current)];
            current = getDifferences(current);
        }

        return sum;
    }

    /**
     * Part 1: Sum of all extrapolated next values
     *
     * @param {array} histories - Parsed input histories
     * @return {numeric} Sum of extrapolated next values
     */
    function part1(histories) {
        var total = 0;
        for (var h in histories) {
            total += extrapolateNext(h);
        }
        return total;
    }

    /**
     * Part 2: Sum of all extrapolated previous values
     *
     * Key insight: Extrapolating backwards is equivalent to
     * extrapolating forwards on the reversed sequence
     *
     * @param {array} histories - Parsed input histories
     * @return {numeric} Sum of extrapolated previous values
     */
    function part2(histories) {
        var total = 0;
        for (var h in histories) {
            // Reverse the sequence and extrapolate forward
            total += extrapolateNext(reverseArray(h));
        }
        return total;
    }

    // Parse input once and reuse
    lines = listToArray(inputText, chr(10));
    histories = parseInput(lines);

    // Execute both parts and display results
    echo("Part 1: " & part1(histories) & chr(10));
    echo("Part 2: " & part2(histories) & chr(10));

} catch (any e) {
    // Graceful error handling with detailed error information
    echo("Error: " & e.message & chr(10));
    if (structKeyExists(e, "detail")) {
        echo("Detail: " & e.detail & chr(10));
    }
    abort;
}
</cfscript>
