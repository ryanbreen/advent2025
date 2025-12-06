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

    // Read and parse input file into lines
    inputText = fileRead(inputPath).trim();
    lines = listToArray(inputText, chr(10));

    /**
     * Part 1: Extract calibration values using only numeric digits
     *
     * For each line, find the first and last numeric digit (0-9),
     * combine them to form a two-digit number, and sum all values.
     *
     * @return {numeric} Sum of all calibration values
     */
    function part1() {
        return arrayReduce(lines, function(total, line) {
            var digits = [];
            var lineLen = len(line);

            // Extract all numeric digits from the line
            for (var i = 1; i <= lineLen; i++) {
                var char = mid(line, i, 1);
                // Ensure character is numeric and not whitespace
                if (isNumeric(char) && char != " ") {
                    arrayAppend(digits, char);
                }
            }

            // Combine first and last digit to form calibration value
            if (arrayLen(digits) > 0) {
                var firstDigit = arrayFirst(digits);
                var lastDigit = arrayLast(digits);
                return total + int(firstDigit & lastDigit);
            }

            return total;
        }, 0);
    }

    /**
     * Part 2: Extract calibration values including spelled-out digits
     *
     * For each line, find the first and last digit (numeric or spelled-out word),
     * combine them to form a two-digit number, and sum all values.
     * Spelled-out words: one, two, three, four, five, six, seven, eight, nine
     *
     * @return {numeric} Sum of all calibration values
     */
    function part2() {
        // Use ordered struct for deterministic iteration order
        var words = [
            "one": 1,
            "two": 2,
            "three": 3,
            "four": 4,
            "five": 5,
            "six": 6,
            "seven": 7,
            "eight": 8,
            "nine": 9
        ];

        return arrayReduce(lines, function(total, line) {
            var digits = [];
            var lineLen = len(line);

            // Scan through each position in the line
            for (var i = 1; i <= lineLen; i++) {
                var char = mid(line, i, 1);

                // Check if current character is a numeric digit
                if (isNumeric(char) && char != " ") {
                    arrayAppend(digits, int(char));
                } else {
                    // Check if a spelled-out word starts at this position
                    for (var word in words) {
                        var wordLen = len(word);
                        // Ensure we don't read beyond the end of the line
                        if (i + wordLen - 1 <= lineLen) {
                            var substring = mid(line, i, wordLen);
                            if (substring == word) {
                                arrayAppend(digits, words[word]);
                                break; // Found a match, move to next position
                            }
                        }
                    }
                }
            }

            // Combine first and last digit to form calibration value
            if (arrayLen(digits) > 0) {
                var firstDigit = arrayFirst(digits);
                var lastDigit = arrayLast(digits);
                return total + int(firstDigit & lastDigit);
            }

            return total;
        }, 0);
    }

    // Execute both parts and display results
    echo("Part 1: " & part1() & chr(10));
    echo("Part 2: " & part2() & chr(10));

} catch (any e) {
    // Graceful error handling with detailed error information
    echo("Error: " & e.message & chr(10));
    if (structKeyExists(e, "detail")) {
        echo("Detail: " & e.detail & chr(10));
    }
    abort;
}
</cfscript>
