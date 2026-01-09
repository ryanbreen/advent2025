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

        writeOutput("Part 1: " & part1Result & chr(10));
        writeOutput("Part 2: " & part2Result & chr(10));
    }

    /**
     * Find first position where last windowSize characters are all unique
     */
    function findMarker(required string data, required numeric windowSize) {
        var dataLen = len(arguments.data);

        for (var i = arguments.windowSize; i <= dataLen; i++) {
            var window = mid(arguments.data, i - arguments.windowSize + 1, arguments.windowSize);

            if (allUnique(window)) {
                return i;
            }
        }

        return -1;
    }

    /**
     * Check if all characters in a string are unique
     */
    private boolean function allUnique(required string str) {
        var seen = {};
        var strLen = len(arguments.str);

        for (var i = 1; i <= strLen; i++) {
            var ch = mid(arguments.str, i, 1);
            if (structKeyExists(seen, ch)) {
                return false;
            }
            seen[ch] = true;
        }

        return true;
    }

    /**
     * Part 1: Find start-of-packet marker (4 unique characters)
     */
    function part1(required string data) {
        return findMarker(arguments.data, 4);
    }

    /**
     * Part 2: Find start-of-message marker (14 unique characters)
     */
    function part2(required string data) {
        return findMarker(arguments.data, 14);
    }

}
