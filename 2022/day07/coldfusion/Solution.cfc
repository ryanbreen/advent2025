component {

    function run() {
        // Read input file
        var inputPath = getCurrentTemplatePath();
        var inputDir = getDirectoryFromPath(inputPath);
        var inputFile = inputDir & "../input.txt";
        var inputText = fileRead(inputFile).trim();

        // Parse filesystem from terminal output
        var dirSizes = parseFilesystem(inputText);

        // Solve both parts
        var part1Result = part1(dirSizes);
        var part2Result = part2(dirSizes);

        writeOutput("Part 1: " & part1Result & chr(10));
        writeOutput("Part 2: " & part2Result & chr(10));
    }

    /**
     * Parse terminal output and return directory sizes
     */
    function parseFilesystem(required string input) {
        var lines = listToArray(arguments.input, chr(10));
        var path = [];
        var dirSizes = {};

        for (var line in lines) {
            line = trim(line);

            if (left(line, 4) == "$ cd") {
                var target = mid(line, 6, len(line) - 5);

                if (target == "/") {
                    path = ["/"];
                } else if (target == "..") {
                    if (arrayLen(path) > 1) {
                        arrayDeleteAt(path, arrayLen(path));
                    }
                } else {
                    arrayAppend(path, target);
                }
            } else if (left(line, 4) == "$ ls") {
                // Skip ls command
                continue;
            } else if (left(line, 4) == "dir ") {
                // Skip directory listings
                continue;
            } else if (len(line) > 0) {
                // It's a file with size
                var parts = listToArray(line, " ");
                if (arrayLen(parts) >= 2 && isNumeric(parts[1])) {
                    var fileSize = val(parts[1]);

                    // Add size to current directory and all parent directories
                    for (var i = 1; i <= arrayLen(path); i++) {
                        var dirPath = buildPath(path, i);
                        if (!structKeyExists(dirSizes, dirPath)) {
                            dirSizes[dirPath] = 0;
                        }
                        dirSizes[dirPath] += fileSize;
                    }
                }
            }
        }

        return dirSizes;
    }

    /**
     * Build a path string from path array up to index i
     */
    private function buildPath(required array pathArray, required numeric upToIndex) {
        if (arguments.upToIndex == 1) {
            return "/";
        }

        var result = "";
        for (var j = 1; j <= arguments.upToIndex; j++) {
            if (j == 1) {
                result = "/";
            } else {
                result = result & "/" & arguments.pathArray[j];
            }
        }

        return result;
    }

    /**
     * Part 1: Sum of sizes of directories with total size <= 100000
     */
    function part1(required struct dirSizes) {
        var total = 0;

        for (var dirPath in arguments.dirSizes) {
            var size = arguments.dirSizes[dirPath];
            if (size <= 100000) {
                total += size;
            }
        }

        return total;
    }

    /**
     * Part 2: Find smallest directory to delete to free enough space
     */
    function part2(required struct dirSizes) {
        var totalSpace = 70000000;
        var neededSpace = 30000000;
        var usedSpace = arguments.dirSizes["/"];
        var freeSpace = totalSpace - usedSpace;
        var needToFree = neededSpace - freeSpace;

        // Find smallest directory >= needToFree
        var smallest = usedSpace; // Start with root size as max possible

        for (var dirPath in arguments.dirSizes) {
            var size = arguments.dirSizes[dirPath];
            if (size >= needToFree && size < smallest) {
                smallest = size;
            }
        }

        return smallest;
    }

}
