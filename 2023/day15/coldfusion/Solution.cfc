component {

    function run() {
        // Read input file
        var inputPath = getCurrentTemplatePath();
        var inputDir = getDirectoryFromPath(inputPath);
        var inputFile = inputDir & "../input.txt";
        var inputText = fileRead(inputFile).trim();

        // Remove any newlines and split on commas
        inputText = replace(inputText, chr(10), "", "all");
        inputText = replace(inputText, chr(13), "", "all");
        var steps = listToArray(inputText, ",");

        // Solve both parts
        var part1Result = part1(steps);
        var part2Result = part2(steps);

        writeOutput("Part 1: " & part1Result & chr(10));
        writeOutput("Part 2: " & part2Result & chr(10));
    }

    function hashAlgorithm(s) {
        /**
         * HASH algorithm: for each character,
         * current = ((current + ASCII) * 17) % 256
         */
        var current = 0;
        for (var i = 1; i <= len(s); i++) {
            var c = mid(s, i, 1);
            var asciiVal = asc(c);
            current = ((current + asciiVal) * 17) mod 256;
        }
        return current;
    }

    function part1(steps) {
        /**
         * Sum of HASH values for all comma-separated steps
         */
        var total = 0;
        for (var step in steps) {
            total += hashAlgorithm(step);
        }
        return total;
    }

    function part2(steps) {
        /**
         * HASHMAP procedure:
         * - 256 boxes with ordered lens lists
         * - `label-`: remove lens from box
         * - `label=N`: add/replace lens
         * - Focusing power = sum of (box+1) * slot * focal
         */

        // Initialize 256 boxes as arrays (0-indexed internally but 1-indexed in CF)
        var boxes = [];
        for (var i = 1; i <= 256; i++) {
            boxes[i] = [];
        }

        for (var step in steps) {
            if (find("=", step) > 0) {
                // Add/replace lens operation
                var parts = listToArray(step, "=");
                var label = parts[1];
                var focal = val(parts[2]);
                var boxNum = hashAlgorithm(label) + 1; // +1 for CF 1-based arrays

                // Check if lens with this label already exists
                var found = false;
                for (var j = 1; j <= arrayLen(boxes[boxNum]); j++) {
                    if (boxes[boxNum][j].label == label) {
                        boxes[boxNum][j].focal = focal;
                        found = true;
                        break;
                    }
                }

                // If not found, add to end
                if (!found) {
                    arrayAppend(boxes[boxNum], {label: label, focal: focal});
                }
            } else {
                // Remove lens operation (ends with -)
                var label = left(step, len(step) - 1);
                var boxNum = hashAlgorithm(label) + 1; // +1 for CF 1-based arrays

                // Remove lens with this label if it exists
                var newBox = [];
                for (var lens in boxes[boxNum]) {
                    if (lens.label != label) {
                        arrayAppend(newBox, lens);
                    }
                }
                boxes[boxNum] = newBox;
            }
        }

        // Calculate focusing power
        var total = 0;
        for (var boxIdx = 1; boxIdx <= 256; boxIdx++) {
            var box = boxes[boxIdx];
            for (var slot = 1; slot <= arrayLen(box); slot++) {
                // boxIdx is already 1-based, but we need 0-based box number + 1
                // boxIdx 1 = box 0, so focusing power uses boxIdx directly
                total += boxIdx * slot * box[slot].focal;
            }
        }

        return total;
    }
}
