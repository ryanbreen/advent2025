component {

    /**
     * Parse input file and return array of rucksack contents
     */
    public array function parseInput(required string filepath) {
        var content = fileRead(arguments.filepath);
        var lines = listToArray(content, chr(10));
        var rucksacks = [];

        for (var line in lines) {
            line = trim(line);
            if (len(line) > 0) {
                arrayAppend(rucksacks, line);
            }
        }

        return rucksacks;
    }

    /**
     * Calculate priority of a character
     * a-z = 1-26, A-Z = 27-52
     */
    public numeric function priority(required string char) {
        var code = asc(arguments.char);

        if (code >= 97 && code <= 122) {
            // lowercase a-z
            return code - 96;
        } else {
            // uppercase A-Z
            return code - 64 + 26;
        }
    }

    /**
     * Convert a string to a case-sensitive Java LinkedHashMap (simulating a set)
     */
    public any function stringToSet(required string str) {
        var result = createObject("java", "java.util.LinkedHashMap").init();
        var length = len(arguments.str);

        for (var i = 1; i <= length; i++) {
            var char = mid(arguments.str, i, 1);
            result.put(char, true);
        }

        return result;
    }

    /**
     * Find intersection of two sets (Java HashMaps)
     */
    public any function intersect(required any set1, required any set2) {
        var result = createObject("java", "java.util.LinkedHashMap").init();
        var keys = arguments.set1.keySet().iterator();

        while (keys.hasNext()) {
            var key = keys.next();
            if (arguments.set2.containsKey(key)) {
                result.put(key, true);
            }
        }

        return result;
    }

    /**
     * Get first key from a Java HashMap
     */
    public string function getFirstKey(required any s) {
        var keys = arguments.s.keySet().iterator();
        return keys.next();
    }

    /**
     * Part 1: Find common item in each rucksack's two compartments
     */
    public numeric function part1(required array rucksacks) {
        var total = 0;

        for (var rucksack in arguments.rucksacks) {
            var length = len(rucksack);
            var midpoint = length / 2;
            var firstHalf = left(rucksack, midpoint);
            var secondHalf = right(rucksack, midpoint);

            var firstSet = stringToSet(firstHalf);
            var secondSet = stringToSet(secondHalf);
            var common = intersect(firstSet, secondSet);

            var commonChar = getFirstKey(common);
            total += priority(commonChar);
        }

        return total;
    }

    /**
     * Part 2: Find common item among groups of 3 rucksacks
     */
    public numeric function part2(required array rucksacks) {
        var total = 0;
        var count = arrayLen(arguments.rucksacks);

        for (var i = 1; i <= count; i += 3) {
            var set1 = stringToSet(arguments.rucksacks[i]);
            var set2 = stringToSet(arguments.rucksacks[i + 1]);
            var set3 = stringToSet(arguments.rucksacks[i + 2]);

            var common = intersect(intersect(set1, set2), set3);
            var commonChar = getFirstKey(common);
            total += priority(commonChar);
        }

        return total;
    }

    /**
     * Main entry point
     */
    public void function main() {
        var scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
        var inputFile = scriptDir & "../input.txt";

        var rucksacks = parseInput(inputFile);

        writeOutput("Part 1: " & part1(rucksacks) & chr(10));
        writeOutput("Part 2: " & part2(rucksacks) & chr(10));
    }

}
