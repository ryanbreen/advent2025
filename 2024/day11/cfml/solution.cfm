<cfscript>
// Read input file
inputText = fileRead("../input.txt").trim();

// Parse space-separated numbers
stones = listToArray(inputText, " ");

// Memoization cache - struct with "value,blinks" as keys
cache = {};

/**
 * Count how many stones result from a single stone after N blinks.
 * Uses memoization to avoid recalculating the same (value, blinks) pairs.
 */
function countStones(required numeric value, required numeric blinks) {
    // Base case
    if (arguments.blinks == 0) {
        return 1;
    }

    // Check cache
    local.cacheKey = arguments.value & "," & arguments.blinks;
    if (structKeyExists(cache, local.cacheKey)) {
        return cache[local.cacheKey];
    }

    local.result = 0;

    // Rule 1: 0 becomes 1
    if (arguments.value == 0) {
        local.result = countStones(1, arguments.blinks - 1);
    }
    // Rule 2: Even number of digits -> split
    else {
        local.valueStr = toString(arguments.value);
        local.digitCount = len(local.valueStr);

        if (local.digitCount % 2 == 0) {
            local.mid = local.digitCount / 2;
            local.leftStr = left(local.valueStr, local.mid);
            local.rightStr = right(local.valueStr, local.mid);

            // Convert to numbers (this removes leading zeros)
            local.leftNum = int(local.leftStr);
            local.rightNum = int(local.rightStr);

            local.result = countStones(local.leftNum, arguments.blinks - 1) + countStones(local.rightNum, arguments.blinks - 1);
        }
        // Rule 3: Multiply by 2024
        else {
            local.newValue = arguments.value * 2024;
            local.result = countStones(local.newValue, arguments.blinks - 1);
        }
    }

    // Cache the result
    cache[local.cacheKey] = local.result;

    return local.result;
}

function part1() {
    local.total = 0;
    for (local.stone in stones) {
        local.stoneNum = int(local.stone);
        local.total += countStones(local.stoneNum, 25);
    }
    return local.total;
}

function part2() {
    local.total = 0;
    for (local.stone in stones) {
        local.stoneNum = int(local.stone);
        local.total += countStones(local.stoneNum, 75);
    }
    return local.total;
}

// Run both parts
writeOutput("Part 1: " & part1() & "
");
writeOutput("Part 2: " & part2() & "
");
</cfscript>
