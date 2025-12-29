<cfscript>
/**
 * Advent of Code 2023 - Day 12: Hot Springs
 * ColdFusion/CFScript Solution
 *
 * Uses memoized dynamic programming to count valid spring arrangements.
 * State: (position, group_index, current_run_length)
 */

// Read input file - get directory of this script
scriptDir = getDirectoryFromPath(getCurrentTemplatePath());
inputPath = scriptDir & "../input.txt";
fileContent = fileRead(inputPath);
lines = listToArray(fileContent, chr(10));

// Clean up carriage returns
lines.each(function(line, idx) {
    lines[idx] = replace(line, chr(13), "", "all");
});

/**
 * Recursive DP function with memoization.
 * Note: CFML uses 1-based indexing, so pos starts at 1.
 * @param pattern The spring pattern string
 * @param groups Array of group sizes
 * @param memo Memoization struct
 * @param patternLen Length of pattern (cached)
 * @param groupsLen Length of groups array (cached)
 * @param pos Current position in pattern (1-based)
 * @param groupIdx Current group index (1-based)
 * @param currentRun Current run length of damaged springs
 * @return Number of valid arrangements from this state
 */
function dp(pattern, groups, memo, patternLen, groupsLen, pos, groupIdx, currentRun) {
    // Create memoization key
    var key = "#pos#,#groupIdx#,#currentRun#";

    if (structKeyExists(memo, key)) {
        return memo[key];
    }

    // Base case: reached end of pattern
    if (pos > patternLen) {
        // Valid if we've matched all groups and no partial run
        if (groupIdx > groupsLen && currentRun == 0) {
            return 1;
        }
        // Or if we're on the last group and the run matches
        if (groupIdx == groupsLen && groups[groupIdx] == currentRun) {
            return 1;
        }
        return 0;
    }

    var result = 0;
    // Note: "##" is CFML escape for literal "#" character
    var char = mid(pattern, pos, 1);

    // Option 1: Place operational spring (.)
    if (char == "." || char == "?") {
        if (currentRun == 0) {
            // No active run, just move forward
            result += dp(pattern, groups, memo, patternLen, groupsLen, pos + 1, groupIdx, 0);
        } else if (groupIdx <= groupsLen && groups[groupIdx] == currentRun) {
            // End current run if it matches expected group size
            result += dp(pattern, groups, memo, patternLen, groupsLen, pos + 1, groupIdx + 1, 0);
        }
        // Otherwise invalid (run doesn't match group)
    }

    // Option 2: Place damaged spring (#)
    // Note: "##" is CFML escape for literal "#" character
    if (char == "##" || char == "?") {
        if (groupIdx <= groupsLen && currentRun < groups[groupIdx]) {
            // Can extend current run
            result += dp(pattern, groups, memo, patternLen, groupsLen, pos + 1, groupIdx, currentRun + 1);
        }
        // Otherwise invalid (exceeds group size or no more groups)
    }

    memo[key] = result;
    return result;
}

/**
 * Count arrangements using memoized DP.
 * @param pattern The spring pattern string
 * @param groups Array of contiguous group sizes
 * @return Number of valid arrangements
 */
function countArrangements(required string pattern, required array groups) {
    var memo = {};
    var patternLen = len(pattern);
    var groupsLen = arrayLen(groups);

    return dp(pattern, groups, memo, patternLen, groupsLen, 1, 1, 0);
}

/**
 * Parse a line into pattern and groups.
 * @param line Input line
 * @return Struct with pattern and groups
 */
function parseLine(required string line) {
    var parts = listToArray(trim(line), " ");
    var pattern = parts[1];
    var groups = listToArray(parts[2], ",").map(function(g) {
        return val(g);
    });

    return {pattern: pattern, groups: groups};
}

/**
 * Unfold pattern and groups by repeating them 5 times.
 * @param pattern The spring pattern
 * @param groups Array of group sizes
 * @return Struct with unfolded pattern and groups
 */
function unfold(required string pattern, required array groups) {
    var unfoldedPattern = pattern;
    for (var i = 2; i <= 5; i++) {
        unfoldedPattern = unfoldedPattern & "?" & pattern;
    }

    var unfoldedGroups = [];
    for (var i = 1; i <= 5; i++) {
        for (var g in groups) {
            arrayAppend(unfoldedGroups, g);
        }
    }

    return {pattern: unfoldedPattern, groups: unfoldedGroups};
}

/**
 * Part 1: Sum of arrangement counts for all rows.
 * @param lines Array of input lines
 * @return Total count
 */
function part1(lines) {
    var total = 0;

    for (var line in lines) {
        if (len(trim(line)) == 0) continue;
        var parsed = parseLine(line);
        total += countArrangements(parsed.pattern, parsed.groups);
    }

    return total;
}

/**
 * Part 2: Sum of arrangement counts for all rows after unfolding.
 * @param lines Array of input lines
 * @return Total count
 */
function part2(lines) {
    var total = 0;

    for (var line in lines) {
        if (len(trim(line)) == 0) continue;
        var parsed = parseLine(line);
        var unfolded = unfold(parsed.pattern, parsed.groups);
        total += countArrangements(unfolded.pattern, unfolded.groups);
    }

    return total;
}

// Run the solution
writeOutput("Part 1: " & part1(lines) & chr(10));
writeOutput("Part 2: " & part2(lines) & chr(10));
</cfscript>
