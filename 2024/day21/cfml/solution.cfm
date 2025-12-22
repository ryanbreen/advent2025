<cfscript>
/**
 * Day 21: Keypad Conundrum - Robot chain control with shortest path optimization
 */

// Keypad layouts - positions as [row, col]
NUMERIC = {
    '7': [0, 0], '8': [0, 1], '9': [0, 2],
    '4': [1, 0], '5': [1, 1], '6': [1, 2],
    '1': [2, 0], '2': [2, 1], '3': [2, 2],
    '0': [3, 1], 'A': [3, 2]
};
NUMERIC_GAP = [3, 0];

DIRECTIONAL = {
    '^': [0, 1], 'A': [0, 2],
    '<': [1, 0], 'v': [1, 1], '>': [1, 2]
};
DIRECTIONAL_GAP = [0, 0];

// Memoization cache
memo = {};

/**
 * DFS helper for finding paths - recursive function
 */
function dfsHelper(r, c, path, er, ec, gapR, gapC, paths) {
    // Check if we hit the gap
    if (r == gapR && c == gapC) {
        return paths;
    }
    // Check if we reached target
    if (r == er && c == ec) {
        arrayAppend(paths, path);
        return paths;
    }
    // Move vertically toward target
    if (r < er) {
        dfsHelper(r + 1, c, path & 'v', er, ec, gapR, gapC, paths);
    } else if (r > er) {
        dfsHelper(r - 1, c, path & '^', er, ec, gapR, gapC, paths);
    }
    // Move horizontally toward target
    if (c < ec) {
        dfsHelper(r, c + 1, path & '>', er, ec, gapR, gapC, paths);
    } else if (c > ec) {
        dfsHelper(r, c - 1, path & '<', er, ec, gapR, gapC, paths);
    }
    return paths;
}

/**
 * Find all shortest paths from start to end, avoiding gap
 */
function shortestPaths(keypad, gap, start, end) {
    var startPos = keypad[start];
    var endPos = keypad[end];
    var sr = startPos[1];
    var sc = startPos[2];
    var er = endPos[1];
    var ec = endPos[2];
    var gapR = gap[1];
    var gapC = gap[2];
    var paths = [];

    paths = dfsHelper(sr, sc, '', er, ec, gapR, gapC, paths);

    // Return empty path if start == end
    if (arrayLen(paths) == 0) {
        return [''];
    }
    return paths;
}

/**
 * Minimum presses needed to move from fromChar to toChar and press, at given depth
 */
function minPressesForMove(fromChar, toChar, depth, isNumeric) {
    // Create memoization key
    var cacheKey = fromChar & '|' & toChar & '|' & depth & '|' & isNumeric;
    if (structKeyExists(memo, cacheKey)) {
        return memo[cacheKey];
    }

    var keypad = isNumeric ? NUMERIC : DIRECTIONAL;
    var gap = isNumeric ? NUMERIC_GAP : DIRECTIONAL_GAP;

    var paths = shortestPaths(keypad, gap, fromChar, toChar);

    if (depth == 0) {
        // At human level, just return path length + 1 for 'A' press
        var minLen = 999999;
        for (var p in paths) {
            if (len(p) < minLen) {
                minLen = len(p);
            }
        }
        var result = minLen + 1;
        memo[cacheKey] = result;
        return result;
    }

    var best = 999999999999;
    for (var path in paths) {
        // Need to type path + 'A' on the directional keypad above
        var sequence = path & 'A';
        var cost = 0;
        var current = 'A';

        for (var i = 1; i <= len(sequence); i++) {
            var char = mid(sequence, i, 1);
            cost += minPressesForMove(current, char, depth - 1, false);
            current = char;
        }

        if (cost < best) {
            best = cost;
        }
    }

    memo[cacheKey] = best;
    return best;
}

/**
 * Compute minimum presses to type code on numeric keypad with given robot depth
 */
function solveCode(code, depth) {
    var total = 0;
    var current = 'A';

    for (var i = 1; i <= len(code); i++) {
        var char = mid(code, i, 1);
        total += minPressesForMove(current, char, depth, true);
        current = char;
    }

    return total;
}

/**
 * Compute complexity: length * numeric part of code
 */
function complexity(code, length) {
    // Extract numeric part (remove trailing 'A')
    var numericPart = val(replace(code, 'A', '', 'all'));
    return length * numericPart;
}

/**
 * Part 1: 2 intermediate robots (depth = 2)
 */
function part1(codes) {
    var total = 0;
    for (var code in codes) {
        var length = solveCode(code, 2);
        total += complexity(code, length);
    }
    return total;
}

/**
 * Part 2: 25 intermediate robots (depth = 25)
 */
function part2(codes) {
    var total = 0;
    for (var code in codes) {
        var length = solveCode(code, 25);
        total += complexity(code, length);
    }
    return total;
}

// Main execution
inputPath = getDirectoryFromPath(getCurrentTemplatePath()) & "../input.txt";
inputText = fileRead(inputPath);
codes = listToArray(inputText, chr(10));

// Filter empty lines
filteredCodes = [];
for (var code in codes) {
    code = trim(code);
    if (len(code) > 0) {
        arrayAppend(filteredCodes, code);
    }
}

writeOutput('Part 1: ' & part1(filteredCodes) & chr(10));
writeOutput('Part 2: ' & part2(filteredCodes) & chr(10));
</cfscript>
