component {

    // Component-level cache for memoization
    variables.memoCache = {};
    variables.valves = {};
    variables.tunnels = {};
    variables.distances = {};
    variables.valuable = [];

    function run() {
        // Read input file
        var inputPath = getCurrentTemplatePath();
        var inputDir = getDirectoryFromPath(inputPath);
        var inputFile = inputDir & "../input.txt";
        var inputText = fileRead(inputFile).trim();

        // Solve both parts
        var part1Result = part1(inputText);
        var part2Result = part2(inputText);

        systemOutput("Part 1: " & part1Result, true);
        systemOutput("Part 2: " & part2Result, true);
    }

    /**
     * Parse valves and tunnels from input
     * Sets component variables: valves (struct of name -> flow rate), tunnels (struct of name -> array of neighbors)
     */
    function parseInput(required string input) {
        variables.valves = {};
        variables.tunnels = {};

        var lines = listToArray(arguments.input, chr(10));
        var pattern = "Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.+)";

        for (var line in lines) {
            line = trim(line);
            if (len(line) == 0) continue;

            var match = reFind(pattern, line, 1, true);
            if (arrayLen(match.pos) >= 4 && match.pos[1] > 0) {
                var name = mid(line, match.pos[2], match.len[2]);
                var rate = val(mid(line, match.pos[3], match.len[3]));
                var neighborsStr = mid(line, match.pos[4], match.len[4]);

                variables.valves[name] = rate;
                variables.tunnels[name] = listToArray(neighborsStr, ", ");
            }
        }
    }

    /**
     * Compute shortest distances between all relevant valves using BFS
     * Relevant valves are AA (start) plus all valves with positive flow
     */
    function computeDistances() {
        variables.distances = {};

        // Collect relevant valves: AA plus valves with flow > 0
        var relevant = ["AA"];
        for (var v in variables.valves) {
            if (variables.valves[v] > 0) {
                arrayAppend(relevant, v);
            }
        }

        // BFS from each relevant valve to all others
        for (var start in relevant) {
            variables.distances[start] = {};

            var queue = [{valve: start, dist: 0}];
            var visited = {};
            visited[start] = true;
            var qIdx = 1;

            while (qIdx <= arrayLen(queue)) {
                var curr = queue[qIdx];
                qIdx++;

                var currValve = curr.valve;
                var currDist = curr.dist;

                // If this is a relevant valve (other than start), record distance
                if (currValve != start && arrayFindNoCase(relevant, currValve) > 0) {
                    variables.distances[start][currValve] = currDist;
                }

                // Explore neighbors
                if (structKeyExists(variables.tunnels, currValve)) {
                    for (var neighbor in variables.tunnels[currValve]) {
                        if (!structKeyExists(visited, neighbor)) {
                            visited[neighbor] = true;
                            arrayAppend(queue, {valve: neighbor, dist: currDist + 1});
                        }
                    }
                }
            }
        }

        // Store valuable valves (with positive flow)
        variables.valuable = [];
        for (var v in variables.valves) {
            if (variables.valves[v] > 0) {
                arrayAppend(variables.valuable, v);
            }
        }
        arraySort(variables.valuable, "textnocase");
    }

    /**
     * Convert a set of valve names to a bitmask
     */
    function setToMask(required array valveSet) {
        var mask = 0;
        for (var v in arguments.valveSet) {
            var idx = arrayFindNoCase(variables.valuable, v);
            if (idx > 0) {
                mask = bitOr(mask, 2 ^ (idx - 1));
            }
        }
        return mask;
    }

    /**
     * Check if a valve is in the opened set (using bitmask)
     */
    function isValveOpen(required numeric mask, required string valve) {
        var idx = arrayFindNoCase(variables.valuable, valve);
        if (idx == 0) return false;
        return bitAnd(arguments.mask, 2 ^ (idx - 1)) > 0;
    }

    /**
     * Add a valve to the opened set (using bitmask)
     */
    function addValveToMask(required numeric mask, required string valve) {
        var idx = arrayFindNoCase(variables.valuable, valve);
        if (idx == 0) return arguments.mask;
        return bitOr(arguments.mask, 2 ^ (idx - 1));
    }

    /**
     * DFS to find maximum pressure release
     * Uses memoization with component-level cache
     */
    function dfs(required string pos, required numeric timeLeft, required numeric openedMask) {
        if (arguments.timeLeft <= 0) {
            return 0;
        }

        // Create cache key
        var cacheKey = arguments.pos & "|" & arguments.timeLeft & "|" & arguments.openedMask;
        if (structKeyExists(variables.memoCache, cacheKey)) {
            return variables.memoCache[cacheKey];
        }

        var best = 0;

        // Try opening each unopened valuable valve
        for (var nextValve in variables.valuable) {
            if (isValveOpen(arguments.openedMask, nextValve)) continue;

            // Time to move there and open it
            if (!structKeyExists(variables.distances[arguments.pos], nextValve)) continue;

            var timeCost = variables.distances[arguments.pos][nextValve] + 1;
            if (timeCost < arguments.timeLeft) {
                var newTime = arguments.timeLeft - timeCost;
                var pressure = variables.valves[nextValve] * newTime;
                var newMask = addValveToMask(arguments.openedMask, nextValve);
                var totalPressure = pressure + dfs(nextValve, newTime, newMask);
                if (totalPressure > best) {
                    best = totalPressure;
                }
            }
        }

        variables.memoCache[cacheKey] = best;
        return best;
    }

    /**
     * Part 1: Find maximum pressure release in 30 minutes
     */
    function part1(required string input) {
        parseInput(arguments.input);
        computeDistances();
        variables.memoCache = {};

        return dfs("AA", 30, 0);
    }

    /**
     * Compute max pressure for a specific subset of valves (by mask)
     * with fresh memoization cache
     */
    function maxPressureForSubset(required numeric subsetMask, required numeric timeLimit) {
        // Clear cache for fresh DFS
        variables.memoCache = {};

        // DFS that only considers valves in the subset
        var result = dfsWithSubset("AA", arguments.timeLimit, 0, arguments.subsetMask);
        return result;
    }

    /**
     * DFS constrained to a subset of valves (by mask)
     */
    function dfsWithSubset(required string pos, required numeric timeLeft, required numeric openedMask, required numeric subsetMask) {
        if (arguments.timeLeft <= 0) {
            return 0;
        }

        // Create cache key including subset
        var cacheKey = arguments.pos & "|" & arguments.timeLeft & "|" & arguments.openedMask & "|" & arguments.subsetMask;
        if (structKeyExists(variables.memoCache, cacheKey)) {
            return variables.memoCache[cacheKey];
        }

        var best = 0;

        // Try opening each unopened valve that's in our subset
        for (var i = 1; i <= arrayLen(variables.valuable); i++) {
            var nextValve = variables.valuable[i];
            var valveBit = 2 ^ (i - 1);

            // Skip if not in our subset
            if (bitAnd(arguments.subsetMask, valveBit) == 0) continue;

            // Skip if already opened
            if (bitAnd(arguments.openedMask, valveBit) > 0) continue;

            // Time to move there and open it
            if (!structKeyExists(variables.distances[arguments.pos], nextValve)) continue;

            var timeCost = variables.distances[arguments.pos][nextValve] + 1;
            if (timeCost < arguments.timeLeft) {
                var newTime = arguments.timeLeft - timeCost;
                var pressure = variables.valves[nextValve] * newTime;
                var newMask = bitOr(arguments.openedMask, valveBit);
                var totalPressure = pressure + dfsWithSubset(nextValve, newTime, newMask, arguments.subsetMask);
                if (totalPressure > best) {
                    best = totalPressure;
                }
            }
        }

        variables.memoCache[cacheKey] = best;
        return best;
    }

    /**
     * Part 2: Find maximum pressure with elephant helper (26 minutes each)
     */
    function part2(required string input) {
        parseInput(arguments.input);
        computeDistances();

        var n = arrayLen(variables.valuable);
        var fullMask = (2 ^ n) - 1;

        // Compute max pressure for each subset
        var maxScores = {};
        for (var mask = 0; mask <= fullMask; mask++) {
            variables.memoCache = {};
            maxScores[mask] = maxPressureForSubset(mask, 26);
        }

        // Find best partition where you and elephant handle disjoint sets
        var best = 0;
        for (var mask = 0; mask <= fullMask; mask++) {
            var complement = bitXor(fullMask, mask);
            if (mask <= complement) {
                var combined = maxScores[mask] + maxScores[complement];
                if (combined > best) {
                    best = combined;
                }
            }
        }

        return best;
    }

}
