component {

    function run() {
        // Read input file
        var inputPath = getCurrentTemplatePath();
        var inputDir = getDirectoryFromPath(inputPath);
        var inputFile = inputDir & "../input.txt";
        var inputText = fileRead(inputFile).trim();
        var lines = listToArray(inputText, chr(10));

        // Parse races
        var races = parseRaces(lines);

        // Solve both parts
        var part1Result = part1(races);
        var part2Result = part2(races);

        writeOutput("Part 1: " & part1Result & chr(10));
        writeOutput("Part 2: " & part2Result & chr(10));
    }

    function parseRaces(lines) {
        // Parse times from first line
        var timesStr = listRest(lines[1], ':');
        var times = [];
        for (var t in listToArray(trim(timesStr), ' ')) {
            if (len(trim(t)) > 0) {
                arrayAppend(times, val(t));
            }
        }

        // Parse distances from second line
        var distancesStr = listRest(lines[2], ':');
        var distances = [];
        for (var d in listToArray(trim(distancesStr), ' ')) {
            if (len(trim(d)) > 0) {
                arrayAppend(distances, val(d));
            }
        }

        // Zip times and distances
        var races = [];
        for (var i = 1; i <= arrayLen(times); i++) {
            arrayAppend(races, {time: times[i], distance: distances[i]});
        }

        return races;
    }

    function countWaysToWin(time, record) {
        /**
         * Count the number of ways to beat the record.
         *
         * If we hold the button for t ms, we travel t * (time - t) mm.
         * We need: t * (time - t) > record
         * Solving: -t^2 + time*t - record > 0
         * Roots: t = (time +/- sqrt(time^2 - 4*record)) / 2
         */
        var discriminant = time * time - 4 * record;
        if (discriminant <= 0) {
            return 0;
        }

        var sqrtD = sqr(discriminant);
        var tLow = (time - sqrtD) / 2;
        var tHigh = (time + sqrtD) / 2;

        // We need integer values strictly between the roots
        var first = int(tLow) + 1;
        // Ceiling - 1: if tHigh is exactly an integer, we need to go one below
        var last = ceiling(tHigh) - 1;

        if (last < first) {
            return 0;
        }
        return last - first + 1;
    }

    function part1(races) {
        var result = 1;
        for (var race in races) {
            var ways = countWaysToWin(race.time, race.distance);
            result *= ways;
        }
        return result;
    }

    function part2(races) {
        // Concatenate all times into single number
        var timeStr = "";
        for (var race in races) {
            timeStr &= race.time;
        }

        // Concatenate all distances into single number
        var distanceStr = "";
        for (var race in races) {
            distanceStr &= race.distance;
        }

        // Parse as numbers (using Java BigDecimal for precision)
        var time = val(timeStr);
        var record = val(distanceStr);

        return countWaysToWin(time, record);
    }
}
