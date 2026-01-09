<?php

function parseInput(string $text): array {
    $valves = [];
    $tunnels = [];

    $pattern = '/Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.+)/';
    foreach (explode("\n", trim($text)) as $line) {
        if (preg_match($pattern, $line, $match)) {
            $name = $match[1];
            $rate = (int)$match[2];
            $neighbors = explode(', ', $match[3]);
            $valves[$name] = $rate;
            $tunnels[$name] = $neighbors;
        }
    }

    return [$valves, $tunnels];
}

function computeDistances(array $valves, array $tunnels): array {
    // Only care about valves with flow > 0 plus starting valve AA
    $relevant = ['AA'];
    foreach ($valves as $name => $rate) {
        if ($rate > 0) {
            $relevant[] = $name;
        }
    }

    $distances = [];

    foreach ($relevant as $start) {
        $distances[$start] = [];
        $queue = [[$start, 0]];
        $visited = [$start => true];
        $head = 0;

        while ($head < count($queue)) {
            [$curr, $dist] = $queue[$head++];

            if (in_array($curr, $relevant) && $curr !== $start) {
                $distances[$start][$curr] = $dist;
            }

            foreach ($tunnels[$curr] as $neighbor) {
                if (!isset($visited[$neighbor])) {
                    $visited[$neighbor] = true;
                    $queue[] = [$neighbor, $dist + 1];
                }
            }
        }
    }

    return $distances;
}

function part1(string $text): int {
    [$valves, $tunnels] = parseInput($text);
    $distances = computeDistances($valves, $tunnels);

    // Only consider valves with positive flow
    $valuable = [];
    foreach ($valves as $name => $rate) {
        if ($rate > 0) {
            $valuable[] = $name;
        }
    }

    // Use memoization with a cache array
    $cache = [];

    $dfs = function(string $pos, int $timeLeft, int $openedMask) use (&$dfs, &$cache, $valuable, $valves, $distances): int {
        if ($timeLeft <= 0) {
            return 0;
        }

        $key = "$pos|$timeLeft|$openedMask";
        if (isset($cache[$key])) {
            return $cache[$key];
        }

        $best = 0;
        foreach ($valuable as $i => $nextValve) {
            if ($openedMask & (1 << $i)) {
                continue; // Already opened
            }

            // Time to move there and open it
            $timeCost = $distances[$pos][$nextValve] + 1;
            if ($timeCost < $timeLeft) {
                $newTime = $timeLeft - $timeCost;
                $pressure = $valves[$nextValve] * $newTime;
                $newMask = $openedMask | (1 << $i);
                $best = max($best, $pressure + $dfs($nextValve, $newTime, $newMask));
            }
        }

        $cache[$key] = $best;
        return $best;
    };

    return $dfs('AA', 30, 0);
}

function part2(string $text): int {
    [$valves, $tunnels] = parseInput($text);
    $distances = computeDistances($valves, $tunnels);

    // Only consider valves with positive flow
    $valuable = [];
    foreach ($valves as $name => $rate) {
        if ($rate > 0) {
            $valuable[] = $name;
        }
    }

    $n = count($valuable);
    $fullMask = (1 << $n) - 1;

    // Create a mapping from valve name to index for faster lookup
    $valveIndex = [];
    foreach ($valuable as $i => $name) {
        $valveIndex[$name] = $i;
    }

    // DFS with subset mask - returns max pressure achievable when
    // only valves in subsetMask are available
    $dfsForSubset = function(int $subsetMask) use ($valuable, $valves, $distances, $valveIndex): int {
        // Use iterative approach with stack to reduce memory overhead
        // State: [pos, timeLeft, openedMask, pressureSoFar]
        $best = 0;
        $stack = [['AA', 26, 0, 0]];

        // For better performance, use local cache for this subset only
        $visited = [];

        while (!empty($stack)) {
            [$pos, $timeLeft, $openedMask, $pressureSoFar] = array_pop($stack);

            // Prune if we can't beat best even with all remaining valves
            $key = "$pos|$timeLeft|$openedMask";
            if (isset($visited[$key]) && $visited[$key] >= $pressureSoFar) {
                continue;
            }
            $visited[$key] = $pressureSoFar;

            $foundNext = false;
            foreach ($valuable as $i => $nextValve) {
                // Skip if not in our subset or already opened
                if (!($subsetMask & (1 << $i))) {
                    continue;
                }
                if ($openedMask & (1 << $i)) {
                    continue;
                }

                $timeCost = $distances[$pos][$nextValve] + 1;
                if ($timeCost < $timeLeft) {
                    $foundNext = true;
                    $newTime = $timeLeft - $timeCost;
                    $pressure = $valves[$nextValve] * $newTime;
                    $newMask = $openedMask | (1 << $i);
                    $stack[] = [$nextValve, $newTime, $newMask, $pressureSoFar + $pressure];
                }
            }

            if (!$foundNext) {
                $best = max($best, $pressureSoFar);
            }
        }

        return $best;
    };

    // Generate all subsets and compute max pressure
    $maxScores = [];

    for ($mask = 0; $mask <= $fullMask; $mask++) {
        $maxScores[$mask] = $dfsForSubset($mask);
    }

    // Find best partition where you and elephant open disjoint sets
    $best = 0;
    for ($mask = 0; $mask <= $fullMask; $mask++) {
        $complement = $fullMask ^ $mask;
        if ($mask <= $complement) { // Avoid counting same partition twice
            $best = max($best, $maxScores[$mask] + $maxScores[$complement]);
        }
    }

    return $best;
}

// Main
$scriptDir = dirname(__FILE__);
$inputFile = $scriptDir . '/../input.txt';
$text = file_get_contents($inputFile);

echo "Part 1: " . part1($text) . "\n";
echo "Part 2: " . part2($text) . "\n";
