<?php
/**
 * Advent of Code 2023 - Day 25: Snowverload
 * Find the minimum cut of 3 edges that divides the graph into two components.
 *
 * Uses edge betweenness centrality: edges that form the cut between two large
 * components will have high betweenness (many shortest paths pass through them).
 */

function parseInput($filename) {
    $graph = [];
    $lines = file($filename, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);

    foreach ($lines as $line) {
        $parts = explode(': ', $line);
        $left = trim($parts[0]);
        $neighbors = explode(' ', trim($parts[1]));

        if (!isset($graph[$left])) {
            $graph[$left] = [];
        }

        foreach ($neighbors as $neighbor) {
            $graph[$left][$neighbor] = true;
            if (!isset($graph[$neighbor])) {
                $graph[$neighbor] = [];
            }
            $graph[$neighbor][$left] = true;
        }
    }

    return $graph;
}

function bfsComponentSize($graph, $start, $excludedEdges) {
    $visited = [$start => true];
    $queue = [$start];
    $head = 0;

    while ($head < count($queue)) {
        $node = $queue[$head++];

        foreach (array_keys($graph[$node]) as $neighbor) {
            $edge = [$node, $neighbor];
            sort($edge);
            $edgeKey = $edge[0] . '-' . $edge[1];

            if (!isset($visited[$neighbor]) && !isset($excludedEdges[$edgeKey])) {
                $visited[$neighbor] = true;
                $queue[] = $neighbor;
            }
        }
    }

    return count($visited);
}

function computeEdgeBetweenness($graph, $sampleNodes = null) {
    $edgeCount = [];
    $nodes = array_keys($graph);

    // Sample nodes for efficiency
    if ($sampleNodes !== null && count($nodes) > $sampleNodes) {
        mt_srand(42);
        shuffle($nodes);
        $nodes = array_slice($nodes, 0, $sampleNodes);
    }

    foreach ($nodes as $source) {
        // BFS to find shortest paths
        $dist = [$source => 0];
        $pred = [];
        $queue = [$source];
        $head = 0;

        while ($head < count($queue)) {
            $node = $queue[$head++];

            foreach (array_keys($graph[$node]) as $neighbor) {
                if (!isset($dist[$neighbor])) {
                    $dist[$neighbor] = $dist[$node] + 1;
                    $pred[$neighbor] = [$node];
                    $queue[] = $neighbor;
                } elseif ($dist[$neighbor] == $dist[$node] + 1) {
                    $pred[$neighbor][] = $node;
                }
            }
        }

        // Number of shortest paths to each node
        $numPaths = [];
        $numPaths[$source] = 1.0;

        // Sort nodes by distance
        $sortedNodes = array_keys($dist);
        usort($sortedNodes, function($a, $b) use ($dist) {
            return $dist[$a] <=> $dist[$b];
        });

        foreach ($sortedNodes as $node) {
            if (!isset($numPaths[$node])) {
                $numPaths[$node] = 0.0;
            }
            if (isset($pred[$node])) {
                foreach ($pred[$node] as $p) {
                    $numPaths[$node] += $numPaths[$p];
                }
            }
        }

        // Accumulate edge betweenness (reverse BFS order)
        $dependency = [];

        // Sort nodes by distance (descending)
        usort($sortedNodes, function($a, $b) use ($dist) {
            return $dist[$b] <=> $dist[$a];
        });

        foreach ($sortedNodes as $node) {
            if (!isset($dependency[$node])) {
                $dependency[$node] = 0.0;
            }

            if (isset($pred[$node])) {
                foreach ($pred[$node] as $p) {
                    $edge = [$p, $node];
                    sort($edge);
                    $edgeKey = $edge[0] . '-' . $edge[1];

                    // Weight by path fraction
                    $frac = $numPaths[$p] / $numPaths[$node];
                    $contrib = $frac * (1 + $dependency[$node]);

                    if (!isset($edgeCount[$edgeKey])) {
                        $edgeCount[$edgeKey] = 0.0;
                    }
                    $edgeCount[$edgeKey] += $contrib;

                    if (!isset($dependency[$p])) {
                        $dependency[$p] = 0.0;
                    }
                    $dependency[$p] += $contrib;
                }
            }
        }
    }

    return $edgeCount;
}

function findCutEdges($graph) {
    // Compute edge betweenness with sampling for speed
    $edgeBetweenness = computeEdgeBetweenness($graph, 100);

    // Sort edges by betweenness (highest first)
    arsort($edgeBetweenness);

    $totalNodes = count($graph);

    // Try removing top candidate edges
    // We need to find 3 edges that disconnect the graph
    $topEdges = array_slice(array_keys($edgeBetweenness), 0, 20);

    for ($i = 0; $i < count($topEdges); $i++) {
        for ($j = $i + 1; $j < count($topEdges); $j++) {
            for ($k = $j + 1; $k < count($topEdges); $k++) {
                $excluded = [
                    $topEdges[$i] => true,
                    $topEdges[$j] => true,
                    $topEdges[$k] => true
                ];

                $start = array_key_first($graph);
                $size1 = bfsComponentSize($graph, $start, $excluded);

                if ($size1 < $totalNodes) {
                    // Graph is disconnected!
                    $size2 = $totalNodes - $size1;
                    return $size1 * $size2;
                }
            }
        }
    }

    return null;
}

function part1($filename) {
    $graph = parseInput($filename);
    return findCutEdges($graph);
}

function part2($filename) {
    return "Push the big red button!";
}

// Main execution
$inputFile = __DIR__ . '/../input.txt';

echo "Part 1: " . part1($inputFile) . "\n";
echo "Part 2: " . part2($inputFile) . "\n";
