#!/usr/bin/env php
<?php
ini_set('memory_limit', '512M');

class UnionFind {
    private $parent;
    private $rank;
    private $size;

    public function __construct($n) {
        $this->parent = range(0, $n - 1);
        $this->rank = array_fill(0, $n, 0);
        $this->size = array_fill(0, $n, 1);
    }

    public function find($x) {
        if ($this->parent[$x] !== $x) {
            $this->parent[$x] = $this->find($this->parent[$x]); // Path compression
        }
        return $this->parent[$x];
    }

    public function union($x, $y) {
        // Union two elements. Returns true if they were in different sets.
        $px = $this->find($x);
        $py = $this->find($y);

        if ($px === $py) {
            return false; // Already in same set
        }

        // Union by rank
        if ($this->rank[$px] < $this->rank[$py]) {
            [$px, $py] = [$py, $px];
        }

        $this->parent[$py] = $px;
        $this->size[$px] += $this->size[$py];

        if ($this->rank[$px] === $this->rank[$py]) {
            $this->rank[$px]++;
        }

        return true;
    }

    public function getComponentSizes() {
        // Get sizes of all connected components
        $sizes = [];
        for ($i = 0; $i < count($this->parent); $i++) {
            if ($this->parent[$i] === $i) { // Root of a component
                $sizes[] = $this->size[$i];
            }
        }
        return $sizes;
    }
}

function parseInput($filename) {
    // Parse the input file and return array of [x, y, z] arrays
    $points = [];
    $lines = file($filename, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);

    foreach ($lines as $line) {
        $coords = array_map('intval', explode(',', trim($line)));
        if (count($coords) === 3) {
            $points[] = $coords;
        }
    }

    return $points;
}

function euclideanDistanceSq($p1, $p2) {
    // Squared Euclidean distance (avoid sqrt for comparison)
    $dx = $p1[0] - $p2[0];
    $dy = $p1[1] - $p2[1];
    $dz = $p1[2] - $p2[2];
    return $dx * $dx + $dy * $dy + $dz * $dz;
}

function part1($points, $numConnections = 1000) {
    // Connect the numConnections closest pairs and return product of 3 largest component sizes
    $n = count($points);

    // Generate all pairs with distances
    $pairs = [];
    for ($i = 0; $i < $n; $i++) {
        for ($j = $i + 1; $j < $n; $j++) {
            $distSq = euclideanDistanceSq($points[$i], $points[$j]);
            $pairs[] = [$distSq, $i, $j];
        }
    }

    // Sort by distance
    usort($pairs, function($a, $b) {
        return $a[0] <=> $b[0];
    });

    // Union-Find to connect closest pairs
    $uf = new UnionFind($n);
    $connections = 0;

    foreach ($pairs as $pair) {
        list($distSq, $i, $j) = $pair;
        $uf->union($i, $j); // Always attempt union
        $connections++;
        if ($connections === $numConnections) {
            break;
        }
    }

    // Get component sizes and find the 3 largest
    $sizes = $uf->getComponentSizes();
    rsort($sizes);

    // Multiply the 3 largest
    return $sizes[0] * $sizes[1] * $sizes[2];
}

function part2($points) {
    // Connect all junction boxes into one circuit. Return product of X coordinates of last connection.
    $n = count($points);

    // Generate all pairs with distances
    $pairs = [];
    for ($i = 0; $i < $n; $i++) {
        for ($j = $i + 1; $j < $n; $j++) {
            $distSq = euclideanDistanceSq($points[$i], $points[$j]);
            $pairs[] = [$distSq, $i, $j];
        }
    }

    // Sort by distance
    usort($pairs, function($a, $b) {
        return $a[0] <=> $b[0];
    });

    // Union-Find to connect until all in one circuit
    $uf = new UnionFind($n);
    $numComponents = $n;

    foreach ($pairs as $pair) {
        list($distSq, $i, $j) = $pair;
        if ($uf->union($i, $j)) { // Actually merged two components
            $numComponents--;
            if ($numComponents === 1) {
                // This was the last connection - all in one circuit now
                return $points[$i][0] * $points[$j][0]; // Product of X coordinates
            }
        }
    }

    return 0;
}

function main() {
    $inputFile = $_SERVER['argv'][1] ?? '../input.txt';
    $points = parseInput($inputFile);

    echo "Part 1: " . part1($points) . "\n";
    echo "Part 2: " . part2($points) . "\n";
}

main();
