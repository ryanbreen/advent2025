#!/usr/bin/env php
<?php

function parseInput($filename) {
    $graph = [];
    $lines = file($filename, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);

    foreach ($lines as $line) {
        list($a, $b) = explode('-', trim($line));

        if (!isset($graph[$a])) {
            $graph[$a] = [];
        }
        if (!isset($graph[$b])) {
            $graph[$b] = [];
        }

        $graph[$a][$b] = true;
        $graph[$b][$a] = true;
    }

    return $graph;
}

function findTriangles($graph) {
    $triangles = [];

    foreach ($graph as $a => $neighborsA) {
        foreach ($neighborsA as $b => $_) {
            if ($a < $b) {
                // Find common neighbors
                foreach ($neighborsA as $c => $_) {
                    if (isset($graph[$b][$c]) && $a < $c && $b < $c) {
                        // Create sorted triangle key
                        $tri = [$a, $b, $c];
                        sort($tri);
                        $key = implode(',', $tri);
                        $triangles[$key] = $tri;
                    }
                }
            }
        }
    }

    return array_values($triangles);
}

function part1($graph) {
    $triangles = findTriangles($graph);
    $count = 0;

    foreach ($triangles as $tri) {
        foreach ($tri as $node) {
            if ($node[0] === 't') {
                $count++;
                break;
            }
        }
    }

    return $count;
}

function bronKerbosch($graph, $r, $p, $x, &$cliques) {
    if (empty($p) && empty($x)) {
        $cliques[] = $r;
        return;
    }

    // Find pivot with maximum neighbors in p
    $union = array_merge($p, $x);
    $maxNeighbors = -1;
    $pivot = null;

    foreach ($union as $v) {
        $neighbors = isset($graph[$v]) ? array_intersect_key($graph[$v], $p) : [];
        $count = count($neighbors);
        if ($count > $maxNeighbors) {
            $maxNeighbors = $count;
            $pivot = $v;
        }
    }

    // Get pivot neighbors in p
    $pivotNeighbors = isset($graph[$pivot]) ? array_intersect_key($graph[$pivot], $p) : [];
    $candidates = array_diff_key($p, $pivotNeighbors);

    foreach ($candidates as $v => $_) {
        $neighbors = isset($graph[$v]) ? $graph[$v] : [];

        $newR = $r;
        $newR[$v] = true;

        $newP = array_intersect_key($p, $neighbors);
        $newX = array_intersect_key($x, $neighbors);

        bronKerbosch($graph, $newR, $newP, $newX, $cliques);

        unset($p[$v]);
        $x[$v] = true;
    }
}

function part2($graph) {
    $cliques = [];
    $allNodes = [];

    foreach ($graph as $node => $_) {
        $allNodes[$node] = true;
    }

    bronKerbosch($graph, [], $allNodes, [], $cliques);

    // Find the largest clique
    $largest = [];
    foreach ($cliques as $clique) {
        if (count($clique) > count($largest)) {
            $largest = $clique;
        }
    }

    // Return sorted, comma-joined password
    $nodes = array_keys($largest);
    sort($nodes);
    return implode(',', $nodes);
}

function main() {
    $graph = parseInput('../input.txt');

    echo 'Part 1: ' . part1($graph) . "\n";
    echo 'Part 2: ' . part2($graph) . "\n";
}

main();
