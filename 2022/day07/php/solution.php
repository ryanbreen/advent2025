<?php

/**
 * Advent of Code 2022 - Day 7: No Space Left On Device
 *
 * Parse terminal output to build directory tree, track sizes,
 * and find directories meeting size criteria.
 */

function parseFilesystem(array $lines): array {
    $path = [];
    $dirSizes = [];

    foreach ($lines as $line) {
        if (str_starts_with($line, '$ cd')) {
            $target = substr($line, 5);
            if ($target === '/') {
                $path = ['/'];
            } elseif ($target === '..') {
                array_pop($path);
            } else {
                $path[] = $target;
            }
        } elseif (str_starts_with($line, '$ ls')) {
            continue;
        } elseif (str_starts_with($line, 'dir ')) {
            continue;
        } else {
            // It's a file with size
            $parts = explode(' ', $line);
            $size = (int)$parts[0];

            // Add size to current directory and all parent directories
            for ($i = 0; $i < count($path); $i++) {
                $dirPath = implode('/', array_slice($path, 0, $i + 1)) ?: '/';
                if (!isset($dirSizes[$dirPath])) {
                    $dirSizes[$dirPath] = 0;
                }
                $dirSizes[$dirPath] += $size;
            }
        }
    }

    return $dirSizes;
}

function part1(array $dirSizes): int {
    $sum = 0;
    foreach ($dirSizes as $size) {
        if ($size <= 100000) {
            $sum += $size;
        }
    }
    return $sum;
}

function part2(array $dirSizes): int {
    $totalSpace = 70000000;
    $neededSpace = 30000000;
    $usedSpace = $dirSizes['/'];
    $freeSpace = $totalSpace - $usedSpace;
    $needToFree = $neededSpace - $freeSpace;

    // Find smallest directory >= needToFree
    $candidates = array_filter($dirSizes, fn($size) => $size >= $needToFree);
    return min($candidates);
}

function main(): void {
    $inputFile = __DIR__ . '/../input.txt';
    $content = trim(file_get_contents($inputFile));
    $lines = explode("\n", $content);

    $dirSizes = parseFilesystem($lines);

    echo 'Part 1: ' . part1($dirSizes) . PHP_EOL;
    echo 'Part 2: ' . part2($dirSizes) . PHP_EOL;
}

main();
