#!/usr/bin/env php
<?php

function parse_input(string $filename): array {
    $graph = [];
    $lines = file($filename, FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES);

    foreach ($lines as $line) {
        $parts = explode(': ', $line, 2);
        $node = $parts[0];
        $neighbors = isset($parts[1]) ? explode(' ', $parts[1]) : [];
        $graph[$node] = $neighbors;
    }

    return $graph;
}

function part1(array $graph): int {
    $counter = new PathCounter($graph);
    return $counter->count_paths('you', 'out');
}

// Helper class for memoized path counting
class PathCounter {
    private array $graph;
    private array $memo_cache = [];

    public function __construct(array $graph) {
        $this->graph = $graph;
    }

    private function count_helper(string $node, string $target, array &$memo): int {
        if ($node === $target) {
            return 1;
        }

        if (isset($memo[$node])) {
            return $memo[$node];
        }

        if (!isset($this->graph[$node])) {
            $memo[$node] = 0;
            return 0;
        }

        $total = 0;
        foreach ($this->graph[$node] as $neighbor) {
            $total += $this->count_helper($neighbor, $target, $memo);
        }

        $memo[$node] = $total;
        return $total;
    }

    public function count_paths(string $start_node, string $target): int {
        if (!isset($this->memo_cache[$target])) {
            $this->memo_cache[$target] = [];
        }

        return $this->count_helper($start_node, $target, $this->memo_cache[$target]);
    }
}

function part2(array $graph): string {
    $counter = new PathCounter($graph);

    // Paths that visit dac before fft: svr -> dac -> fft -> out
    $svr_to_dac = $counter->count_paths('svr', 'dac');
    $dac_to_fft = $counter->count_paths('dac', 'fft');
    $fft_to_out = $counter->count_paths('fft', 'out');

    // Use bcmul for large number multiplication to avoid overflow
    $dac_before_fft = bcmul(bcmul((string)$svr_to_dac, (string)$dac_to_fft), (string)$fft_to_out);

    // Paths that visit fft before dac: svr -> fft -> dac -> out
    $svr_to_fft = $counter->count_paths('svr', 'fft');
    $fft_to_dac = $counter->count_paths('fft', 'dac');
    $dac_to_out = $counter->count_paths('dac', 'out');

    $fft_before_dac = bcmul(bcmul((string)$svr_to_fft, (string)$fft_to_dac), (string)$dac_to_out);

    return bcadd($dac_before_fft, $fft_before_dac);
}

function main(): void {
    global $argc, $argv;

    $input_file = '../input.txt';
    if ($argc > 1) {
        $input_file = $argv[1];
    }

    $graph = parse_input($input_file);

    echo "Part 1: " . part1($graph) . "\n";
    echo "Part 2: " . part2($graph) . "\n";
}

main();
