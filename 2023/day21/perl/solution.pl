#!/usr/bin/env perl
# Day 21: Step Counter - Garden plot reachability
# Optimized: removed bigint, use array-based queue with index, packed keys

use strict;
use warnings;

# Parse the input grid and find starting position
sub parse_input {
    my ($filename) = @_;
    open my $fh, '<', $filename or die "Cannot open $filename: $!";
    my @grid;
    my ($start_r, $start_c);
    my $row = 0;
    while (my $line = <$fh>) {
        chomp $line;
        push @grid, $line;
        my $col = index($line, 'S');
        if ($col >= 0) {
            $start_r = $row;
            $start_c = $col;
        }
        $row++;
    }
    close $fh;
    return (\@grid, $start_r, $start_c);
}

# Pack coordinates into a single integer key
# Grid is 131x131, so we need to handle coordinates from about -400 to +400
# Use offset of 500 to make all values positive, then pack as (r+500)*1000 + (c+500)
sub pack_key {
    my ($r, $c) = @_;
    return ($r + 500) * 1000 + ($c + 500);
}

# Part 1: Count cells reachable in exactly 'steps' steps (bounded grid)
sub count_reachable {
    my ($grid, $start_r, $start_c, $steps) = @_;
    my $rows = scalar @$grid;
    my $cols = length $grid->[0];

    # BFS to find minimum steps to each cell
    my %visited;
    my @queue;
    push @queue, [$start_r, $start_c, 0];
    my $qi = 0;
    $visited{pack_key($start_r, $start_c)} = 0;

    while ($qi < @queue) {
        my ($r, $c, $dist) = @{$queue[$qi++]};

        next if $dist >= $steps;

        for my $dir ([-1, 0], [1, 0], [0, -1], [0, 1]) {
            my ($dr, $dc) = @$dir;
            my $nr = $r + $dr;
            my $nc = $c + $dc;

            if ($nr >= 0 && $nr < $rows && $nc >= 0 && $nc < $cols) {
                my $key = pack_key($nr, $nc);
                my $ch = substr($grid->[$nr], $nc, 1);
                if ($ch ne '#' && !exists $visited{$key}) {
                    $visited{$key} = $dist + 1;
                    push @queue, [$nr, $nc, $dist + 1];
                }
            }
        }
    }

    # Count cells reachable in exactly 'steps' steps
    # A cell reachable in d steps can be reached in d+2, d+4, ... steps
    my $target_parity = $steps % 2;
    my $count = 0;
    for my $d (values %visited) {
        if ($d <= $steps && $d % 2 == $target_parity) {
            $count++;
        }
    }
    return $count;
}

# BFS on infinite tiled grid for small step counts
sub count_reachable_infinite_bfs {
    my ($grid, $start_r, $start_c, $steps) = @_;
    my $rows = scalar @$grid;
    my $cols = length $grid->[0];

    my %visited;
    my @queue;
    push @queue, [$start_r, $start_c, 0];
    my $qi = 0;
    $visited{pack_key($start_r, $start_c)} = 0;

    while ($qi < @queue) {
        my ($r, $c, $dist) = @{$queue[$qi++]};

        next if $dist >= $steps;

        for my $dir ([-1, 0], [1, 0], [0, -1], [0, 1]) {
            my ($dr, $dc) = @$dir;
            my $nr = $r + $dr;
            my $nc = $c + $dc;

            my $key = pack_key($nr, $nc);
            # Map to grid coordinates (infinite tiling)
            my $gr = $nr % $rows;
            $gr += $rows if $gr < 0;
            my $gc = $nc % $cols;
            $gc += $cols if $gc < 0;

            my $ch = substr($grid->[$gr], $gc, 1);
            if ($ch ne '#' && !exists $visited{$key}) {
                $visited{$key} = $dist + 1;
                push @queue, [$nr, $nc, $dist + 1];
            }
        }
    }

    # Count cells reachable in exactly 'steps' steps
    my $target_parity = $steps % 2;
    my $count = 0;
    for my $d (values %visited) {
        if ($d <= $steps && $d % 2 == $target_parity) {
            $count++;
        }
    }
    return $count;
}

# Part 2: Count cells reachable in 26501365 steps on infinite grid
# Uses quadratic pattern based on grid structure
sub count_reachable_infinite {
    my ($grid, $start_r, $start_c, $steps) = @_;
    my $rows = scalar @$grid;
    my $size = $rows;  # Grid is square
    my $half = int($size / 2);

    # For small step counts, use direct BFS
    if ($steps <= $size * 2) {
        return count_reachable_infinite_bfs($grid, $start_r, $start_c, $steps);
    }

    # The number of full grid widths we travel
    my $n = int(($steps - $half) / $size);

    # Calculate reachable counts for n=0, 1, 2
    my $y0 = count_reachable_infinite_bfs($grid, $start_r, $start_c, $half);
    my $y1 = count_reachable_infinite_bfs($grid, $start_r, $start_c, $half + $size);
    my $y2 = count_reachable_infinite_bfs($grid, $start_r, $start_c, $half + 2 * $size);

    # Solve for a, b, c using Lagrange interpolation
    # f(x) = y0 + (y1-y0)*x + (y2-2*y1+y0)*x*(x-1)/2
    # a = (y2 - 2*y1 + y0) / 2
    # b = y1 - y0 - a
    # c = y0
    my $a = int(($y2 - 2 * $y1 + $y0) / 2);
    my $b = $y1 - $y0 - $a;
    my $c = $y0;

    return $a * $n * $n + $b * $n + $c;
}

sub part1 {
    my ($grid, $start_r, $start_c) = @_;
    return count_reachable($grid, $start_r, $start_c, 64);
}

sub part2 {
    my ($grid, $start_r, $start_c) = @_;
    return count_reachable_infinite($grid, $start_r, $start_c, 26501365);
}

# Main
my $input_file = $ARGV[0] // "../input.txt";
my ($grid, $start_r, $start_c) = parse_input($input_file);

print "Part 1: ", part1($grid, $start_r, $start_c), "\n";
print "Part 2: ", part2($grid, $start_r, $start_c), "\n";
