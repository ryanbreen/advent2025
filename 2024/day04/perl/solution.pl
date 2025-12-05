#!/usr/bin/env perl
use strict;
use warnings;
use Cwd 'abs_path';
use File::Basename;

# Read input file
my $script_dir = dirname(abs_path($0));
my $input_file = "$script_dir/../input.txt";
open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
my @grid = <$fh>;
chomp @grid;
close $fh;

my $rows = scalar @grid;
my $cols = length($grid[0]);

# 8 directions: right, left, down, up, and 4 diagonals
my @DIRECTIONS = (
    [0, 1],    # right
    [0, -1],   # left
    [1, 0],    # down
    [-1, 0],   # up
    [1, 1],    # down-right
    [1, -1],   # down-left
    [-1, 1],   # up-right
    [-1, -1],  # up-left
);

sub part1 {
    my $target = "XMAS";
    my @target_chars = split //, $target;
    my $count = 0;

    for my $r (0 .. $rows - 1) {
        for my $c (0 .. $cols - 1) {
            # Try each direction from this position
            for my $dir (@DIRECTIONS) {
                my ($dr, $dc) = @$dir;
                my $found = 1;

                # Check if XMAS fits in this direction
                for my $i (0 .. $#target_chars) {
                    my $nr = $r + $dr * $i;
                    my $nc = $c + $dc * $i;

                    if ($nr < 0 || $nr >= $rows || $nc < 0 || $nc >= $cols) {
                        $found = 0;
                        last;
                    }

                    if (substr($grid[$nr], $nc, 1) ne $target_chars[$i]) {
                        $found = 0;
                        last;
                    }
                }

                $count++ if $found;
            }
        }
    }

    return $count;
}

sub part2 {
    # Find X-MAS patterns: two MAS strings forming an X with A in the center
    # Each diagonal can be MAS or SAM
    my $count = 0;

    # Check each possible center point (A must be in the middle)
    for my $r (1 .. $rows - 2) {
        for my $c (1 .. $cols - 2) {
            next if substr($grid[$r], $c, 1) ne 'A';

            # Get the four corners
            my $top_left = substr($grid[$r - 1], $c - 1, 1);
            my $top_right = substr($grid[$r - 1], $c + 1, 1);
            my $bottom_left = substr($grid[$r + 1], $c - 1, 1);
            my $bottom_right = substr($grid[$r + 1], $c + 1, 1);

            # Check diagonal 1 (top-left to bottom-right): MAS or SAM
            my $diag1_ok = ($top_left eq 'M' && $bottom_right eq 'S') ||
                          ($top_left eq 'S' && $bottom_right eq 'M');

            # Check diagonal 2 (top-right to bottom-left): MAS or SAM
            my $diag2_ok = ($top_right eq 'M' && $bottom_left eq 'S') ||
                          ($top_right eq 'S' && $bottom_left eq 'M');

            $count++ if $diag1_ok && $diag2_ok;
        }
    }

    return $count;
}

print "Part 1: ", part1(), "\n";
print "Part 2: ", part2(), "\n";
