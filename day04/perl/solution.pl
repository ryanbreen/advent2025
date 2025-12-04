#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;
use Cwd 'abs_path';

# Read input file
my $script_dir = dirname(abs_path($0));
my $input_file = "$script_dir/../input.txt";
open(my $fh, '<', $input_file) or die "Cannot open $input_file: $!";
my @lines = <$fh>;
close($fh);
chomp @lines;

# Parse grid
my @grid;
foreach my $line (@lines) {
    push @grid, [split(//, $line)];
}

my $rows = scalar @grid;
my $cols = scalar @{$grid[0]};

# Directions for 8 neighbors (including diagonals)
my @directions = (
    [-1, -1], [-1, 0], [-1, 1],
    [0, -1],           [0, 1],
    [1, -1],  [1, 0],  [1, 1]
);

# ============== PRECOMPUTE ROLL POSITIONS AND NEIGHBORS ==============
my @roll_positions;  # Array of [r, c] for each roll
my %pos_to_index;    # Hash: "r,c" => index in roll_positions
my @roll_neighbors;  # Array of arrays: neighbors for each roll

for (my $r = 0; $r < $rows; $r++) {
    for (my $c = 0; $c < $cols; $c++) {
        if ($grid[$r][$c] eq '@') {
            my $idx = scalar @roll_positions;
            push @roll_positions, [$r, $c];
            $pos_to_index{"$r,$c"} = $idx;

            # Precompute neighbors for this roll
            my @neighbors;
            foreach my $dir (@directions) {
                my $nr = $r + $dir->[0];
                my $nc = $c + $dir->[1];
                if ($nr >= 0 && $nr < $rows && $nc >= 0 && $nc < $cols) {
                    if ($grid[$nr][$nc] eq '@') {
                        push @neighbors, "$nr,$nc";
                    }
                }
            }
            push @roll_neighbors, \@neighbors;
        }
    }
}

my $num_rolls = scalar @roll_positions;

# Part 1: Count rolls with fewer than 4 adjacent rolls
sub part1 {
    my $accessible_count = 0;

    for (my $i = 0; $i < $num_rolls; $i++) {
        my $neighbor_count = scalar @{$roll_neighbors[$i]};
        if ($neighbor_count < 4) {
            $accessible_count++;
        }
    }

    return $accessible_count;
}

# Part 2: Work-queue algorithm
sub part2 {
    # Track which rolls are still active
    my %active;
    for (my $i = 0; $i < $num_rolls; $i++) {
        my ($r, $c) = @{$roll_positions[$i]};
        $active{"$r,$c"} = 1;
    }

    # Compute initial neighbor counts
    my @neighbor_count;
    for (my $i = 0; $i < $num_rolls; $i++) {
        my $count = 0;
        foreach my $neighbor_pos (@{$roll_neighbors[$i]}) {
            if ($active{$neighbor_pos}) {
                $count++;
            }
        }
        $neighbor_count[$i] = $count;
    }

    # Initialize queue with accessible rolls (neighbor count < 4)
    my @queue;
    my %in_queue;
    for (my $i = 0; $i < $num_rolls; $i++) {
        if ($neighbor_count[$i] < 4) {
            push @queue, $i;
            $in_queue{$i} = 1;
        }
    }

    # Process queue
    my $total_removed = 0;

    while (@queue) {
        my @next_queue;

        foreach my $idx (@queue) {
            my ($r, $c) = @{$roll_positions[$idx]};
            my $pos_key = "$r,$c";

            # Skip if already removed
            next unless $active{$pos_key};

            # Remove this roll
            delete $active{$pos_key};
            $total_removed++;

            # Update neighbors' counts
            foreach my $neighbor_pos (@{$roll_neighbors[$idx]}) {
                if ($active{$neighbor_pos}) {
                    my $neighbor_idx = $pos_to_index{$neighbor_pos};
                    $neighbor_count[$neighbor_idx]--;

                    # Add to queue if now accessible and not already queued
                    if ($neighbor_count[$neighbor_idx] < 4 && !$in_queue{$neighbor_idx}) {
                        push @next_queue, $neighbor_idx;
                        $in_queue{$neighbor_idx} = 1;
                    }
                }
            }
        }

        @queue = @next_queue;
    }

    return $total_removed;
}

# Run both parts
my $answer1 = part1();
my $answer2 = part2();

print "Part 1: $answer1\n";
print "Part 2: $answer2\n";
