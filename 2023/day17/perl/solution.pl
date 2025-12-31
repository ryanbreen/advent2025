#!/usr/bin/env perl
# Day 17: Clumsy Crucible - Dijkstra's shortest path with movement constraints.

use strict;
use warnings;
use File::Basename;

# Priority queue implementation using a heap
package MinHeap;

sub new {
    my $class = shift;
    return bless { heap => [] }, $class;
}

sub push {
    my ($self, $priority, @data) = @_;
    my $heap = $self->{heap};
    push @$heap, [$priority, @data];
    my $i = $#$heap;
    while ($i > 0) {
        my $parent = int(($i - 1) / 2);
        last if $heap->[$parent][0] <= $heap->[$i][0];
        @{$heap}[$parent, $i] = @{$heap}[$i, $parent];
        $i = $parent;
    }
}

sub pop {
    my $self = shift;
    my $heap = $self->{heap};
    return undef unless @$heap;
    my $min = $heap->[0];
    my $last = pop @$heap;
    if (@$heap) {
        $heap->[0] = $last;
        my $i = 0;
        my $size = @$heap;
        while (1) {
            my $left = 2 * $i + 1;
            my $right = 2 * $i + 2;
            my $smallest = $i;
            $smallest = $left if $left < $size && $heap->[$left][0] < $heap->[$smallest][0];
            $smallest = $right if $right < $size && $heap->[$right][0] < $heap->[$smallest][0];
            last if $smallest == $i;
            @{$heap}[$i, $smallest] = @{$heap}[$smallest, $i];
            $i = $smallest;
        }
    }
    return @$min;
}

sub is_empty {
    my $self = shift;
    return @{$self->{heap}} == 0;
}

package main;

# Direction deltas: 0=right, 1=down, 2=left, 3=up
my @dr = (0, 1, 0, -1);
my @dc = (1, 0, -1, 0);

sub parse_input {
    my ($filename) = @_;
    open my $fh, '<', $filename or die "Cannot open $filename: $!";
    my @grid;
    while (<$fh>) {
        chomp;
        push @grid, [split //, $_];
    }
    close $fh;
    return \@grid;
}

sub dijkstra {
    my ($grid, $min_straight, $max_straight) = @_;
    my $rows = @$grid;
    my $cols = @{$grid->[0]};

    # Priority queue: (heat_loss, row, col, direction, consecutive)
    my $pq = MinHeap->new();
    $pq->push(0, 0, 0, -1, 0);  # -1 direction means no direction yet

    my %visited;

    while (!$pq->is_empty()) {
        my ($heat, $r, $c, $d, $consec) = $pq->pop();

        # Check if we reached the goal
        if ($r == $rows - 1 && $c == $cols - 1) {
            if ($min_straight == 0 || $consec >= $min_straight) {
                return $heat;
            }
        }

        my $state = "$r,$c,$d,$consec";
        next if exists $visited{$state};
        $visited{$state} = 1;

        # Try all four directions
        for my $nd (0..3) {
            # Can't reverse direction
            if ($d != -1 && $nd == ($d + 2) % 4) {
                next;
            }

            my $nr = $r + $dr[$nd];
            my $nc = $c + $dc[$nd];

            # Bounds check
            next if $nr < 0 || $nr >= $rows || $nc < 0 || $nc >= $cols;

            my $new_consec;
            if ($nd == $d) {
                # Continuing in same direction
                $new_consec = $consec + 1;
                next if $new_consec > $max_straight;
            } else {
                # Turning - must have gone min_straight in previous direction first
                next if $d != -1 && $consec < $min_straight;
                $new_consec = 1;
            }

            my $new_heat = $heat + $grid->[$nr][$nc];
            my $new_state = "$nr,$nc,$nd,$new_consec";

            unless (exists $visited{$new_state}) {
                $pq->push($new_heat, $nr, $nc, $nd, $new_consec);
            }
        }
    }

    return -1;  # No path found
}

sub part1 {
    my ($grid) = @_;
    return dijkstra($grid, 0, 3);
}

sub part2 {
    my ($grid) = @_;
    return dijkstra($grid, 4, 10);
}

sub main {
    my $dir = dirname(__FILE__);
    my $input_file = "$dir/../input.txt";
    my $grid = parse_input($input_file);

    print "Part 1: ", part1($grid), "\n";
    print "Part 2: ", part2($grid), "\n";
}

main();
