#!/usr/bin/env perl
use strict;
use warnings;
use v5.10;

# Read input
my $input_file = "../input.txt";
open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
my @grid = map { chomp; [split //] } <$fh>;
close $fh;

my $rows = scalar @grid;
my $cols = scalar @{$grid[0]};

sub find_regions {
    my %visited;
    my @regions;

    for my $r (0 .. $rows - 1) {
        for my $c (0 .. $cols - 1) {
            my $key = "$r,$c";
            next if exists $visited{$key};

            # BFS to find all cells in this region
            my $plant = $grid[$r][$c];
            my %region;
            my @queue = ([$r, $c]);

            while (@queue) {
                my $cell = shift @queue;
                my ($cr, $cc) = @$cell;
                my $cell_key = "$cr,$cc";

                next if exists $visited{$cell_key};
                next if $cr < 0 || $cr >= $rows || $cc < 0 || $cc >= $cols;
                next if $grid[$cr][$cc] ne $plant;

                $visited{$cell_key} = 1;
                $region{$cell_key} = 1;

                # Add neighbors
                for my $dir ([0, 1], [0, -1], [1, 0], [-1, 0]) {
                    my ($dr, $dc) = @$dir;
                    my $nr = $cr + $dr;
                    my $nc = $cc + $dc;
                    my $next_key = "$nr,$nc";
                    push @queue, [$nr, $nc] unless exists $visited{$next_key};
                }
            }

            push @regions, \%region;
        }
    }

    return @regions;
}

sub calculate_perimeter {
    my ($region) = @_;
    my $perimeter = 0;

    for my $cell (keys %$region) {
        my ($r, $c) = split /,/, $cell;
        for my $dir ([0, 1], [0, -1], [1, 0], [-1, 0]) {
            my ($dr, $dc) = @$dir;
            my $nr = $r + $dr;
            my $nc = $c + $dc;
            my $neighbor = "$nr,$nc";
            $perimeter++ unless exists $region->{$neighbor};
        }
    }

    return $perimeter;
}

sub count_sides {
    my ($region) = @_;
    my $corners = 0;

    for my $cell (keys %$region) {
        my ($r, $c) = split /,/, $cell;

        # Check all 4 corners of this cell
        # Each corner is defined by checking two orthogonal neighbors and the diagonal
        # Convex: both orthogonal out
        # Concave: both orthogonal in, diagonal out

        my $up = exists $region->{"" . ($r - 1) . ",$c"};
        my $down = exists $region->{"" . ($r + 1) . ",$c"};
        my $left = exists $region->{"$r," . ($c - 1)};
        my $right = exists $region->{"$r," . ($c + 1)};
        my $up_left = exists $region->{"" . ($r - 1) . "," . ($c - 1)};
        my $up_right = exists $region->{"" . ($r - 1) . "," . ($c + 1)};
        my $down_left = exists $region->{"" . ($r + 1) . "," . ($c - 1)};
        my $down_right = exists $region->{"" . ($r + 1) . "," . ($c + 1)};

        # Top-left corner
        if (!$up && !$left) {  # convex
            $corners++;
        } elsif ($up && $left && !$up_left) {  # concave
            $corners++;
        }

        # Top-right corner
        if (!$up && !$right) {  # convex
            $corners++;
        } elsif ($up && $right && !$up_right) {  # concave
            $corners++;
        }

        # Bottom-left corner
        if (!$down && !$left) {  # convex
            $corners++;
        } elsif ($down && $left && !$down_left) {  # concave
            $corners++;
        }

        # Bottom-right corner
        if (!$down && !$right) {  # convex
            $corners++;
        } elsif ($down && $right && !$down_right) {  # concave
            $corners++;
        }
    }

    return $corners;
}

sub part1 {
    my @regions = find_regions();
    my $total = 0;
    for my $region (@regions) {
        my $area = scalar keys %$region;
        my $perimeter = calculate_perimeter($region);
        $total += $area * $perimeter;
    }
    return $total;
}

sub part2 {
    my @regions = find_regions();
    my $total = 0;
    for my $region (@regions) {
        my $area = scalar keys %$region;
        my $sides = count_sides($region);
        $total += $area * $sides;
    }
    return $total;
}

say "Part 1: " . part1();
say "Part 2: " . part2();
