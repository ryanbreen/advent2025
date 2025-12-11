#!/usr/bin/env perl
use strict;
use warnings;
use v5.10;  # Enables say

sub in_bounds {
    my ($r, $c, $rows, $cols) = @_;
    return $r >= 0 && $r < $rows && $c >= 0 && $c < $cols;
}

sub parse_input {
    my ($filename) = @_;

    # Read grid and remove trailing newlines
    open my $fh, '<', $filename or die "Cannot open $filename: $!";
    chomp(my @grid = <$fh>);
    close $fh;

    my $rows = scalar @grid;
    my $cols = length($grid[0]);

    # Group antenna positions by frequency
    my %antennas;
    for my $r (0..$rows-1) {
        my @chars = split //, $grid[$r];
        for my $c (0..$#chars) {
            my $ch = $chars[$c];
            if ($ch ne '.') {
                push @{$antennas{$ch}}, [$r, $c];
            }
        }
    }

    return ($rows, $cols, \%antennas);
}

sub part1 {
    my ($rows, $cols, $antennas) = parse_input('../input.txt');

    # Use hash as set for unique antinode locations
    my %antinodes;

    # For each frequency
    for my $freq (keys %$antennas) {
        my @positions = @{$antennas->{$freq}};

        # For each pair of antennas with same frequency
        for my $i (0..$#positions) {
            for my $j ($i+1..$#positions) {
                my ($r1, $c1) = @{$positions[$i]};
                my ($r2, $c2) = @{$positions[$j]};

                # Calculate the two antinodes
                # Antinode beyond antenna 1 (away from antenna 2)
                my $ar1 = 2*$r1 - $r2;
                my $ac1 = 2*$c1 - $c2;

                # Antinode beyond antenna 2 (away from antenna 1)
                my $ar2 = 2*$r2 - $r1;
                my $ac2 = 2*$c2 - $c1;

                # Add if within bounds
                if (in_bounds($ar1, $ac1, $rows, $cols)) {
                    $antinodes{"$ar1,$ac1"} = 1;
                }
                if (in_bounds($ar2, $ac2, $rows, $cols)) {
                    $antinodes{"$ar2,$ac2"} = 1;
                }
            }
        }
    }

    return scalar keys %antinodes;
}

sub part2 {
    my ($rows, $cols, $antennas) = parse_input('../input.txt');

    # Use hash as set for unique antinode locations
    my %antinodes;

    # For each frequency
    for my $freq (keys %$antennas) {
        my @positions = @{$antennas->{$freq}};

        # For each pair of antennas with same frequency
        for my $i (0..$#positions) {
            for my $j ($i+1..$#positions) {
                my ($r1, $c1) = @{$positions[$i]};
                my ($r2, $c2) = @{$positions[$j]};

                my $dr = $r2 - $r1;
                my $dc = $c2 - $c1;

                # Extend in both directions along the line
                # Direction 1: from antenna 1 towards and beyond antenna 2
                my ($r, $c) = ($r1, $c1);
                while (in_bounds($r, $c, $rows, $cols)) {
                    $antinodes{"$r,$c"} = 1;
                    $r += $dr;
                    $c += $dc;
                }

                # Direction 2: from antenna 1 away from antenna 2
                ($r, $c) = ($r1 - $dr, $c1 - $dc);
                while (in_bounds($r, $c, $rows, $cols)) {
                    $antinodes{"$r,$c"} = 1;
                    $r -= $dr;
                    $c -= $dc;
                }
            }
        }
    }

    return scalar keys %antinodes;
}

say 'Part 1: ', part1();
say 'Part 2: ', part2();
