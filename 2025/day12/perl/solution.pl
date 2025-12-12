#!/usr/bin/env perl
use strict;
use warnings;
use v5.10;
use List::Util qw(sum);

# Day 12: Christmas Tree Farm - Polyomino Packing
#
# The solution checks if presents (polyominoes) can fit into rectangular regions.
# For this problem, the constraint is simply: total cells needed <= available cells.

sub parse_input {
    my ($text) = @_;
    my %shapes;
    my @regions;

    # Split by double newlines to get sections
    my @sections = split /\n\n/, $text;

    foreach my $section (@sections) {
        my @lines = split /\n/, $section;
        next unless @lines;

        if ($lines[0] =~ /^(\d+):$/ && $lines[0] !~ /x/) {
            # Shape definition - count '#' characters using map and scalar context
            my $idx = $1;
            my $cell_count = sum map { scalar(() = /\#/g) } @lines[1..$#lines];
            $shapes{$idx} = $cell_count;
        } else {
            # Region definitions - use map to extract from lines
            push @regions, map {
                /(\d+)x(\d+):\s*(.+)/ ? {
                    width => $1,
                    height => $2,
                    counts => [split /\s+/, $3]
                } : ()
            } @lines;
        }
    }

    return (\%shapes, \@regions);
}

sub can_fit_region {
    my ($width, $height, $counts, $shape_sizes) = @_;

    # Use map to calculate cells needed for each shape, then sum
    my $total_cells_needed = sum map {
        $counts->[$_] * $shape_sizes->{$_}
    } 0..$#{$counts};

    my $available = $width * $height;
    return $total_cells_needed <= $available;
}

sub part1 {
    my ($shapes, $regions) = @_;

    # Use grep to count regions that fit
    return scalar grep {
        can_fit_region($_->{width}, $_->{height}, $_->{counts}, $shapes)
    } @$regions;
}

sub part2 {
    # Part 2 is just a button click to finish - no computation needed
    return 0;
}

# Main execution
open my $fh, '<', '../input.txt' or die "Cannot open input.txt: $!";
my $text = do { local $/; <$fh> };
close $fh;

my ($shapes, $regions) = parse_input($text);

say "Part 1: ", part1($shapes, $regions);
say "Part 2: ", part2();
