#!/usr/bin/env perl
use strict;
use warnings;

# Read input file
my $input_file = '../input.txt';
open(my $fh, '<', $input_file) or die "Cannot open $input_file: $!";
my $input = do { local $/; <$fh> };
close($fh);
chomp $input;

# Check if a number is invalid (pattern repeated exactly twice)
sub is_invalid_id {
    my ($num) = @_;
    my $str = "$num";
    my $len = length($str);

    # Must be even length to be repeated twice
    return 0 if $len % 2 != 0;

    my $half_len = $len / 2;
    my $first_half = substr($str, 0, $half_len);
    my $second_half = substr($str, $half_len);

    # Check if both halves are identical
    return $first_half eq $second_half;
}

# Check if a number is invalid (pattern repeated at least twice)
sub is_invalid_id_part2 {
    my ($num) = @_;
    my $str = "$num";
    my $len = length($str);

    # Try all possible pattern lengths from 1 to len/2
    for (my $pattern_len = 1; $pattern_len <= $len / 2; $pattern_len++) {
        # Check if the string can be divided evenly by this pattern length
        if ($len % $pattern_len == 0) {
            my $pattern = substr($str, 0, $pattern_len);
            my $repetitions = $len / $pattern_len;

            # Check if repeating the pattern creates the full string
            if ($repetitions >= 2 && $pattern x $repetitions eq $str) {
                return 1;
            }
        }
    }

    return 0;
}

# Part 1: Sum of invalid IDs where pattern repeated exactly twice
sub part1 {
    my ($input) = @_;

    # Parse the comma-separated ranges
    my @range_strs = split(/,/, $input);
    my @ranges = map {
        my ($start, $end) = split(/-/, $_);
        { start => $start, end => $end }
    } @range_strs;

    my $sum = 0;

    # Check each range for invalid IDs
    foreach my $range (@ranges) {
        for (my $id = $range->{start}; $id <= $range->{end}; $id++) {
            if (is_invalid_id($id)) {
                $sum += $id;
            }
        }
    }

    return $sum;
}

# Part 2: Sum of invalid IDs where pattern repeated at least twice
sub part2 {
    my ($input) = @_;

    # Parse the comma-separated ranges
    my @range_strs = split(/,/, $input);
    my @ranges = map {
        my ($start, $end) = split(/-/, $_);
        { start => $start, end => $end }
    } @range_strs;

    my $sum = 0;

    # Check each range for invalid IDs
    foreach my $range (@ranges) {
        for (my $id = $range->{start}; $id <= $range->{end}; $id++) {
            if (is_invalid_id_part2($id)) {
                $sum += $id;
            }
        }
    }

    return $sum;
}

# Run both parts
print "Part 1: ", part1($input), "\n";
print "Part 2: ", part2($input), "\n";
