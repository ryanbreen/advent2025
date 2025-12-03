#!/usr/bin/env perl
use strict;
use warnings;
use v5.10;

# Read input file
my $input_file = '../input.txt';
open(my $fh, '<', $input_file) or die "Cannot open $input_file: $!";
my @lines = <$fh>;
chomp @lines;
close($fh);

sub part1 {
    my @lines = @_;
    my $total = 0;

    foreach my $line (@lines) {
        my @digits = split //, $line;
        my $n = scalar @digits;

        # Precompute max suffix: max_suffix[i] = max digit from position i to end
        my @max_suffix;
        $max_suffix[$n - 1] = $digits[$n - 1];
        for (my $i = $n - 2; $i >= 0; $i--) {
            $max_suffix[$i] = $digits[$i] > $max_suffix[$i + 1] ? $digits[$i] : $max_suffix[$i + 1];
        }

        my $max_joltage = 0;
        # For each possible first battery position
        for (my $i = 0; $i < $n - 1; $i++) {
            my $first_digit = $digits[$i];
            # The maximum second digit is the max from position i+1 onwards
            my $max_second = $max_suffix[$i + 1];
            my $joltage = $first_digit * 10 + $max_second;
            $max_joltage = $joltage if $joltage > $max_joltage;
        }

        $total += $max_joltage;
    }

    return $total;
}

sub part2 {
    my @lines = @_;
    my $total = 0;

    foreach my $line (@lines) {
        my @digits = split //, $line;
        my $n = scalar @digits;
        my $k = 12;  # Select exactly 12 batteries

        # Greedy algorithm to select k digits that form the maximum number
        my @result;
        my $current_pos = 0;

        for (my $i = 0; $i < $k; $i++) {
            # How many digits we still need to select after this one
            my $remaining_needed = $k - $i - 1;
            # Latest position we can start searching from
            my $search_end = $n - $remaining_needed;

            # Find the maximum digit in the valid range
            my $max_digit = -1;
            my $max_pos = $current_pos;
            for (my $j = $current_pos; $j < $search_end; $j++) {
                my $digit = $digits[$j];
                if ($digit > $max_digit) {
                    $max_digit = $digit;
                    $max_pos = $j;
                }
            }

            push @result, $max_digit;
            $current_pos = $max_pos + 1;
        }

        my $joltage = join('', @result);
        $total += $joltage;
    }

    return $total;
}

# Run both parts
say "Part 1: " . part1(@lines);
say "Part 2: " . part2(@lines);
