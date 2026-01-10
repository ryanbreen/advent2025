#!/usr/bin/env perl
use strict;
use warnings;

# Read input file
my $input_file = "../input.txt";
open(my $fh, '<', $input_file) or die "Cannot open $input_file: $!";
my @depths = map { chomp; $_ } <$fh>;
close($fh);

sub part1 {
    # Count the number of times a depth measurement increases from the previous
    my $count = 0;
    for my $i (1 .. $#depths) {
        $count++ if $depths[$i] > $depths[$i - 1];
    }
    return $count;
}

sub part2 {
    # Count increases in 3-measurement sliding window sums
    my @window_sums;
    for my $i (0 .. $#depths - 2) {
        push @window_sums, $depths[$i] + $depths[$i + 1] + $depths[$i + 2];
    }

    # Count how many times the sum increases
    my $count = 0;
    for my $i (1 .. $#window_sums) {
        $count++ if $window_sums[$i] > $window_sums[$i - 1];
    }
    return $count;
}

print "Part 1: " . part1() . "\n";
print "Part 2: " . part2() . "\n";
