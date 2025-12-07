#!/usr/bin/env perl
use strict;
use warnings;

# Read input file
my $input_file = '../input.txt';
open(my $fh, '<', $input_file) or die "Cannot open $input_file: $!";
my $input = do { local $/; <$fh> };
close($fh);
chomp $input;

# Parse ranges once - store as array of [start, end] pairs
my @ranges;
for my $part (split /,/, $input) {
    my ($start, $end) = split /-/, $part;
    push @ranges, [$start, $end] if defined $end;
}

# Part 1: Sum of invalid IDs where pattern repeated exactly twice
# Direct substr comparison is faster for the simple "exactly twice" case
sub part1 {
    my $sum = 0;

    for my $range (@ranges) {
        my ($start, $end) = @$range;
        for my $id ($start .. $end) {
            my $str = "$id";
            my $len = length($str);
            next if $len & 1;  # Skip odd length

            my $half = $len >> 1;
            $sum += $id if substr($str, 0, $half) eq substr($str, $half);
        }
    }

    return $sum;
}

# Part 2: Sum of invalid IDs where pattern repeated at least twice
# Regex with backreference is highly optimized in Perl
sub part2 {
    my $sum = 0;

    for my $range (@ranges) {
        my ($start, $end) = @$range;
        for my $id ($start .. $end) {
            $sum += $id if "$id" =~ /^(.+)\1+$/;
        }
    }

    return $sum;
}

# Run both parts
print "Part 1: ", part1(), "\n";
print "Part 2: ", part2(), "\n";
