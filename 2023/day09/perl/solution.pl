#!/usr/bin/env perl
use strict;
use warnings;
use List::Util qw(all sum);

# Compute successive differences of a sequence
sub differences {
    my $seq = shift;
    return [ map { $seq->[$_ + 1] - $seq->[$_] } 0 .. $#$seq - 1 ];
}

# Extrapolate next value: build difference pyramid, sum last elements
sub extrapolate {
    my $seq = shift;
    my @last_values = ($seq->[-1]);

    while (!all { $_ == 0 } @$seq) {
        $seq = differences($seq);
        push @last_values, $seq->[-1];
    }

    return sum(@last_values);
}

# Parse input
open my $fh, '<', '../input.txt' or die "Cannot open input: $!";
my @histories;
while (<$fh>) {
    chomp;
    next unless /\S/;
    push @histories, [ split ];
}
close $fh;

# Part 1: extrapolate forward
my $part1 = sum map { extrapolate($_) } @histories;

# Part 2: extrapolate backward = extrapolate forward on reversed sequence
my $part2 = sum map { extrapolate([ reverse @$_ ]) } @histories;

print "Part 1: $part1\n";
print "Part 2: $part2\n";
