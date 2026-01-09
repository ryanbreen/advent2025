#!/usr/bin/env perl
use strict;
use warnings;

# Get script directory and input file path
use File::Basename;
use File::Spec;

my $script_dir = dirname(__FILE__);
my $input_file = File::Spec->catfile($script_dir, '..', 'input.txt');

# Parse input and compute results
my ($part1, $part2) = (0, 0);

open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
while (my $line = <$fh>) {
    chomp $line;
    next unless $line;

    # Parse line like "2-4,6-8"
    my ($a1, $b1, $a2, $b2) = $line =~ /(\d+)-(\d+),(\d+)-(\d+)/;

    # Part 1: Check if one range fully contains the other
    if (($a1 <= $a2 && $b1 >= $b2) || ($a2 <= $a1 && $b2 >= $b1)) {
        $part1++;
    }

    # Part 2: Check if ranges overlap at all
    if ($a1 <= $b2 && $a2 <= $b1) {
        $part2++;
    }
}
close $fh;

print "Part 1: $part1\n";
print "Part 2: $part2\n";
