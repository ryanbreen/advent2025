#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;

# Read input file
my $input_path = dirname(__FILE__) . '/../input.txt';
open(my $fh, '<', $input_path) or die "Cannot open $input_path: $!";
my $input_text = do { local $/; <$fh> };
close($fh);
$input_text =~ s/^\s+|\s+$//g;

# Parse input into two lists
my @left_list;
my @right_list;

foreach my $line (split(/\n/, $input_text)) {
    my ($left, $right) = split(/\s+/, $line);
    push @left_list, int($left);
    push @right_list, int($right);
}

sub part1 {
    my ($left_ref, $right_ref) = @_;

    # Sort both lists
    my @sorted_left = sort { $a <=> $b } @$left_ref;
    my @sorted_right = sort { $a <=> $b } @$right_ref;

    # Calculate total distance
    my $total_distance = 0;
    for (my $i = 0; $i < @sorted_left; $i++) {
        $total_distance += abs($sorted_left[$i] - $sorted_right[$i]);
    }

    return $total_distance;
}

sub part2 {
    my ($left_ref, $right_ref) = @_;

    # Count occurrences in right list
    my %right_counts;
    foreach my $num (@$right_ref) {
        $right_counts{$num}++;
    }

    # Calculate similarity score
    my $similarity_score = 0;
    foreach my $num (@$left_ref) {
        my $count = $right_counts{$num} // 0;
        $similarity_score += $num * $count;
    }

    return $similarity_score;
}

print "Part 1: " . part1(\@left_list, \@right_list) . "\n";
print "Part 2: " . part2(\@left_list, \@right_list) . "\n";
