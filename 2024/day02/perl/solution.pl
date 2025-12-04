#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;
use File::Spec;

my $input_path = File::Spec->catfile(dirname(__FILE__), '..', 'input.txt');
open my $fh, '<', $input_path or die "Cannot open $input_path: $!";
my @lines = <$fh>;
close $fh;
chomp @lines;

# Parse reports
my @reports;
foreach my $line (@lines) {
    my @levels = split ' ', $line;
    push @reports, \@levels;
}

sub is_safe {
    my ($levels_ref) = @_;
    my @levels = @$levels_ref;

    return 0 if @levels < 2;

    my @diffs;
    for (my $i = 0; $i < @levels - 1; $i++) {
        push @diffs, $levels[$i + 1] - $levels[$i];
    }

    # All increasing or all decreasing
    my $all_increasing = 1;
    my $all_decreasing = 1;
    foreach my $d (@diffs) {
        $all_increasing = 0 if $d <= 0;
        $all_decreasing = 0 if $d >= 0;
    }

    return 0 unless $all_increasing || $all_decreasing;

    # All diffs must be 1-3 in absolute value
    foreach my $d (@diffs) {
        return 0 if abs($d) < 1 || abs($d) > 3;
    }

    return 1;
}

sub is_safe_with_dampener {
    my ($levels_ref) = @_;
    my @levels = @$levels_ref;

    # Already safe without removing anything
    return 1 if is_safe(\@levels);

    # Try removing each level one at a time
    for (my $i = 0; $i < @levels; $i++) {
        my @modified = (@levels[0..$i-1], @levels[$i+1..$#levels]);
        return 1 if is_safe(\@modified);
    }

    return 0;
}

sub part1 {
    my $count = 0;
    foreach my $report_ref (@reports) {
        $count++ if is_safe($report_ref);
    }
    return $count;
}

sub part2 {
    my $count = 0;
    foreach my $report_ref (@reports) {
        $count++ if is_safe_with_dampener($report_ref);
    }
    return $count;
}

print "Part 1: " . part1() . "\n";
print "Part 2: " . part2() . "\n";
