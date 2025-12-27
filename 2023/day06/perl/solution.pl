#!/usr/bin/env perl
use strict;
use warnings;
use POSIX qw(floor ceil);

# Read input
my $input_file = '../input.txt';
open(my $fh, '<', $input_file) or die "Cannot open $input_file: $!";
my @lines = <$fh>;
close($fh);
chomp(@lines);

sub parse_races {
    my ($time_line, $dist_line) = @lines;

    my @times = ($time_line =~ /\d+/g);
    my @distances = ($dist_line =~ /\d+/g);

    my @races;
    for my $i (0 .. $#times) {
        push @races, [$times[$i], $distances[$i]];
    }
    return @races;
}

sub count_ways_to_win {
    my ($time, $record) = @_;

    # If we hold the button for t ms, we travel t * (time - t) mm.
    # We need: t * (time - t) > record
    # Solving: -t^2 + time*t - record > 0
    # Roots: t = (time +/- sqrt(time^2 - 4*record)) / 2

    my $discriminant = $time * $time - 4 * $record;
    return 0 if $discriminant <= 0;

    my $sqrt_d = sqrt($discriminant);
    my $t_low = ($time - $sqrt_d) / 2;
    my $t_high = ($time + $sqrt_d) / 2;

    # We need integer values strictly between the roots
    my $first = floor($t_low) + 1;
    my $last = ceil($t_high) - 1;

    return 0 if $last < $first;
    return $last - $first + 1;
}

sub part1 {
    my @races = parse_races();
    my $result = 1;

    for my $race (@races) {
        my ($time, $record) = @$race;
        my $ways = count_ways_to_win($time, $record);
        $result *= $ways;
    }

    return $result;
}

sub part2 {
    my @races = parse_races();

    # Concatenate all times and distances into single numbers
    my $time = join('', map { $_->[0] } @races);
    my $record = join('', map { $_->[1] } @races);

    return count_ways_to_win($time, $record);
}

print "Part 1: " . part1() . "\n";
print "Part 2: " . part2() . "\n";
