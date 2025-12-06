#!/usr/bin/env perl
use strict;
use warnings;
use v5.10;  # Enable 'say' feature
use File::Basename;
use File::Spec;

# Read input file
my $script_dir = dirname(__FILE__);
my $input_file = File::Spec->catfile($script_dir, '..', 'input.txt');
open(my $fh, '<', $input_file) or die "Cannot open $input_file: $!";
my @lines = <$fh>;
chomp @lines;
close($fh);

# Word to digit mapping
my %WORDS = (
    one   => '1',
    two   => '2',
    three => '3',
    four  => '4',
    five  => '5',
    six   => '6',
    seven => '7',
    eight => '8',
    nine  => '9'
);

sub part1 {
    my ($lines) = @_;
    my $total = 0;

    foreach my $line (@$lines) {
        # Extract all digits using regex
        my @digits = $line =~ /\d/g;
        if (@digits) {
            $total += $digits[0] . $digits[-1];
        }
    }

    return $total;
}

sub part2 {
    my ($lines) = @_;
    my $total = 0;

    # Build regex pattern with lookahead for overlapping matches
    my $pattern = '(?=(\d|' . join('|', keys %WORDS) . '))';

    foreach my $line (@$lines) {
        my @matches;

        # Use lookahead to find all overlapping matches
        while ($line =~ /$pattern/g) {
            push @matches, $1;
        }

        if (@matches) {
            # Convert first and last matches to digits
            my $first = $matches[0] =~ /\d/ ? $matches[0] : $WORDS{$matches[0]};
            my $last = $matches[-1] =~ /\d/ ? $matches[-1] : $WORDS{$matches[-1]};
            $total += $first . $last;
        }
    }

    return $total;
}

# Run both parts
say "Part 1: " . part1(\@lines);
say "Part 2: " . part2(\@lines);
