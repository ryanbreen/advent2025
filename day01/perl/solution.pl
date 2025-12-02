#!/usr/bin/env perl
use strict;
use warnings;

# Read input file
my $input_file = '../input.txt';
open(my $fh, '<', $input_file) or die "Cannot open $input_file: $!";
my @lines = <$fh>;
chomp @lines;
close($fh);

# Part 1: Count times dial ends at 0 after each rotation
sub part1 {
    my @rotations = @_;
    my $position = 50;  # Starting position
    my $zeros_count = 0;

    foreach my $line (@rotations) {
        my $direction = substr($line, 0, 1);  # 'L' or 'R'
        my $distance = substr($line, 1);

        if ($direction eq 'L') {
            # Moving left (toward lower numbers)
            $position = ($position - $distance) % 100;
            if ($position < 0) {
                $position += 100;
            }
        } else {
            # Moving right (toward higher numbers)
            $position = ($position + $distance) % 100;
        }

        # Check if we landed on 0
        if ($position == 0) {
            $zeros_count++;
        }
    }

    return $zeros_count;
}

# Part 2: Count times dial points at 0 during any click
sub part2 {
    my @rotations = @_;
    my $position = 50;  # Starting position
    my $zeros_count = 0;

    foreach my $line (@rotations) {
        my $direction = substr($line, 0, 1);  # 'L' or 'R'
        my $distance = substr($line, 1);

        if ($direction eq 'L') {
            # Moving left (toward lower numbers)
            # We hit 0 when we've moved 'position' clicks
            # Then every 100 clicks after that

            if ($distance >= $position && $position > 0) {
                # We will pass through 0 at least once
                $zeros_count++;  # Count the first crossing at position 0

                # After the first crossing, how many more times?
                my $remaining_after_first_zero = $distance - $position;
                $zeros_count += int($remaining_after_first_zero / 100);
            } elsif ($position == 0 && $distance > 0) {
                # Starting at 0, going left means we immediately leave 0
                # We come back to 0 every 100 clicks
                $zeros_count += int($distance / 100);
            }

            # Calculate final position
            $position = ($position - $distance) % 100;
            if ($position < 0) {
                $position += 100;
            }
        } else {
            # Moving right (toward higher numbers)
            # We hit 0 when we've moved (100 - position) clicks
            # Then every 100 clicks after that

            if ($distance >= 100 - $position && $position > 0) {
                # We will pass through 0 at least once
                $zeros_count++;  # Count the first crossing at position 0

                # After the first crossing, how many more times?
                my $remaining_after_first_zero = $distance - (100 - $position);
                $zeros_count += int($remaining_after_first_zero / 100);
            } elsif ($position == 0 && $distance > 0) {
                # Starting at 0, going right means we immediately leave 0
                # We come back to 0 every 100 clicks
                $zeros_count += int($distance / 100);
            }

            # Calculate final position
            $position = ($position + $distance) % 100;
        }
    }

    return $zeros_count;
}

# Run both parts
print "Part 1: ", part1(@lines), "\n";
print "Part 2: ", part2(@lines), "\n";
