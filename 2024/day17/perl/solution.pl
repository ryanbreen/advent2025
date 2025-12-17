#!/usr/bin/env perl
# Day 17: Chronospatial Computer - 3-bit VM emulator
use strict;
use warnings;
use feature 'say';
use FindBin qw($RealBin);

# Parse input file
sub parse_input {
    my ($filename) = @_;
    open my $fh, '<', $filename or die "Cannot open $filename: $!";
    my $text = do { local $/; <$fh> };
    close $fh;

    my ($a) = $text =~ /Register A: (\d+)/;
    my ($b) = $text =~ /Register B: (\d+)/;
    my ($c) = $text =~ /Register C: (\d+)/;
    my ($prog_str) = $text =~ /Program: ([\d,]+)/;
    my @program = split /,/, $prog_str;

    return ($a, $b, $c, \@program);
}

# Execute the 3-bit computer program and return output
sub run_program {
    my ($a, $b, $c, $program) = @_;
    my $ip = 0;
    my @output;

    # Get combo operand value
    my $combo = sub {
        my ($operand) = @_;
        return $operand if $operand <= 3;
        return $a if $operand == 4;
        return $b if $operand == 5;
        return $c if $operand == 6;
        die "Invalid combo operand: $operand";
    };

    while ($ip < scalar(@$program)) {
        my $opcode = $program->[$ip];
        my $operand = $program->[$ip + 1];

        if ($opcode == 0) {         # adv - A = A >> combo
            $a = $a >> $combo->($operand);
        }
        elsif ($opcode == 1) {      # bxl - B = B XOR literal
            $b = $b ^ $operand;
        }
        elsif ($opcode == 2) {      # bst - B = combo % 8
            $b = $combo->($operand) & 7;
        }
        elsif ($opcode == 3) {      # jnz - jump if A != 0
            if ($a != 0) {
                $ip = $operand;
                next;
            }
        }
        elsif ($opcode == 4) {      # bxc - B = B XOR C
            $b = $b ^ $c;
        }
        elsif ($opcode == 5) {      # out - output combo % 8
            push @output, $combo->($operand) & 7;
        }
        elsif ($opcode == 6) {      # bdv - B = A >> combo
            $b = $a >> $combo->($operand);
        }
        elsif ($opcode == 7) {      # cdv - C = A >> combo
            $c = $a >> $combo->($operand);
        }

        $ip += 2;
    }

    return \@output;
}

# Part 1: Run the program and return comma-separated output
sub part1 {
    my ($a, $b, $c, $program) = @_;
    my $output = run_program($a, $b, $c, $program);
    return join(',', @$output);
}

# Part 2: Find initial A value that makes program output itself
sub part2 {
    my ($a, $b, $c, $program) = @_;
    my @prog = @$program;
    my $prog_len = scalar(@prog);

    # Recursive search - work backwards from the last digit
    # Build A 3 bits at a time
    my $search;
    $search = sub {
        my ($target_idx, $current_a) = @_;

        # Found complete solution
        return $current_a if $target_idx < 0;

        # Try all 8 possible 3-bit values for this position
        for my $bits (0..7) {
            my $candidate_a = ($current_a << 3) | $bits;

            # A can't be 0 at start (would halt immediately)
            next if $candidate_a == 0 && $target_idx == $prog_len - 1;

            my $output = run_program($candidate_a, $b, $c, $program);

            # Check if output matches the suffix of the program
            my @expected = @prog[$target_idx .. $#prog];

            if (arrays_equal($output, \@expected)) {
                my $result = $search->($target_idx - 1, $candidate_a);
                return $result if defined $result;
            }
        }

        return undef;
    };

    return $search->($prog_len - 1, 0);
}

# Helper to compare two arrays
sub arrays_equal {
    my ($a, $b) = @_;
    return 0 if scalar(@$a) != scalar(@$b);
    for my $i (0 .. $#$a) {
        return 0 if $a->[$i] != $b->[$i];
    }
    return 1;
}

# Main
my $input_file = "$RealBin/../input.txt";

my ($a, $b, $c, $program) = parse_input($input_file);

say "Part 1: ", part1($a, $b, $c, $program);
say "Part 2: ", part2($a, $b, $c, $program);
