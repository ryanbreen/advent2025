#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;

sub parse_input {
    my $dir = dirname(__FILE__);
    my $input_path = "$dir/../input.txt";
    open my $fh, '<', $input_path or die "Cannot open $input_path: $!";
    my @numbers;
    while (<$fh>) {
        chomp;
        push @numbers, $_ if /\S/;
    }
    close $fh;
    return @numbers;
}

sub part1 {
    my @numbers = @_;
    my $num_bits = length($numbers[0]);
    my $gamma = 0;

    for my $pos (0 .. $num_bits - 1) {
        my $ones = 0;
        for my $n (@numbers) {
            $ones++ if substr($n, $pos, 1) eq '1';
        }
        my $zeros = scalar(@numbers) - $ones;

        if ($ones >= $zeros) {
            $gamma |= (1 << ($num_bits - 1 - $pos));
        }
    }

    # epsilon is bitwise NOT of gamma (within num_bits)
    my $epsilon = $gamma ^ ((1 << $num_bits) - 1);

    return $gamma * $epsilon;
}

sub find_rating {
    my ($numbers_ref, $use_most_common) = @_;
    my @candidates = @$numbers_ref;
    my $num_bits = length($candidates[0]);

    for my $pos (0 .. $num_bits - 1) {
        last if scalar(@candidates) == 1;

        my $ones = 0;
        for my $n (@candidates) {
            $ones++ if substr($n, $pos, 1) eq '1';
        }
        my $zeros = scalar(@candidates) - $ones;

        my $target;
        if ($use_most_common) {
            $target = ($ones >= $zeros) ? '1' : '0';
        } else {
            $target = ($zeros <= $ones) ? '0' : '1';
        }

        @candidates = grep { substr($_, $pos, 1) eq $target } @candidates;
    }

    return oct("0b" . $candidates[0]);
}

sub part2 {
    my @numbers = @_;
    my $oxygen = find_rating(\@numbers, 1);
    my $co2 = find_rating(\@numbers, 0);
    return $oxygen * $co2;
}

my @numbers = parse_input();
print "Part 1: " . part1(@numbers) . "\n";
print "Part 2: " . part2(@numbers) . "\n";
