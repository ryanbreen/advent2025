#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;
use File::Spec;

# SNAFU digit values: 2, 1, 0, -, =
my %digit_values = (
    '2' => 2,
    '1' => 1,
    '0' => 0,
    '-' => -1,
    '=' => -2,
);

sub snafu_to_decimal {
    my ($s) = @_;
    my $result = 0;
    for my $char (split //, $s) {
        $result = $result * 5 + $digit_values{$char};
    }
    return $result;
}

sub decimal_to_snafu {
    my ($n) = @_;
    return '0' if $n == 0;

    my @digits;
    while ($n) {
        my $remainder = $n % 5;
        if ($remainder <= 2) {
            push @digits, $remainder;
            $n = int($n / 5);
        } elsif ($remainder == 3) {
            push @digits, '=';
            $n = int($n / 5) + 1;
        } else {  # remainder == 4
            push @digits, '-';
            $n = int($n / 5) + 1;
        }
    }

    return join '', reverse @digits;
}

sub part1 {
    my ($text) = @_;
    my @lines = grep { /\S/ } split /\n/, $text;
    my $total = 0;
    for my $line (@lines) {
        $total += snafu_to_decimal($line);
    }
    return decimal_to_snafu($total);
}

sub main {
    my $script_dir = dirname(__FILE__);
    my $input_file = File::Spec->catfile($script_dir, '..', 'input.txt');

    open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
    my $text = do { local $/; <$fh> };
    close $fh;

    print "Part 1: ", part1($text), "\n";
    print "Part 2: No Part 2 on Day 25!\n";
}

main();
