#!/usr/bin/env perl
use strict;
use warnings;
use v5.10;
use FindBin qw($RealBin);

# Keypad layouts - positions as [row, col]
my %NUMERIC = (
    '7' => [0, 0], '8' => [0, 1], '9' => [0, 2],
    '4' => [1, 0], '5' => [1, 1], '6' => [1, 2],
    '1' => [2, 0], '2' => [2, 1], '3' => [2, 2],
    '0' => [3, 1], 'A' => [3, 2],
);
my @NUMERIC_GAP = (3, 0);

my %DIRECTIONAL = (
    '^' => [0, 1], 'A' => [0, 2],
    '<' => [1, 0], 'v' => [1, 1], '>' => [1, 2],
);
my @DIRECTIONAL_GAP = (0, 0);

# Memoization cache for min_presses_for_move
my %memo;

sub shortest_paths {
    my ($keypad_ref, $gap_ref, $start, $end) = @_;

    my ($sr, $sc) = @{$keypad_ref->{$start}};
    my ($er, $ec) = @{$keypad_ref->{$end}};
    my ($gr, $gc) = @$gap_ref;

    my @paths;

    my $dfs;
    $dfs = sub {
        my ($r, $c, $path) = @_;

        # Hit gap - invalid
        return if $r == $gr && $c == $gc;

        # Reached end
        if ($r == $er && $c == $ec) {
            push @paths, $path;
            return;
        }

        # Move vertically toward target
        if ($r < $er) {
            $dfs->($r + 1, $c, $path . 'v');
        } elsif ($r > $er) {
            $dfs->($r - 1, $c, $path . '^');
        }

        # Move horizontally toward target
        if ($c < $ec) {
            $dfs->($r, $c + 1, $path . '>');
        } elsif ($c > $ec) {
            $dfs->($r, $c - 1, $path . '<');
        }
    };

    $dfs->($sr, $sc, '');

    return @paths ? @paths : ('');  # Empty path if start == end
}

sub min_presses_for_move {
    my ($from_char, $to_char, $depth, $is_numeric) = @_;

    # Check memoization cache
    my $key = "$from_char|$to_char|$depth|$is_numeric";
    return $memo{$key} if exists $memo{$key};

    my ($keypad_ref, $gap_ref);
    if ($is_numeric) {
        $keypad_ref = \%NUMERIC;
        $gap_ref = \@NUMERIC_GAP;
    } else {
        $keypad_ref = \%DIRECTIONAL;
        $gap_ref = \@DIRECTIONAL_GAP;
    }

    my @paths = shortest_paths($keypad_ref, $gap_ref, $from_char, $to_char);

    my $result;
    if ($depth == 0) {
        # At human level, just return path length + 1 for 'A' press
        my $min_len = length($paths[0]);
        for my $p (@paths) {
            my $len = length($p);
            $min_len = $len if $len < $min_len;
        }
        $result = $min_len + 1;
    } else {
        my $best = 1e100;  # Infinity approximation

        for my $path (@paths) {
            # Need to type path + 'A' on the directional keypad above
            my $sequence = $path . 'A';
            my $cost = 0;
            my $current = 'A';

            for my $i (0 .. length($sequence) - 1) {
                my $char = substr($sequence, $i, 1);
                $cost += min_presses_for_move($current, $char, $depth - 1, 0);
                $current = $char;
            }

            $best = $cost if $cost < $best;
        }

        $result = $best;
    }

    $memo{$key} = $result;
    return $result;
}

sub solve_code {
    my ($code, $depth) = @_;

    my $total = 0;
    my $current = 'A';

    for my $i (0 .. length($code) - 1) {
        my $char = substr($code, $i, 1);
        $total += min_presses_for_move($current, $char, $depth, 1);
        $current = $char;
    }

    return $total;
}

sub complexity {
    my ($code, $length) = @_;

    # Extract numeric part (remove trailing 'A')
    my $numeric_part = $code;
    $numeric_part =~ s/A$//;
    $numeric_part = int($numeric_part);

    return $length * $numeric_part;
}

sub part1 {
    my @codes = @_;

    my $total = 0;
    for my $code (@codes) {
        my $length = solve_code($code, 2);
        $total += complexity($code, $length);
    }

    return $total;
}

sub part2 {
    my @codes = @_;

    my $total = 0;
    for my $code (@codes) {
        my $length = solve_code($code, 25);
        $total += complexity($code, $length);
    }

    return $total;
}

# Main
my $input_file = "$RealBin/../input.txt";

open my $fh, '<', $input_file or die "Cannot open input file: $!";
my @codes = grep { /\S/ } map { chomp; $_ } <$fh>;
close $fh;

say 'Part 1: ', part1(@codes);
say 'Part 2: ', part2(@codes);
