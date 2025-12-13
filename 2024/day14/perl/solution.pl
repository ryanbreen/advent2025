#!/usr/bin/env perl
use strict;
use warnings;
use v5.10;

my $WIDTH = 101;
my $HEIGHT = 103;

sub parse_robots {
    my ($text) = @_;
    my @robots;

    for my $line (split /\n/, $text) {
        if ($line =~ /p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)/) {
            push @robots, [$1, $2, $3, $4];  # [px, py, vx, vy]
        }
    }

    return @robots;
}

sub simulate {
    my ($robots_ref, $seconds) = @_;
    my @positions;

    for my $robot (@$robots_ref) {
        my ($px, $py, $vx, $vy) = @$robot;

        # Position after 'seconds' time, with wrapping
        my $new_x = ($px + $vx * $seconds) % $WIDTH;
        my $new_y = ($py + $vy * $seconds) % $HEIGHT;

        push @positions, [$new_x, $new_y];
    }

    return @positions;
}

sub count_quadrants {
    my (@positions) = @_;

    my $mid_x = int($WIDTH / 2);   # 50
    my $mid_y = int($HEIGHT / 2);  # 51

    my ($q1, $q2, $q3, $q4) = (0, 0, 0, 0);

    for my $pos (@positions) {
        my ($x, $y) = @$pos;

        # Skip robots on middle lines
        next if $x == $mid_x || $y == $mid_y;

        if ($x < $mid_x && $y < $mid_y) {
            $q1++;  # Top-left
        } elsif ($x > $mid_x && $y < $mid_y) {
            $q2++;  # Top-right
        } elsif ($x < $mid_x && $y > $mid_y) {
            $q3++;  # Bottom-left
        } else {
            $q4++;  # Bottom-right
        }
    }

    return ($q1, $q2, $q3, $q4);
}

sub part1 {
    my (@robots) = @_;

    my @positions = simulate(\@robots, 100);
    my ($q1, $q2, $q3, $q4) = count_quadrants(@positions);

    return $q1 * $q2 * $q3 * $q4;
}

sub part2 {
    my (@robots) = @_;

    # The Christmas tree appears when robots cluster together
    # Look for a frame with a long horizontal line of robots (tree base/border)
    for my $seconds (1 .. $WIDTH * $HEIGHT) {
        my @positions = simulate(\@robots, $seconds);

        # Create a set of positions for fast lookup
        my %pos_set;
        for my $pos (@positions) {
            my ($x, $y) = @$pos;
            $pos_set{"$x,$y"} = 1;
        }

        # Look for a horizontal line of at least 20 consecutive robots
        for my $y (0 .. $HEIGHT - 1) {
            my $max_consecutive = 0;
            my $consecutive = 0;

            for my $x (0 .. $WIDTH - 1) {
                if (exists $pos_set{"$x,$y"}) {
                    $consecutive++;
                    $max_consecutive = $consecutive if $consecutive > $max_consecutive;
                } else {
                    $consecutive = 0;
                }
            }

            return $seconds if $max_consecutive >= 20;
        }
    }

    return -1;
}

# Main
my $input_file = '../input.txt';
open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
my $input_text = do { local $/; <$fh> };
close $fh;

chomp $input_text;

my @robots = parse_robots($input_text);

say "Part 1: ", part1(@robots);
say "Part 2: ", part2(@robots);
