#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;

sub sign {
    my ($x) = @_;
    return $x > 0 ? 1 : $x < 0 ? -1 : 0;
}

sub parse_input {
    my $dir = dirname(__FILE__);
    my $input_path = "$dir/../input.txt";

    open(my $fh, '<', $input_path) or die "Cannot open $input_path: $!";

    my @lines;
    while (my $line = <$fh>) {
        chomp $line;
        next if $line eq '';

        if ($line =~ /(\d+),(\d+)\s*->\s*(\d+),(\d+)/) {
            push @lines, [$1, $2, $3, $4];
        }
    }

    close($fh);
    return @lines;
}

sub count_overlaps {
    my ($lines_ref, $include_diagonals) = @_;
    my %grid;

    for my $line (@$lines_ref) {
        my ($x1, $y1, $x2, $y2) = @$line;
        my $dx = sign($x2 - $x1);
        my $dy = sign($y2 - $y1);

        # Skip diagonals in part 1
        next if (!$include_diagonals && $dx != 0 && $dy != 0);

        my ($x, $y) = ($x1, $y1);
        while (1) {
            $grid{"$x,$y"}++;
            last if ($x == $x2 && $y == $y2);
            $x += $dx;
            $y += $dy;
        }
    }

    my $count = 0;
    for my $v (values %grid) {
        $count++ if $v >= 2;
    }
    return $count;
}

sub part1 {
    my @lines = @_;
    return count_overlaps(\@lines, 0);
}

sub part2 {
    my @lines = @_;
    return count_overlaps(\@lines, 1);
}

my @lines = parse_input();
print "Part 1: ", part1(@lines), "\n";
print "Part 2: ", part2(@lines), "\n";
