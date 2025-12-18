#!/usr/bin/env perl
use strict;
use warnings;

sub parse_input {
    my ($filename) = @_;
    my @positions;
    open my $fh, '<', $filename or die "Cannot open $filename: $!";
    while (my $line = <$fh>) {
        chomp $line;
        next unless $line =~ /\S/;
        my ($x, $y) = split /,/, $line;
        push @positions, [$x, $y];
    }
    close $fh;
    return \@positions;
}

sub bfs {
    my ($corrupted, $size) = @_;
    $size //= 71;

    my $start = "0,0";
    my $goal = ($size - 1) . "," . ($size - 1);

    return -1 if exists $corrupted->{$start} || exists $corrupted->{$goal};

    my @queue = ([$start, 0]);
    my %visited = ($start => 1);
    my @directions = ([0, 1], [0, -1], [1, 0], [-1, 0]);

    while (@queue) {
        my $item = shift @queue;
        my ($pos, $steps) = @$item;

        return $steps if $pos eq $goal;

        my ($x, $y) = split /,/, $pos;

        for my $dir (@directions) {
            my ($dx, $dy) = @$dir;
            my $nx = $x + $dx;
            my $ny = $y + $dy;
            my $npos = "$nx,$ny";

            if ($nx >= 0 && $nx < $size && $ny >= 0 && $ny < $size &&
                !exists $visited{$npos} && !exists $corrupted->{$npos}) {
                $visited{$npos} = 1;
                push @queue, [$npos, $steps + 1];
            }
        }
    }

    return -1;
}

sub part1 {
    my ($positions, $num_bytes, $size) = @_;
    $num_bytes //= 1024;
    $size //= 71;

    my %corrupted;
    for my $i (0 .. $num_bytes - 1) {
        my $pos = $positions->[$i];
        my $key = "$pos->[0],$pos->[1]";
        $corrupted{$key} = 1;
    }

    return bfs(\%corrupted, $size);
}

sub part2 {
    my ($positions, $size) = @_;
    $size //= 71;

    my $left = 0;
    my $right = scalar(@$positions);

    while ($left < $right) {
        my $mid = int(($left + $right) / 2);

        my %corrupted;
        for my $i (0 .. $mid) {
            my $pos = $positions->[$i];
            my $key = "$pos->[0],$pos->[1]";
            $corrupted{$key} = 1;
        }

        if (bfs(\%corrupted, $size) == -1) {
            $right = $mid;
        } else {
            $left = $mid + 1;
        }
    }

    my $blocking_pos = $positions->[$left];
    return "$blocking_pos->[0],$blocking_pos->[1]";
}

my $positions = parse_input("../input.txt");

print "Part 1: ", part1($positions), "\n";
print "Part 2: ", part2($positions), "\n";
