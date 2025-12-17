#!/usr/bin/env perl
use strict;
use warnings;
use v5.10;

# Day 16: Reindeer Maze - Weighted shortest path with turn costs

# Directions: 0=East, 1=South, 2=West, 3=North
my @DX = (1, 0, -1, 0);
my @DY = (0, 1, 0, -1);

sub parse_input {
    my ($text) = @_;
    my @grid = map { [split //] } split /\n/, $text;
    my ($start, $end);

    for my $y (0 .. $#grid) {
        for my $x (0 .. $#{$grid[$y]}) {
            if ($grid[$y][$x] eq 'S') {
                $start = [$x, $y];
            } elsif ($grid[$y][$x] eq 'E') {
                $end = [$x, $y];
            }
        }
    }

    return (\@grid, $start, $end);
}

sub dijkstra_forward {
    my ($grid, $start) = @_;
    my @pq = ([0, $start->[0], $start->[1], 0]);  # Start facing East
    my %dist;

    while (@pq) {
        # Extract minimum by cost
        @pq = sort { $a->[0] <=> $b->[0] } @pq;
        my ($cost, $x, $y, $d) = @{shift @pq};

        my $state = "$x,$y,$d";
        next if exists $dist{$state};
        $dist{$state} = $cost;

        # Move forward
        my $nx = $x + $DX[$d];
        my $ny = $y + $DY[$d];
        if ($ny >= 0 && $ny < @$grid && $nx >= 0 && $nx < @{$grid->[$ny]} &&
            $grid->[$ny][$nx] ne '#') {
            push @pq, [$cost + 1, $nx, $ny, $d];
        }

        # Turn left/right
        push @pq, [$cost + 1000, $x, $y, ($d - 1) % 4];
        push @pq, [$cost + 1000, $x, $y, ($d + 1) % 4];
    }

    return \%dist;
}

sub dijkstra_backward {
    my ($grid, $end) = @_;
    # At end, we can arrive facing any direction
    my @pq = map { [0, $end->[0], $end->[1], $_] } (0..3);
    my %dist;

    while (@pq) {
        # Extract minimum by cost
        @pq = sort { $a->[0] <=> $b->[0] } @pq;
        my ($cost, $x, $y, $d) = @{shift @pq};

        my $state = "$x,$y,$d";
        next if exists $dist{$state};
        $dist{$state} = $cost;

        # Reverse of "move forward": come from behind
        my $px = $x - $DX[$d];
        my $py = $y - $DY[$d];
        if ($py >= 0 && $py < @$grid && $px >= 0 && $px < @{$grid->[$py]} &&
            $grid->[$py][$px] ne '#') {
            push @pq, [$cost + 1, $px, $py, $d];
        }

        # Reverse of turn: came from same position with different direction
        push @pq, [$cost + 1000, $x, $y, ($d - 1) % 4];
        push @pq, [$cost + 1000, $x, $y, ($d + 1) % 4];
    }

    return \%dist;
}

sub part1 {
    my ($grid, $start, $end) = @_;
    my $dist = dijkstra_forward($grid, $start);

    my $min_cost = 1e100;
    for my $d (0..3) {
        my $state = "$end->[0],$end->[1],$d";
        if (exists $dist->{$state} && $dist->{$state} < $min_cost) {
            $min_cost = $dist->{$state};
        }
    }

    return $min_cost;
}

sub part2 {
    my ($grid, $start, $end, $best_score) = @_;
    my $dist_from_start = dijkstra_forward($grid, $start);
    my $dist_to_end = dijkstra_backward($grid, $end);

    my %tiles_on_best_path;

    for my $y (0 .. $#{$grid}) {
        for my $x (0 .. $#{$grid->[$y]}) {
            next if $grid->[$y][$x] eq '#';

            # Check if this tile is on any optimal path
            for my $d (0..3) {
                my $state = "$x,$y,$d";
                my $from_start = $dist_from_start->{$state} // 1e100;
                my $to_end = $dist_to_end->{$state} // 1e100;

                if ($from_start + $to_end == $best_score) {
                    $tiles_on_best_path{"$x,$y"} = 1;
                    last;
                }
            }
        }
    }

    return scalar keys %tiles_on_best_path;
}

sub main {
    # Read input
    my $input_path = '../input.txt';
    open my $fh, '<', $input_path or die "Cannot open $input_path: $!";
    my $text = do { local $/; <$fh> };
    close $fh;

    chomp $text;
    my ($grid, $start, $end) = parse_input($text);

    my $answer1 = part1($grid, $start, $end);
    say "Part 1: $answer1";

    my $answer2 = part2($grid, $start, $end, $answer1);
    say "Part 2: $answer2";
}

main();
