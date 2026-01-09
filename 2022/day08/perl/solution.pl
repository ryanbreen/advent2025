#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;
use File::Spec;

sub parse_grid {
    my ($lines) = @_;
    my @grid;
    for my $line (@$lines) {
        push @grid, [split //, $line];
    }
    return \@grid;
}

sub is_visible {
    my ($grid, $row, $col) = @_;
    my $rows = scalar @$grid;
    my $cols = scalar @{$grid->[0]};
    my $height = $grid->[$row][$col];

    # Check from left
    my $visible_left = 1;
    for my $c (0 .. $col - 1) {
        if ($grid->[$row][$c] >= $height) {
            $visible_left = 0;
            last;
        }
    }
    return 1 if $visible_left;

    # Check from right
    my $visible_right = 1;
    for my $c ($col + 1 .. $cols - 1) {
        if ($grid->[$row][$c] >= $height) {
            $visible_right = 0;
            last;
        }
    }
    return 1 if $visible_right;

    # Check from top
    my $visible_top = 1;
    for my $r (0 .. $row - 1) {
        if ($grid->[$r][$col] >= $height) {
            $visible_top = 0;
            last;
        }
    }
    return 1 if $visible_top;

    # Check from bottom
    my $visible_bottom = 1;
    for my $r ($row + 1 .. $rows - 1) {
        if ($grid->[$r][$col] >= $height) {
            $visible_bottom = 0;
            last;
        }
    }
    return 1 if $visible_bottom;

    return 0;
}

sub scenic_score {
    my ($grid, $row, $col) = @_;
    my $rows = scalar @$grid;
    my $cols = scalar @{$grid->[0]};
    my $height = $grid->[$row][$col];

    # Count trees visible in each direction
    # Left
    my $left = 0;
    for my $c (reverse 0 .. $col - 1) {
        $left++;
        last if $grid->[$row][$c] >= $height;
    }

    # Right
    my $right = 0;
    for my $c ($col + 1 .. $cols - 1) {
        $right++;
        last if $grid->[$row][$c] >= $height;
    }

    # Up
    my $up = 0;
    for my $r (reverse 0 .. $row - 1) {
        $up++;
        last if $grid->[$r][$col] >= $height;
    }

    # Down
    my $down = 0;
    for my $r ($row + 1 .. $rows - 1) {
        $down++;
        last if $grid->[$r][$col] >= $height;
    }

    return $left * $right * $up * $down;
}

sub part1 {
    my ($grid) = @_;
    my $rows = scalar @$grid;
    my $cols = scalar @{$grid->[0]};
    my $count = 0;

    for my $r (0 .. $rows - 1) {
        for my $c (0 .. $cols - 1) {
            $count++ if is_visible($grid, $r, $c);
        }
    }
    return $count;
}

sub part2 {
    my ($grid) = @_;
    my $rows = scalar @$grid;
    my $cols = scalar @{$grid->[0]};
    my $max_score = 0;

    for my $r (0 .. $rows - 1) {
        for my $c (0 .. $cols - 1) {
            my $score = scenic_score($grid, $r, $c);
            $max_score = $score if $score > $max_score;
        }
    }
    return $max_score;
}

sub main {
    my $script_dir = dirname(File::Spec->rel2abs(__FILE__));
    my $input_file = File::Spec->catfile($script_dir, '..', 'input.txt');

    open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
    my @lines = map { chomp; $_ } <$fh>;
    close $fh;

    my $grid = parse_grid(\@lines);

    print "Part 1: ", part1($grid), "\n";
    print "Part 2: ", part2($grid), "\n";
}

main();
