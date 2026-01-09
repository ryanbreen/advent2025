#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;
use Cwd 'abs_path';

sub parse_grid {
    my ($text) = @_;
    my @lines = split /\n/, $text;
    my @grid;
    my ($start_r, $start_c, $end_r, $end_c);

    for my $r (0 .. $#lines) {
        my @row = split //, $lines[$r];
        for my $c (0 .. $#row) {
            if ($row[$c] eq 'S') {
                ($start_r, $start_c) = ($r, $c);
                $row[$c] = 'a';
            } elsif ($row[$c] eq 'E') {
                ($end_r, $end_c) = ($r, $c);
                $row[$c] = 'z';
            }
        }
        push @grid, \@row;
    }

    return (\@grid, $start_r, $start_c, $end_r, $end_c);
}

sub bfs {
    my ($grid, $starts, $end_r, $end_c) = @_;
    my $rows = scalar @$grid;
    my $cols = scalar @{$grid->[0]};
    my %visited;
    my @queue;

    # Initialize queue with all starting positions
    for my $start (@$starts) {
        my ($r, $c) = @$start;
        push @queue, [$r, $c, 0];
        $visited{"$r,$c"} = 1;
    }

    my @directions = ([-1, 0], [1, 0], [0, -1], [0, 1]);

    while (@queue) {
        my $item = shift @queue;
        my ($r, $c, $dist) = @$item;

        # Check if we reached the end
        if ($r == $end_r && $c == $end_c) {
            return $dist;
        }

        my $current_height = ord($grid->[$r][$c]);

        for my $dir (@directions) {
            my ($dr, $dc) = @$dir;
            my ($nr, $nc) = ($r + $dr, $c + $dc);

            # Check bounds
            next if $nr < 0 || $nr >= $rows || $nc < 0 || $nc >= $cols;

            # Check if already visited
            next if $visited{"$nr,$nc"};

            my $next_height = ord($grid->[$nr][$nc]);

            # Can move if destination is at most 1 higher
            if ($next_height <= $current_height + 1) {
                $visited{"$nr,$nc"} = 1;
                push @queue, [$nr, $nc, $dist + 1];
            }
        }
    }

    return -1;  # No path found
}

sub part1 {
    my ($text) = @_;
    my ($grid, $start_r, $start_c, $end_r, $end_c) = parse_grid($text);
    return bfs($grid, [[$start_r, $start_c]], $end_r, $end_c);
}

sub part2 {
    my ($text) = @_;
    my ($grid, undef, undef, $end_r, $end_c) = parse_grid($text);

    # Find all cells with elevation 'a'
    my @starts;
    for my $r (0 .. $#$grid) {
        for my $c (0 .. $#{$grid->[$r]}) {
            if ($grid->[$r][$c] eq 'a') {
                push @starts, [$r, $c];
            }
        }
    }

    return bfs($grid, \@starts, $end_r, $end_c);
}

sub main {
    my $script_dir = dirname(abs_path(__FILE__));
    my $input_file = "$script_dir/../input.txt";

    open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
    my $text = do { local $/; <$fh> };
    close $fh;

    # Remove trailing newline if present
    chomp $text;

    print "Part 1: ", part1($text), "\n";
    print "Part 2: ", part2($text), "\n";
}

main();
