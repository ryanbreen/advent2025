#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;
use File::Spec;

# Read input
my $script_dir = dirname(__FILE__);
my $input_path = File::Spec->catfile($script_dir, '..', 'input.txt');
open my $fh, '<', $input_path or die "Cannot open $input_path: $!";
my @lines = <$fh>;
close $fh;
chomp @lines;

# Pipe connections: each pipe connects to certain directions
# Directions: N=(-1,0), S=(1,0), E=(0,1), W=(0,-1)
my %PIPE_CONNECTIONS = (
    '|' => [[-1, 0], [1, 0]],    # N, S
    '-' => [[0, -1], [0, 1]],    # W, E
    'L' => [[-1, 0], [0, 1]],    # N, E
    'J' => [[-1, 0], [0, -1]],   # N, W
    '7' => [[1, 0], [0, -1]],    # S, W
    'F' => [[1, 0], [0, 1]],     # S, E
);

sub find_start {
    my ($grid) = @_;
    for my $r (0 .. $#$grid) {
        my @chars = split //, $grid->[$r];
        for my $c (0 .. $#chars) {
            if ($chars[$c] eq 'S') {
                return ($r, $c);
            }
        }
    }
    return (undef, undef);
}

sub get_neighbors {
    my ($grid, $r, $c) = @_;
    my $rows = scalar @$grid;
    my $cols = length $grid->[0];
    my $ch = substr($grid->[$r], $c, 1);
    my @neighbors;

    if ($ch eq 'S') {
        # S can connect to any adjacent pipe that connects back to it
        for my $dir ([-1, 0], [1, 0], [0, -1], [0, 1]) {
            my ($dr, $dc) = @$dir;
            my $nr = $r + $dr;
            my $nc = $c + $dc;
            if ($nr >= 0 && $nr < $rows && $nc >= 0 && $nc < $cols) {
                my $adj_ch = substr($grid->[$nr], $nc, 1);
                if (exists $PIPE_CONNECTIONS{$adj_ch}) {
                    for my $adj_dir (@{$PIPE_CONNECTIONS{$adj_ch}}) {
                        my ($adj_dr, $adj_dc) = @$adj_dir;
                        if ($nr + $adj_dr == $r && $nc + $adj_dc == $c) {
                            push @neighbors, [$nr, $nc];
                            last;
                        }
                    }
                }
            }
        }
    } elsif (exists $PIPE_CONNECTIONS{$ch}) {
        for my $dir (@{$PIPE_CONNECTIONS{$ch}}) {
            my ($dr, $dc) = @$dir;
            my $nr = $r + $dr;
            my $nc = $c + $dc;
            if ($nr >= 0 && $nr < $rows && $nc >= 0 && $nc < $cols) {
                push @neighbors, [$nr, $nc];
            }
        }
    }

    return @neighbors;
}

sub find_loop {
    my ($grid, $start_r, $start_c) = @_;
    my %distances;
    my $start_key = "$start_r,$start_c";
    $distances{$start_key} = 0;
    my @queue = ([$start_r, $start_c]);

    while (@queue) {
        my $pos = shift @queue;
        my ($r, $c) = @$pos;
        my $curr_key = "$r,$c";

        for my $neighbor (get_neighbors($grid, $r, $c)) {
            my ($nr, $nc) = @$neighbor;
            my $neighbor_key = "$nr,$nc";
            if (!exists $distances{$neighbor_key}) {
                $distances{$neighbor_key} = $distances{$curr_key} + 1;
                push @queue, [$nr, $nc];
            }
        }
    }

    return \%distances;
}

sub determine_start_pipe {
    my ($grid, $start_r, $start_c, $loop_positions) = @_;
    my $rows = scalar @$grid;
    my $cols = length $grid->[0];
    my %connections;

    for my $dir ([-1, 0], [1, 0], [0, -1], [0, 1]) {
        my ($dr, $dc) = @$dir;
        my $nr = $start_r + $dr;
        my $nc = $start_c + $dc;
        my $neighbor_key = "$nr,$nc";

        if (exists $loop_positions->{$neighbor_key}) {
            my $adj_ch = substr($grid->[$nr], $nc, 1);
            if (exists $PIPE_CONNECTIONS{$adj_ch}) {
                for my $adj_dir (@{$PIPE_CONNECTIONS{$adj_ch}}) {
                    my ($adj_dr, $adj_dc) = @$adj_dir;
                    if ($nr + $adj_dr == $start_r && $nc + $adj_dc == $start_c) {
                        $connections{"$dr,$dc"} = 1;
                        last;
                    }
                }
            }
        }
    }

    for my $pipe (keys %PIPE_CONNECTIONS) {
        my @dirs = @{$PIPE_CONNECTIONS{$pipe}};
        my %pipe_dirs;
        for my $d (@dirs) {
            $pipe_dirs{"$d->[0],$d->[1]"} = 1;
        }

        my $match = 1;
        if (scalar(keys %connections) != scalar(keys %pipe_dirs)) {
            $match = 0;
        } else {
            for my $k (keys %connections) {
                if (!exists $pipe_dirs{$k}) {
                    $match = 0;
                    last;
                }
            }
        }

        if ($match) {
            return $pipe;
        }
    }

    return 'S';
}

sub part1 {
    my ($start_r, $start_c) = find_start(\@lines);
    my $distances = find_loop(\@lines, $start_r, $start_c);
    my $max_dist = 0;
    for my $d (values %$distances) {
        $max_dist = $d if $d > $max_dist;
    }
    return $max_dist;
}

sub part2 {
    my ($start_r, $start_c) = find_start(\@lines);
    my $distances = find_loop(\@lines, $start_r, $start_c);
    my $loop_positions = $distances;

    # Determine what pipe S actually is
    my $start_pipe = determine_start_pipe(\@lines, $start_r, $start_c, $loop_positions);

    # Create a mutable copy of the grid
    my @grid = map { [split //, $_] } @lines;
    $grid[$start_r][$start_c] = $start_pipe;

    my $rows = scalar @grid;
    my $cols = scalar @{$grid[0]};
    my $enclosed = 0;

    for my $r (0 .. $rows - 1) {
        my $inside = 0;
        for my $c (0 .. $cols - 1) {
            my $key = "$r,$c";
            if (exists $loop_positions->{$key}) {
                my $ch = $grid[$r][$c];
                # Count vertical crossings using "north" rule
                # Count pipes that have a north connection: |, L, J
                if ($ch eq '|' || $ch eq 'L' || $ch eq 'J') {
                    $inside = !$inside;
                }
            } else {
                if ($inside) {
                    $enclosed++;
                }
            }
        }
    }

    return $enclosed;
}

print "Part 1: ", part1(), "\n";
print "Part 2: ", part2(), "\n";
