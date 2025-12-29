#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;
use File::Spec;
use List::Util qw(max);

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
        my $c = index($grid->[$r], 'S');
        return ($r, $c) if $c >= 0;
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
            my ($nr, $nc) = ($r + $dr, $c + $dc);
            next unless $nr >= 0 && $nr < $rows && $nc >= 0 && $nc < $cols;

            my $adj_ch = substr($grid->[$nr], $nc, 1);
            next unless exists $PIPE_CONNECTIONS{$adj_ch};

            for my $adj_dir (@{$PIPE_CONNECTIONS{$adj_ch}}) {
                my ($adj_dr, $adj_dc) = @$adj_dir;
                if ($nr + $adj_dr == $r && $nc + $adj_dc == $c) {
                    push @neighbors, [$nr, $nc];
                    last;
                }
            }
        }
    } elsif (exists $PIPE_CONNECTIONS{$ch}) {
        for my $dir (@{$PIPE_CONNECTIONS{$ch}}) {
            my ($dr, $dc) = @$dir;
            my ($nr, $nc) = ($r + $dr, $c + $dc);
            push @neighbors, [$nr, $nc] if $nr >= 0 && $nr < $rows && $nc >= 0 && $nc < $cols;
        }
    }

    return @neighbors;
}

sub find_loop {
    my ($grid, $start_r, $start_c) = @_;
    my %distances = ("$start_r,$start_c" => 0);
    my @queue = ([$start_r, $start_c]);

    while (@queue) {
        my ($r, $c) = @{shift @queue};
        my $curr_dist = $distances{"$r,$c"};

        for my $neighbor (get_neighbors($grid, $r, $c)) {
            my $key = "$neighbor->[0],$neighbor->[1]";
            next if exists $distances{$key};
            $distances{$key} = $curr_dist + 1;
            push @queue, $neighbor;
        }
    }

    return \%distances;
}

sub determine_start_pipe {
    my ($grid, $start_r, $start_c, $loop_positions) = @_;
    my %connections;

    for my $dir ([-1, 0], [1, 0], [0, -1], [0, 1]) {
        my ($dr, $dc) = @$dir;
        my ($nr, $nc) = ($start_r + $dr, $start_c + $dc);
        my $neighbor_key = "$nr,$nc";

        next unless exists $loop_positions->{$neighbor_key};

        my $adj_ch = substr($grid->[$nr], $nc, 1);
        next unless exists $PIPE_CONNECTIONS{$adj_ch};

        for my $adj_dir (@{$PIPE_CONNECTIONS{$adj_ch}}) {
            my ($adj_dr, $adj_dc) = @$adj_dir;
            if ($nr + $adj_dr == $start_r && $nc + $adj_dc == $start_c) {
                $connections{"$dr,$dc"} = 1;
                last;
            }
        }
    }

    # Find matching pipe by comparing connection sets
    my $conn_key = join('|', sort keys %connections);
    for my $pipe (keys %PIPE_CONNECTIONS) {
        my $pipe_key = join('|', sort map { "$_->[0],$_->[1]" } @{$PIPE_CONNECTIONS{$pipe}});
        return $pipe if $conn_key eq $pipe_key;
    }

    return 'S';
}

# Compute loop once and share between parts
my ($start_r, $start_c) = find_start(\@lines);
my $loop_positions = find_loop(\@lines, $start_r, $start_c);

sub part1 {
    return max(values %$loop_positions);
}

sub part2 {
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
                $inside = !$inside if $ch eq '|' || $ch eq 'L' || $ch eq 'J';
            } else {
                $enclosed++ if $inside;
            }
        }
    }

    return $enclosed;
}

print "Part 1: ", part1(), "\n";
print "Part 2: ", part2(), "\n";
