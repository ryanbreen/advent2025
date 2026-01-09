#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;
use File::Spec;

# Calculate GCD using Euclidean algorithm
sub gcd {
    my ($a, $b) = @_;
    while ($b) {
        ($a, $b) = ($b, $a % $b);
    }
    return $a;
}

# Calculate LCM
sub lcm {
    my ($a, $b) = @_;
    return ($a * $b) / gcd($a, $b);
}

# Parse input and extract blizzard positions
sub parse_input {
    my ($text) = @_;
    my @lines = split /\n/, $text;
    @lines = grep { length($_) > 0 } @lines;

    my $height = scalar @lines;
    my $width = length($lines[0]);

    # Inner dimensions (excluding walls)
    my $inner_h = $height - 2;
    my $inner_w = $width - 2;

    my @blizzards;
    for my $r (0 .. $#lines) {
        my @chars = split //, $lines[$r];
        for my $c (0 .. $#chars) {
            my $ch = $chars[$c];
            if ($ch =~ /[\^v<>]/) {
                push @blizzards, [$r, $c, $ch];
            }
        }
    }

    # Find start and end positions
    my $start_col = index($lines[0], '.');
    my $end_col = index($lines[-1], '.');
    my $start = [0, $start_col];
    my $end = [$height - 1, $end_col];

    return (\@blizzards, $height, $width, $inner_h, $inner_w, $start, $end);
}

# Get all blizzard positions at a given time
sub get_blizzard_positions {
    my ($blizzards, $inner_h, $inner_w, $time) = @_;
    my %positions;

    for my $bliz (@$blizzards) {
        my ($r, $c, $direction) = @$bliz;
        # Adjust to inner coordinates (subtract 1 for wall)
        my $ir = $r - 1;
        my $ic = $c - 1;

        my ($nr, $nc);

        if ($direction eq '^') {
            $nr = ($ir - $time) % $inner_h;
            $nc = $ic;
        } elsif ($direction eq 'v') {
            $nr = ($ir + $time) % $inner_h;
            $nc = $ic;
        } elsif ($direction eq '<') {
            $nc = ($ic - $time) % $inner_w;
            $nr = $ir;
        } elsif ($direction eq '>') {
            $nc = ($ic + $time) % $inner_w;
            $nr = $ir;
        }

        # Convert back to full coordinates
        my $key = ($nr + 1) . "," . ($nc + 1);
        $positions{$key} = 1;
    }

    return \%positions;
}

# BFS to find shortest path avoiding blizzards
sub bfs {
    my ($blizzards, $height, $width, $inner_h, $inner_w, $start, $end, $start_time) = @_;

    my $period = lcm($inner_h, $inner_w);

    # Precompute blizzard positions for all times in one period
    my %blizzard_cache;
    for my $t (0 .. $period - 1) {
        $blizzard_cache{$t} = get_blizzard_positions($blizzards, $inner_h, $inner_w, $t);
    }

    # BFS queue: [time, row, col]
    my @queue = ([$start_time, $start->[0], $start->[1]]);
    my %visited;
    my $init_state = ($start_time % $period) . "," . $start->[0] . "," . $start->[1];
    $visited{$init_state} = 1;

    # Directions: wait, up, down, left, right
    my @directions = ([0, 0], [-1, 0], [1, 0], [0, -1], [0, 1]);

    while (@queue) {
        my $current = shift @queue;
        my ($time, $r, $c) = @$current;

        if ($r == $end->[0] && $c == $end->[1]) {
            return $time;
        }

        my $next_time = $time + 1;
        my $next_blizzards = $blizzard_cache{$next_time % $period};

        for my $dir (@directions) {
            my ($dr, $dc) = @$dir;
            my $nr = $r + $dr;
            my $nc = $c + $dc;

            # Check bounds
            my $is_start = ($nr == $start->[0] && $nc == $start->[1]);
            my $is_end = ($nr == $end->[0] && $nc == $end->[1]);

            if (!$is_start && !$is_end) {
                # Check if in wall
                if ($nr <= 0 || $nr >= $height - 1 || $nc <= 0 || $nc >= $width - 1) {
                    next;
                }
            }

            # Check blizzards
            my $pos_key = "$nr,$nc";
            if (exists $next_blizzards->{$pos_key}) {
                next;
            }

            my $state = ($next_time % $period) . ",$nr,$nc";
            if (!exists $visited{$state}) {
                $visited{$state} = 1;
                push @queue, [$next_time, $nr, $nc];
            }
        }
    }

    return -1;  # No path found
}

sub part1 {
    my ($text) = @_;
    my ($blizzards, $height, $width, $inner_h, $inner_w, $start, $end) = parse_input($text);
    return bfs($blizzards, $height, $width, $inner_h, $inner_w, $start, $end, 0);
}

sub part2 {
    my ($text) = @_;
    my ($blizzards, $height, $width, $inner_h, $inner_w, $start, $end) = parse_input($text);

    # Trip 1: start to end
    my $t1 = bfs($blizzards, $height, $width, $inner_h, $inner_w, $start, $end, 0);

    # Trip 2: end to start
    my $t2 = bfs($blizzards, $height, $width, $inner_h, $inner_w, $end, $start, $t1);

    # Trip 3: start to end again
    my $t3 = bfs($blizzards, $height, $width, $inner_h, $inner_w, $start, $end, $t2);

    return $t3;
}

sub main {
    my $script_dir = dirname(__FILE__);
    my $input_file = File::Spec->catfile($script_dir, '..', 'input.txt');

    open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
    my $text = do { local $/; <$fh> };
    close $fh;

    print "Part 1: ", part1($text), "\n";
    print "Part 2: ", part2($text), "\n";
}

main();
