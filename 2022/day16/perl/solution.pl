#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;

# Parse input file
sub parse_input {
    my ($text) = @_;
    my %valves;
    my %tunnels;

    for my $line (split /\n/, $text) {
        if ($line =~ /Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.+)/) {
            my ($name, $rate, $neighbors) = ($1, $2, $3);
            $valves{$name} = int($rate);
            $tunnels{$name} = [split /, /, $neighbors];
        }
    }

    return (\%valves, \%tunnels);
}

# BFS to compute shortest distances between relevant valves
sub compute_distances {
    my ($valves, $tunnels) = @_;

    # Only care about valves with flow > 0 plus starting valve AA
    my @relevant = ('AA');
    for my $v (keys %$valves) {
        push @relevant, $v if $valves->{$v} > 0;
    }

    my %distances;

    for my $start (@relevant) {
        $distances{$start} = {};
        my @queue = ([$start, 0]);
        my %visited = ($start => 1);

        while (@queue) {
            my ($curr, $dist) = @{shift @queue};

            # Check if current is a relevant valve (not start)
            if ($curr ne $start && exists $valves->{$curr} && $valves->{$curr} > 0) {
                $distances{$start}{$curr} = $dist;
            }
            # Special case for AA
            if ($curr ne $start && $curr eq 'AA') {
                $distances{$start}{$curr} = $dist;
            }

            for my $neighbor (@{$tunnels->{$curr}}) {
                unless ($visited{$neighbor}) {
                    $visited{$neighbor} = 1;
                    push @queue, [$neighbor, $dist + 1];
                }
            }
        }
    }

    return \%distances;
}

# Convert a set (array ref) to a string key for memoization
sub set_to_key {
    return join(',', sort @_);
}

# Part 1: Find maximum pressure release in 30 minutes
sub part1 {
    my ($text) = @_;
    my ($valves, $tunnels) = parse_input($text);
    my $distances = compute_distances($valves, $tunnels);

    # Get valuable valves (flow > 0)
    my @valuable = grep { $valves->{$_} > 0 } keys %$valves;
    my %valuable_set = map { $_ => 1 } @valuable;

    my %memo;

    my $dfs;
    $dfs = sub {
        my ($pos, $time_left, $opened_ref) = @_;

        return 0 if $time_left <= 0;

        my $key = "$pos:$time_left:" . set_to_key(@$opened_ref);
        return $memo{$key} if exists $memo{$key};

        my %opened = map { $_ => 1 } @$opened_ref;
        my $best = 0;

        for my $next_valve (@valuable) {
            next if $opened{$next_valve};

            # Time to move there and open it
            my $time_cost = $distances->{$pos}{$next_valve} + 1;
            if ($time_cost < $time_left) {
                my $new_time = $time_left - $time_cost;
                my $pressure = $valves->{$next_valve} * $new_time;
                my @new_opened = (@$opened_ref, $next_valve);
                my $result = $pressure + $dfs->($next_valve, $new_time, \@new_opened);
                $best = $result if $result > $best;
            }
        }

        $memo{$key} = $best;
        return $best;
    };

    return $dfs->('AA', 30, []);
}

# Part 2: Find maximum pressure with elephant helper (26 minutes each)
sub part2 {
    my ($text) = @_;
    my ($valves, $tunnels) = parse_input($text);
    my $distances = compute_distances($valves, $tunnels);

    # Get valuable valves (flow > 0)
    my @valuable = sort grep { $valves->{$_} > 0 } keys %$valves;
    my $n = scalar @valuable;

    # Compute max pressure for each subset of valves (represented as bitmask)
    sub max_pressure_for_mask {
        my ($mask, $valuable_ref, $valves, $distances) = @_;

        # Build subset from mask
        my @subset;
        for my $i (0 .. $#{$valuable_ref}) {
            push @subset, $valuable_ref->[$i] if $mask & (1 << $i);
        }

        return 0 unless @subset;

        my %subset_set = map { $_ => 1 } @subset;
        my %memo;

        my $dfs;
        $dfs = sub {
            my ($pos, $time_left, $opened_ref) = @_;

            return 0 if $time_left <= 0;

            my $key = "$pos:$time_left:" . set_to_key(@$opened_ref);
            return $memo{$key} if exists $memo{$key};

            my %opened = map { $_ => 1 } @$opened_ref;
            my $best = 0;

            for my $next_valve (@subset) {
                next if $opened{$next_valve};

                my $time_cost = $distances->{$pos}{$next_valve} + 1;
                if ($time_cost < $time_left) {
                    my $new_time = $time_left - $time_cost;
                    my $pressure = $valves->{$next_valve} * $new_time;
                    my @new_opened = (@$opened_ref, $next_valve);
                    my $result = $pressure + $dfs->($next_valve, $new_time, \@new_opened);
                    $best = $result if $result > $best;
                }
            }

            $memo{$key} = $best;
            return $best;
        };

        return $dfs->('AA', 26, []);
    }

    # Compute max scores for all masks
    my @max_scores;
    for my $mask (0 .. (1 << $n) - 1) {
        $max_scores[$mask] = max_pressure_for_mask($mask, \@valuable, $valves, $distances);
    }

    # Find best partition where you and elephant open disjoint sets
    my $best = 0;
    my $full_mask = (1 << $n) - 1;
    for my $mask (0 .. (1 << $n) - 1) {
        my $complement = $full_mask ^ $mask;
        if ($mask <= $complement) {
            my $total = $max_scores[$mask] + $max_scores[$complement];
            $best = $total if $total > $best;
        }
    }

    return $best;
}

# Main
my $script_dir = dirname(__FILE__);
my $input_file = "$script_dir/../input.txt";

open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
my $text = do { local $/; <$fh> };
close $fh;

print "Part 1: ", part1($text), "\n";
print "Part 2: ", part2($text), "\n";
