#!/usr/bin/env perl
use strict;
use warnings;
use Math::BigInt;

# Parse input into a graph (hash of arrays)
sub parse_input {
    my ($filename) = @_;
    my %graph;

    open(my $fh, '<', $filename) or die "Cannot open $filename: $!";
    while (my $line = <$fh>) {
        chomp $line;
        next if $line =~ /^\s*$/;

        my ($node, $neighbors_str) = split(/: /, $line, 2);
        my @neighbors = split(/\s+/, $neighbors_str || '');
        $graph{$node} = \@neighbors;
    }
    close($fh);

    return \%graph;
}

# Part 1: Count all paths from 'you' to 'out' using memoization
sub part1 {
    my ($graph) = @_;
    my %memo;

    sub count_paths {
        my ($node, $graph, $memo) = @_;

        return $memo->{$node} if exists $memo->{$node};

        if ($node eq 'out') {
            $memo->{$node} = 1;
            return 1;
        }

        if (!exists $graph->{$node}) {
            $memo->{$node} = 0;
            return 0;
        }

        my $total = 0;
        for my $neighbor (@{$graph->{$node}}) {
            $total += count_paths($neighbor, $graph, $memo);
        }

        $memo->{$node} = $total;
        return $total;
    }

    return count_paths('you', $graph, \%memo);
}

# Helper function to count paths to a specific target
sub count_paths_to_target {
    my ($start, $target, $graph) = @_;
    my %memo;

    my $count_recursive;
    $count_recursive = sub {
        my ($node) = @_;

        return $memo{$node} if exists $memo{$node};

        if ($node eq $target) {
            $memo{$node} = Math::BigInt->new(1);
            return $memo{$node};
        }

        if (!exists $graph->{$node}) {
            $memo{$node} = Math::BigInt->new(0);
            return $memo{$node};
        }

        my $total = Math::BigInt->new(0);
        for my $neighbor (@{$graph->{$node}}) {
            $total += $count_recursive->($neighbor);
        }

        $memo{$node} = $total;
        return $total;
    };

    return $count_recursive->($start);
}

# Part 2: Count paths from 'svr' to 'out' that visit both 'dac' and 'fft'
sub part2 {
    my ($graph) = @_;

    # Count paths for each segment
    my $svr_to_dac = count_paths_to_target('svr', 'dac', $graph);
    my $svr_to_fft = count_paths_to_target('svr', 'fft', $graph);
    my $dac_to_fft = count_paths_to_target('dac', 'fft', $graph);
    my $fft_to_dac = count_paths_to_target('fft', 'dac', $graph);
    my $dac_to_out = count_paths_to_target('dac', 'out', $graph);
    my $fft_to_out = count_paths_to_target('fft', 'out', $graph);

    # Paths that visit dac before fft: svr -> dac -> fft -> out
    my $dac_before_fft = $svr_to_dac * $dac_to_fft * $fft_to_out;

    # Paths that visit fft before dac: svr -> fft -> dac -> out
    my $fft_before_dac = $svr_to_fft * $fft_to_dac * $dac_to_out;

    return $dac_before_fft + $fft_before_dac;
}

# Main
my $input_file = '../input.txt';
if (@ARGV > 0) {
    $input_file = $ARGV[0];
}

my $graph = parse_input($input_file);

print "Part 1: ", part1($graph), "\n";
print "Part 2: ", part2($graph), "\n";
