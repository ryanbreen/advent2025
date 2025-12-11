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

# Count paths from start to target using memoization
sub count_paths_memoized {
    my ($start, $target, $graph) = @_;
    my %memo;

    my $count_recursive;
    $count_recursive = sub {
        my ($node) = @_;

        return $memo{$node} if exists $memo{$node};

        if ($node eq $target) {
            $memo{$node} = 1;
            return 1;
        }

        if (!exists $graph->{$node}) {
            $memo{$node} = 0;
            return 0;
        }

        my $total = 0;
        for my $neighbor (@{$graph->{$node}}) {
            $total += $count_recursive->($neighbor);
        }

        $memo{$node} = $total;
        return $total;
    };

    return $count_recursive->($start);
}

# Part 1: Count all paths from 'you' to 'out'
sub part1 {
    my ($graph) = @_;
    return count_paths_memoized('you', 'out', $graph);
}

# Part 2: Count paths from 'svr' to 'out' that visit both 'dac' and 'fft'
# Strategy: Compute path counts for each segment, then multiply them for each ordering:
# - svr->dac->fft->out: paths(svr,dac) * paths(dac,fft) * paths(fft,out)
# - svr->fft->dac->out: paths(svr,fft) * paths(fft,dac) * paths(dac,out)
sub part2 {
    my ($graph) = @_;

    # Count paths for each segment (using BigInt for large numbers)
    my $svr_to_dac = Math::BigInt->new(count_paths_memoized('svr', 'dac', $graph));
    my $svr_to_fft = Math::BigInt->new(count_paths_memoized('svr', 'fft', $graph));
    my $dac_to_fft = Math::BigInt->new(count_paths_memoized('dac', 'fft', $graph));
    my $fft_to_dac = Math::BigInt->new(count_paths_memoized('fft', 'dac', $graph));
    my $dac_to_out = Math::BigInt->new(count_paths_memoized('dac', 'out', $graph));
    my $fft_to_out = Math::BigInt->new(count_paths_memoized('fft', 'out', $graph));

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
