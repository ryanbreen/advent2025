#!/usr/bin/env perl
use strict;
use warnings;
use List::Util qw(shuffle);

# Advent of Code 2023 - Day 25: Snowverload
# Find the minimum cut of 3 edges that divides the graph into two components.
#
# Uses edge betweenness centrality: edges that form the cut between two large
# components will have high betweenness (many shortest paths pass through them).

sub parse_input {
    my ($filename) = @_;
    my %graph;

    open my $fh, '<', $filename or die "Cannot open $filename: $!";
    while (my $line = <$fh>) {
        chomp $line;
        next if $line eq '';

        my ($left, $right) = split /: /, $line;
        my @neighbors = split / /, $right;

        for my $neighbor (@neighbors) {
            $graph{$left}{$neighbor} = 1;
            $graph{$neighbor}{$left} = 1;
        }
    }
    close $fh;

    return \%graph;
}

sub bfs_component_size {
    my ($graph, $start, $excluded_edges) = @_;

    my %visited = ($start => 1);
    my @queue = ($start);

    while (@queue) {
        my $node = shift @queue;
        for my $neighbor (keys %{$graph->{$node}}) {
            my $edge = join(',', sort($node, $neighbor));
            if (!$visited{$neighbor} && !$excluded_edges->{$edge}) {
                $visited{$neighbor} = 1;
                push @queue, $neighbor;
            }
        }
    }

    return scalar(keys %visited);
}

sub compute_edge_betweenness {
    my ($graph, $sample_nodes) = @_;

    my %edge_count;
    my @nodes = keys %$graph;

    # Sample nodes for efficiency
    if ($sample_nodes && @nodes > $sample_nodes) {
        srand(42);
        @nodes = (shuffle @nodes)[0..$sample_nodes-1];
    }

    for my $source (@nodes) {
        # BFS to find shortest paths
        my %dist = ($source => 0);
        my %pred;  # Predecessors on shortest paths
        my @queue = ($source);

        while (@queue) {
            my $node = shift @queue;
            for my $neighbor (keys %{$graph->{$node}}) {
                if (!exists $dist{$neighbor}) {
                    $dist{$neighbor} = $dist{$node} + 1;
                    push @{$pred{$neighbor}}, $node;
                    push @queue, $neighbor;
                } elsif ($dist{$neighbor} == $dist{$node} + 1) {
                    push @{$pred{$neighbor}}, $node;
                }
            }
        }

        # Backtrack to count edge usage
        # Number of shortest paths to each node
        my %num_paths = ($source => 1.0);
        for my $node (sort { $dist{$a} <=> $dist{$b} } keys %dist) {
            if (exists $pred{$node}) {
                for my $p (@{$pred{$node}}) {
                    $num_paths{$node} = ($num_paths{$node} || 0) + ($num_paths{$p} || 0);
                }
            }
        }

        # Accumulate edge betweenness (reverse BFS order)
        my %dependency;
        for my $node (sort { $dist{$b} <=> $dist{$a} } keys %dist) {
            next unless exists $pred{$node};
            for my $p (@{$pred{$node}}) {
                my $edge = join(',', sort($p, $node));
                # Weight by path fraction
                my $frac = $num_paths{$p} / $num_paths{$node};
                my $contrib = $frac * (1 + ($dependency{$node} || 0));
                $edge_count{$edge} = ($edge_count{$edge} || 0) + $contrib;
                $dependency{$p} = ($dependency{$p} || 0) + $contrib;
            }
        }
    }

    return \%edge_count;
}

sub find_cut_edges {
    my ($graph) = @_;

    # Compute edge betweenness with sampling for speed
    my $edge_betweenness = compute_edge_betweenness($graph, 100);

    # Sort edges by betweenness (highest first)
    my @sorted_edges = sort { $edge_betweenness->{$b} <=> $edge_betweenness->{$a} }
                       keys %$edge_betweenness;

    my $total_nodes = scalar(keys %$graph);

    # Try removing top candidate edges
    # We need to find 3 edges that disconnect the graph
    my @top_edges = @sorted_edges[0..19];  # Check top 20 candidates

    for my $i (0..$#top_edges-2) {
        for my $j ($i+1..$#top_edges-1) {
            for my $k ($j+1..$#top_edges) {
                my %excluded = (
                    $top_edges[$i] => 1,
                    $top_edges[$j] => 1,
                    $top_edges[$k] => 1
                );

                my ($start) = keys %$graph;
                my $size1 = bfs_component_size($graph, $start, \%excluded);

                if ($size1 < $total_nodes) {
                    # Graph is disconnected!
                    my $size2 = $total_nodes - $size1;
                    return $size1 * $size2;
                }
            }
        }
    }

    return undef;
}

sub part1 {
    my ($filename) = @_;
    my $graph = parse_input($filename);
    return find_cut_edges($graph);
}

sub part2 {
    my ($filename) = @_;
    # Part 2: Day 25 Part 2 is traditionally unlocked by having 49 stars.
    # There's no computation needed - just push the button!
    return "Push the big red button!";
}

# Main
use File::Basename;
use File::Spec;
my $dir = dirname(__FILE__);
my $input_file = File::Spec->catfile($dir, '..', 'input.txt');
print "Part 1: ", part1($input_file), "\n";
print "Part 2: ", part2($input_file), "\n";
