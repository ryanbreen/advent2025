#!/usr/bin/env perl
use strict;
use warnings;
use List::Util qw(max);

sub parse_input {
    my ($filename) = @_;
    my %graph;

    open my $fh, '<', $filename or die "Cannot open $filename: $!";
    while (my $line = <$fh>) {
        chomp $line;
        my ($a, $b) = split /-/, $line;
        $graph{$a}{$b} = 1;
        $graph{$b}{$a} = 1;
    }
    close $fh;

    return \%graph;
}

sub find_triangles {
    my ($graph) = @_;
    my %triangles;

    for my $a (keys %$graph) {
        for my $b (keys %{$graph->{$a}}) {
            next unless $a lt $b;  # Only process each edge once

            # Find common neighbors (intersection of neighbors of a and b)
            for my $c (keys %{$graph->{$a}}) {
                if (exists $graph->{$b}{$c}) {
                    # Create sorted triangle key
                    my @tri = sort ($a, $b, $c);
                    my $key = join(',', @tri);
                    $triangles{$key} = \@tri;
                }
            }
        }
    }

    return \%triangles;
}

sub part1 {
    my ($graph) = @_;
    my $triangles = find_triangles($graph);
    my $count = 0;

    for my $key (keys %$triangles) {
        my $tri = $triangles->{$key};
        if (grep { /^t/ } @$tri) {
            $count++;
        }
    }

    return $count;
}

sub set_intersection {
    my ($set1, $set2) = @_;
    my %result;
    for my $key (keys %$set1) {
        $result{$key} = 1 if exists $set2->{$key};
    }
    return \%result;
}

sub set_union {
    my ($set1, $set2) = @_;
    my %result = %$set1;
    for my $key (keys %$set2) {
        $result{$key} = 1;
    }
    return \%result;
}

sub set_difference {
    my ($set1, $set2) = @_;
    my %result;
    for my $key (keys %$set1) {
        $result{$key} = 1 unless exists $set2->{$key};
    }
    return \%result;
}

sub bron_kerbosch {
    my ($graph, $r, $p, $x, $cliques) = @_;

    # If both P and X are empty, R is a maximal clique
    if (!%$p && !%$x) {
        push @$cliques, {%$r};
        return;
    }

    # Choose pivot from P ∪ X to minimize branching
    my $p_union_x = set_union($p, $x);
    my $pivot = '';
    my $max_neighbors = -1;

    for my $v (keys %$p_union_x) {
        my $neighbors_in_p = set_intersection($graph->{$v}, $p);
        my $count = scalar keys %$neighbors_in_p;
        if ($count > $max_neighbors) {
            $max_neighbors = $count;
            $pivot = $v;
        }
    }

    # For each vertex in P \ N(pivot)
    my $pivot_neighbors = exists $graph->{$pivot} ? $graph->{$pivot} : {};
    my $candidates = set_difference($p, $pivot_neighbors);

    for my $v (keys %$candidates) {
        my $new_r = set_union($r, {$v => 1});
        my $new_p = set_intersection($p, $graph->{$v});
        my $new_x = set_intersection($x, $graph->{$v});

        bron_kerbosch($graph, $new_r, $new_p, $new_x, $cliques);

        delete $p->{$v};
        $x->{$v} = 1;
    }
}

sub part2 {
    my ($graph) = @_;
    my @cliques;

    # Initialize P with all nodes
    my %all_nodes = map { $_ => 1 } keys %$graph;

    bron_kerbosch($graph, {}, \%all_nodes, {}, \@cliques);

    # Find the largest clique
    my $largest = $cliques[0];
    my $max_size = scalar keys %$largest;

    for my $clique (@cliques) {
        my $size = scalar keys %$clique;
        if ($size > $max_size) {
            $max_size = $size;
            $largest = $clique;
        }
    }

    # Return sorted, comma-joined password
    my @sorted = sort keys %$largest;
    return join(',', @sorted);
}

sub main {
    my $graph = parse_input('../input.txt');

    print 'Part 1: ', part1($graph), "\n";
    print 'Part 2: ', part2($graph), "\n";
}

main();
