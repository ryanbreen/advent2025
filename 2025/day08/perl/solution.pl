#!/usr/bin/env perl
use strict;
use warnings;
use v5.10;

# Union-Find data structure
package UnionFind {
    sub new {
        my ($class, $n) = @_;
        my $self = {
            parent => [0..$n-1],
            rank => [(0) x $n],
            size => [(1) x $n],
        };
        return bless $self, $class;
    }

    sub find {
        my ($self, $x) = @_;
        if ($self->{parent}[$x] != $x) {
            $self->{parent}[$x] = $self->find($self->{parent}[$x]);  # Path compression
        }
        return $self->{parent}[$x];
    }

    sub union {
        my ($self, $x, $y) = @_;
        my $px = $self->find($x);
        my $py = $self->find($y);

        if ($px == $py) {
            return 0;  # Already in same set
        }

        # Union by rank
        if ($self->{rank}[$px] < $self->{rank}[$py]) {
            ($px, $py) = ($py, $px);
        }
        $self->{parent}[$py] = $px;
        $self->{size}[$px] += $self->{size}[$py];
        if ($self->{rank}[$px] == $self->{rank}[$py]) {
            $self->{rank}[$px]++;
        }
        return 1;
    }

    sub get_component_sizes {
        my ($self) = @_;
        my @sizes;
        for my $i (0..$#{$self->{parent}}) {
            if ($self->{parent}[$i] == $i) {  # Root of a component
                push @sizes, $self->{size}[$i];
            }
        }
        return @sizes;
    }
}

# Parse input file
sub parse_input {
    my ($filename) = @_;
    my @points;

    open my $fh, '<', $filename or die "Cannot open $filename: $!";
    while (my $line = <$fh>) {
        chomp $line;
        next if $line eq '';
        my ($x, $y, $z) = split /,/, $line;
        push @points, [$x, $y, $z];
    }
    close $fh;

    return @points;
}

# Squared Euclidean distance
sub euclidean_distance_sq {
    my ($p1, $p2) = @_;
    my $dx = $p1->[0] - $p2->[0];
    my $dy = $p1->[1] - $p2->[1];
    my $dz = $p1->[2] - $p2->[2];
    return $dx*$dx + $dy*$dy + $dz*$dz;
}

sub part1 {
    my ($points_ref, $num_connections) = @_;
    my @points = @$points_ref;
    my $n = scalar @points;

    # Generate all pairs with distances
    my @pairs;
    for my $i (0..$n-2) {
        for my $j ($i+1..$n-1) {
            my $dist_sq = euclidean_distance_sq($points[$i], $points[$j]);
            push @pairs, [$dist_sq, $i, $j];
        }
    }

    # Sort by distance
    @pairs = sort { $a->[0] <=> $b->[0] } @pairs;

    # Union-Find to connect closest pairs
    my $uf = UnionFind->new($n);
    my $connections = 0;
    for my $pair (@pairs) {
        my ($dist_sq, $i, $j) = @$pair;
        $uf->union($i, $j);
        $connections++;
        last if $connections == $num_connections;
    }

    # Get component sizes and find the 3 largest
    my @sizes = sort { $b <=> $a } $uf->get_component_sizes();

    # Multiply the 3 largest
    return $sizes[0] * $sizes[1] * $sizes[2];
}

sub part2 {
    my ($points_ref) = @_;
    my @points = @$points_ref;
    my $n = scalar @points;

    # Generate all pairs with distances
    my @pairs;
    for my $i (0..$n-2) {
        for my $j ($i+1..$n-1) {
            my $dist_sq = euclidean_distance_sq($points[$i], $points[$j]);
            push @pairs, [$dist_sq, $i, $j];
        }
    }

    # Sort by distance
    @pairs = sort { $a->[0] <=> $b->[0] } @pairs;

    # Union-Find to connect until all in one circuit
    my $uf = UnionFind->new($n);
    my $num_components = $n;

    for my $pair (@pairs) {
        my ($dist_sq, $i, $j) = @$pair;
        if ($uf->union($i, $j)) {  # Actually merged two components
            $num_components--;
            if ($num_components == 1) {
                # This was the last connection - all in one circuit now
                return $points[$i][0] * $points[$j][0];  # Product of X coordinates
            }
        }
    }

    return 0;
}

# Main
my $input_file = $ARGV[0] // '../input.txt';
my @points = parse_input($input_file);

say "Part 1: ", part1(\@points, 1000);
say "Part 2: ", part2(\@points);
