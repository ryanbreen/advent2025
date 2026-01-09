#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;

# 6 directions: +x, -x, +y, -y, +z, -z
my @DIRECTIONS = (
    [1, 0, 0], [-1, 0, 0],
    [0, 1, 0], [0, -1, 0],
    [0, 0, 1], [0, 0, -1]
);

sub parse_input {
    my ($text) = @_;
    my %cubes;
    for my $line (split /\n/, $text) {
        next if $line =~ /^\s*$/;
        my ($x, $y, $z) = split /,/, $line;
        $cubes{"$x,$y,$z"} = 1;
    }
    return \%cubes;
}

sub part1 {
    my ($text) = @_;
    my $cubes = parse_input($text);
    my $surface_area = 0;

    for my $key (keys %$cubes) {
        my ($x, $y, $z) = split /,/, $key;
        for my $dir (@DIRECTIONS) {
            my ($dx, $dy, $dz) = @$dir;
            my $neighbor = ($x + $dx) . "," . ($y + $dy) . "," . ($z + $dz);
            $surface_area++ unless exists $cubes->{$neighbor};
        }
    }

    return $surface_area;
}

sub part2 {
    my ($text) = @_;
    my $cubes = parse_input($text);

    # Find bounding box with 1 unit padding
    my ($min_x, $max_x, $min_y, $max_y, $min_z, $max_z);
    my $first = 1;

    for my $key (keys %$cubes) {
        my ($x, $y, $z) = split /,/, $key;
        if ($first) {
            $min_x = $max_x = $x;
            $min_y = $max_y = $y;
            $min_z = $max_z = $z;
            $first = 0;
        } else {
            $min_x = $x if $x < $min_x;
            $max_x = $x if $x > $max_x;
            $min_y = $y if $y < $min_y;
            $max_y = $y if $y > $max_y;
            $min_z = $z if $z < $min_z;
            $max_z = $z if $z > $max_z;
        }
    }

    # Add padding
    $min_x--; $max_x++;
    $min_y--; $max_y++;
    $min_z--; $max_z++;

    # BFS to find all exterior air cells
    my %exterior;
    my @queue = ("$min_x,$min_y,$min_z");
    $exterior{"$min_x,$min_y,$min_z"} = 1;

    while (@queue) {
        my $current = shift @queue;
        my ($x, $y, $z) = split /,/, $current;

        for my $dir (@DIRECTIONS) {
            my ($dx, $dy, $dz) = @$dir;
            my ($nx, $ny, $nz) = ($x + $dx, $y + $dy, $z + $dz);

            # Stay within bounds
            next unless $nx >= $min_x && $nx <= $max_x &&
                        $ny >= $min_y && $ny <= $max_y &&
                        $nz >= $min_z && $nz <= $max_z;

            my $neighbor = "$nx,$ny,$nz";

            # Skip cubes and already visited
            next if exists $cubes->{$neighbor} || exists $exterior{$neighbor};

            $exterior{$neighbor} = 1;
            push @queue, $neighbor;
        }
    }

    # Count faces touching exterior air
    my $surface_area = 0;
    for my $key (keys %$cubes) {
        my ($x, $y, $z) = split /,/, $key;
        for my $dir (@DIRECTIONS) {
            my ($dx, $dy, $dz) = @$dir;
            my $neighbor = ($x + $dx) . "," . ($y + $dy) . "," . ($z + $dz);
            $surface_area++ if exists $exterior{$neighbor};
        }
    }

    return $surface_area;
}

sub main {
    my $script_dir = dirname(__FILE__);
    my $input_file = "$script_dir/../input.txt";

    open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
    my $text = do { local $/; <$fh> };
    close $fh;

    print "Part 1: ", part1($text), "\n";
    print "Part 2: ", part2($text), "\n";
}

main();
