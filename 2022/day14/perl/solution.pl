#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;
use List::Util qw(min max);

# Parse rock paths and return hash of blocked positions
sub parse_paths {
    my ($text) = @_;
    my %rocks;

    for my $line (split /\n/, $text) {
        next unless $line =~ /\S/;
        my @points = split / -> /, $line;

        for my $i (0 .. $#points - 1) {
            my ($x1, $y1) = split /,/, $points[$i];
            my ($x2, $y2) = split /,/, $points[$i + 1];

            if ($x1 == $x2) {
                # Vertical line
                for my $y (min($y1, $y2) .. max($y1, $y2)) {
                    $rocks{"$x1,$y"} = 1;
                }
            } else {
                # Horizontal line
                for my $x (min($x1, $x2) .. max($x1, $x2)) {
                    $rocks{"$x,$y1"} = 1;
                }
            }
        }
    }

    return \%rocks;
}

# Simulate one unit of sand falling
# Returns the resting position as "x,y" or undef if sand falls into abyss
sub simulate_sand {
    my ($blocked, $max_y, $floor) = @_;
    my ($x, $y) = (500, 0);

    while (1) {
        # Check if sand has fallen below all rocks (into abyss)
        if (!$floor && $y > $max_y) {
            return undef;
        }

        # Try to move down
        if ($floor && $y + 1 == $max_y + 2) {
            # Hit the floor
            return "$x,$y";
        } elsif (!exists $blocked->{"$x," . ($y + 1)}) {
            $y++;
        }
        # Try to move down-left
        elsif (!exists $blocked->{($x - 1) . "," . ($y + 1)}) {
            $x--;
            $y++;
        }
        # Try to move down-right
        elsif (!exists $blocked->{($x + 1) . "," . ($y + 1)}) {
            $x++;
            $y++;
        }
        # Sand comes to rest
        else {
            return "$x,$y";
        }
    }
}

# Count sand units that come to rest before sand falls into abyss
sub part1 {
    my ($text) = @_;
    my $rocks = parse_paths($text);

    # Find max y coordinate
    my $max_y = 0;
    for my $key (keys %$rocks) {
        my ($x, $y) = split /,/, $key;
        $max_y = $y if $y > $max_y;
    }

    my %blocked = %$rocks;
    my $count = 0;

    while (1) {
        my $pos = simulate_sand(\%blocked, $max_y, 0);
        last unless defined $pos;
        $blocked{$pos} = 1;
        $count++;
    }

    return $count;
}

# Count sand units until source is blocked (with floor)
sub part2 {
    my ($text) = @_;
    my $rocks = parse_paths($text);

    # Find max y coordinate
    my $max_y = 0;
    for my $key (keys %$rocks) {
        my ($x, $y) = split /,/, $key;
        $max_y = $y if $y > $max_y;
    }

    my %blocked = %$rocks;
    my $count = 0;

    while (1) {
        my $pos = simulate_sand(\%blocked, $max_y, 1);
        $blocked{$pos} = 1;
        $count++;
        last if $pos eq "500,0";
    }

    return $count;
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
