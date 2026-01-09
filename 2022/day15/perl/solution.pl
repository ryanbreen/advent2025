#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;
use Cwd 'abs_path';

# Parse sensor and beacon positions from input
sub parse_sensors {
    my ($text) = @_;
    my @sensors;

    for my $line (split /\n/, $text) {
        next unless $line =~ /Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)/;
        my ($sx, $sy, $bx, $by) = ($1, $2, $3, $4);
        my $dist = abs($sx - $bx) + abs($sy - $by);  # Manhattan distance
        push @sensors, [$sx, $sy, $bx, $by, $dist];
    }

    return \@sensors;
}

# Merge overlapping ranges
sub merge_ranges {
    my ($ranges) = @_;
    return [] unless @$ranges;

    my @sorted = sort { $a->[0] <=> $b->[0] || $a->[1] <=> $b->[1] } @$ranges;
    my @merged = ($sorted[0]);

    for my $i (1 .. $#sorted) {
        my ($start, $end) = @{$sorted[$i]};
        if ($start <= $merged[-1][1] + 1) {
            $merged[-1][1] = $end if $end > $merged[-1][1];
        } else {
            push @merged, [$start, $end];
        }
    }

    return \@merged;
}

# Get ranges covered by sensors at a specific row
sub get_coverage_at_row {
    my ($sensors, $row) = @_;
    my @ranges;

    for my $sensor (@$sensors) {
        my ($sx, $sy, $bx, $by, $dist) = @$sensor;
        my $row_dist = abs($sy - $row);
        next if $row_dist > $dist;  # Sensor doesn't reach this row

        my $x_spread = $dist - $row_dist;
        push @ranges, [$sx - $x_spread, $sx + $x_spread];
    }

    return merge_ranges(\@ranges);
}

# Part 1: Count positions that cannot contain a beacon at row y=2000000
sub part1 {
    my ($sensors) = @_;
    my $target_row = 2000000;

    my $ranges = get_coverage_at_row($sensors, $target_row);

    # Count total coverage
    my $total = 0;
    for my $range (@$ranges) {
        $total += $range->[1] - $range->[0] + 1;
    }

    # Subtract beacons that are on this row
    my %beacons_on_row;
    for my $sensor (@$sensors) {
        my ($sx, $sy, $bx, $by, $dist) = @$sensor;
        if ($by == $target_row) {
            $beacons_on_row{$bx} = 1;
        }
    }

    return $total - scalar(keys %beacons_on_row);
}

# Part 2: Find the distress beacon's tuning frequency
sub part2 {
    my ($sensors) = @_;
    my $max_coord = 4000000;

    for my $row (0 .. $max_coord) {
        my $ranges = get_coverage_at_row($sensors, $row);

        # Clip ranges to search area
        my @clipped;
        for my $range (@$ranges) {
            my ($start, $end) = @$range;
            next if $end < 0 || $start > $max_coord;
            push @clipped, [
                $start < 0 ? 0 : $start,
                $end > $max_coord ? $max_coord : $end
            ];
        }

        my $merged = merge_ranges(\@clipped);

        # Check if full row is covered
        if (@$merged == 1 && $merged->[0][0] == 0 && $merged->[0][1] == $max_coord) {
            next;
        }

        # Found a gap - the beacon is in the gap
        my $x;
        if (@$merged > 1) {
            # Gap between ranges
            $x = $merged->[0][1] + 1;
        } elsif ($merged->[0][0] > 0) {
            $x = 0;
        } else {
            $x = $merged->[0][1] + 1;
        }

        return $x * 4000000 + $row;
    }

    return undef;
}

sub main {
    my $script_dir = dirname(abs_path($0));
    my $input_file = "$script_dir/../input.txt";

    open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
    my $text = do { local $/; <$fh> };
    close $fh;

    my $sensors = parse_sensors($text);

    print "Part 1: ", part1($sensors), "\n";
    print "Part 2: ", part2($sensors), "\n";
}

main();
