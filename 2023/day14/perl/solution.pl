#!/usr/bin/env perl
use strict;
use warnings;
use v5.10;

# Parse input into a 2D grid
sub parse_input {
    my ($text) = @_;
    my @lines = split /\n/, $text;
    return [ map { [ split //, $_ ] } @lines ];
}

# Tilt the grid north, moving all round rocks up
sub tilt_north {
    my ($grid) = @_;
    my $rows = scalar @$grid;
    my $cols = scalar @{$grid->[0]};

    for my $col (0 .. $cols - 1) {
        my $write_pos = 0;
        for my $row (0 .. $rows - 1) {
            if ($grid->[$row][$col] eq '#') {
                $write_pos = $row + 1;
            }
            elsif ($grid->[$row][$col] eq 'O') {
                $grid->[$row][$col] = '.';
                $grid->[$write_pos][$col] = 'O';
                $write_pos++;
            }
        }
    }
}

# Tilt the grid south, moving all round rocks down
sub tilt_south {
    my ($grid) = @_;
    my $rows = scalar @$grid;
    my $cols = scalar @{$grid->[0]};

    for my $col (0 .. $cols - 1) {
        my $write_pos = $rows - 1;
        for (my $row = $rows - 1; $row >= 0; $row--) {
            if ($grid->[$row][$col] eq '#') {
                $write_pos = $row - 1;
            }
            elsif ($grid->[$row][$col] eq 'O') {
                $grid->[$row][$col] = '.';
                $grid->[$write_pos][$col] = 'O';
                $write_pos--;
            }
        }
    }
}

# Tilt the grid west, moving all round rocks left
sub tilt_west {
    my ($grid) = @_;
    my $rows = scalar @$grid;
    my $cols = scalar @{$grid->[0]};

    for my $row (0 .. $rows - 1) {
        my $write_pos = 0;
        for my $col (0 .. $cols - 1) {
            if ($grid->[$row][$col] eq '#') {
                $write_pos = $col + 1;
            }
            elsif ($grid->[$row][$col] eq 'O') {
                $grid->[$row][$col] = '.';
                $grid->[$row][$write_pos] = 'O';
                $write_pos++;
            }
        }
    }
}

# Tilt the grid east, moving all round rocks right
sub tilt_east {
    my ($grid) = @_;
    my $rows = scalar @$grid;
    my $cols = scalar @{$grid->[0]};

    for my $row (0 .. $rows - 1) {
        my $write_pos = $cols - 1;
        for (my $col = $cols - 1; $col >= 0; $col--) {
            if ($grid->[$row][$col] eq '#') {
                $write_pos = $col - 1;
            }
            elsif ($grid->[$row][$col] eq 'O') {
                $grid->[$row][$col] = '.';
                $grid->[$row][$write_pos] = 'O';
                $write_pos--;
            }
        }
    }
}

# Perform one spin cycle: N, W, S, E
sub spin_cycle {
    my ($grid) = @_;
    tilt_north($grid);
    tilt_west($grid);
    tilt_south($grid);
    tilt_east($grid);
}

# Convert grid to hashable string for cycle detection
sub grid_to_string {
    my ($grid) = @_;
    return join("\n", map { join('', @$_) } @$grid);
}

# Calculate total load on north support beams
sub calculate_load {
    my ($grid) = @_;
    my $rows = scalar @$grid;
    my $total = 0;

    for my $row (0 .. $rows - 1) {
        for my $cell (@{$grid->[$row]}) {
            if ($cell eq 'O') {
                $total += $rows - $row;
            }
        }
    }
    return $total;
}

# Deep copy a grid
sub copy_grid {
    my ($grid) = @_;
    return [ map { [ @$_ ] } @$grid ];
}

# Part 1: Tilt north and calculate load
sub part1 {
    my ($grid) = @_;
    my $copy = copy_grid($grid);
    tilt_north($copy);
    return calculate_load($copy);
}

# Part 2: Run 1 billion spin cycles and calculate load
sub part2 {
    my ($grid) = @_;
    my $copy = copy_grid($grid);
    my $target = 1_000_000_000;

    my %seen;
    my $cycle_num = 0;

    while ($cycle_num < $target) {
        my $state = grid_to_string($copy);
        if (exists $seen{$state}) {
            my $cycle_start = $seen{$state};
            my $cycle_length = $cycle_num - $cycle_start;
            my $remaining = ($target - $cycle_num) % $cycle_length;
            for (1 .. $remaining) {
                spin_cycle($copy);
            }
            return calculate_load($copy);
        }

        $seen{$state} = $cycle_num;
        spin_cycle($copy);
        $cycle_num++;
    }

    return calculate_load($copy);
}

# Main
sub main {
    my $input_file = $0;
    $input_file =~ s|[^/]+$||;
    $input_file .= '../input.txt';

    open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
    my $text = do { local $/; <$fh> };
    close $fh;

    # Remove trailing newline
    chomp $text;

    my $grid = parse_input($text);

    say "Part 1: ", part1($grid);
    say "Part 2: ", part2($grid);
}

main();
