#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;
use File::Spec;

# Read input file
my $dir = dirname(__FILE__);
my $input_file = File::Spec->catfile($dir, '..', 'input.txt');
open(my $fh, '<', $input_file) or die "Cannot open $input_file: $!";
my @lines = <$fh>;
close($fh);
chomp(@lines);

# Parse grid
my @grid = map { [split //] } @lines;

# Direction vectors
my %directions = (
    '^' => [0, -1],
    'v' => [0, 1],
    '<' => [-1, 0],
    '>' => [1, 0]
);

# Find starting position and direction
sub find_start {
    my ($grid) = @_;

    for my $y (0 .. $#{$grid}) {
        for my $x (0 .. $#{$grid->[$y]}) {
            my $char = $grid->[$y][$x];
            if (exists $directions{$char}) {
                return {
                    x => $x,
                    y => $y,
                    dir => $directions{$char},
                    symbol => $char
                };
            }
        }
    }
    return undef;
}

# Turn right 90 degrees
sub turn_right {
    my ($dir) = @_;
    my ($dx, $dy) = @$dir;
    # Up (0,-1) -> Right (1,0)
    # Right (1,0) -> Down (0,1)
    # Down (0,1) -> Left (-1,0)
    # Left (-1,0) -> Up (0,-1)
    return [-$dy, $dx];
}

# Simulate guard patrol
# If check_loop is true, returns undef if loop detected, otherwise returns visited positions hash ref
# If check_loop is false, returns count of visited positions
sub simulate_patrol {
    my ($grid, $check_loop) = @_;

    my $start = find_start($grid);
    return 0 unless $start;

    my $x = $start->{x};
    my $y = $start->{y};
    my $dir = $start->{dir};

    my %visited;
    my %states;  # Track position+direction states for loop detection
    $visited{"$x,$y"} = 1;

    my $height = scalar @$grid;
    my $width = scalar @{$grid->[0]};

    while (1) {
        # For loop detection, track state as position+direction
        if ($check_loop) {
            my ($dx, $dy) = @$dir;
            my $state = "$x,$y,$dx,$dy";
            if (exists $states{$state}) {
                # Loop detected!
                return undef;
            }
            $states{$state} = 1;
        }

        # Calculate next position
        my ($dx, $dy) = @$dir;
        my $next_x = $x + $dx;
        my $next_y = $y + $dy;

        # Check if guard leaves the map
        if ($next_x < 0 || $next_x >= $width || $next_y < 0 || $next_y >= $height) {
            last;
        }

        # Check if there's an obstacle ahead
        if ($grid->[$next_y][$next_x] eq '#') {
            # Turn right
            $dir = turn_right($dir);
        } else {
            # Move forward
            $x = $next_x;
            $y = $next_y;
            $visited{"$x,$y"} = 1;
        }
    }

    return $check_loop ? \%visited : scalar keys %visited;
}

# Part 1
sub part1 {
    return simulate_patrol(\@grid, 0);
}

# Part 2
sub part2 {
    # First get all positions the guard visits in normal patrol
    my $visited = simulate_patrol(\@grid, 1);
    return 0 unless ref($visited) eq 'HASH';

    my $start = find_start(\@grid);
    my $loop_count = 0;

    my $height = scalar @grid;
    my $width = scalar @{$grid[0]};

    # Try placing an obstruction at each visited position (except start)
    for my $pos (keys %$visited) {
        my ($x, $y) = split /,/, $pos;

        # Skip the starting position
        next if ($x == $start->{x} && $y == $start->{y});

        # Skip if already an obstacle
        next if $grid[$y][$x] eq '#';

        # Create a copy of the grid with the new obstruction
        my @test_grid;
        for my $row (0 .. $height - 1) {
            push @test_grid, [@{$grid[$row]}];
        }
        $test_grid[$y][$x] = '#';

        # Simulate with loop detection
        my $result = simulate_patrol(\@test_grid, 1);

        # If result is undef, a loop was detected
        if (!defined $result) {
            $loop_count++;
        }
    }

    return $loop_count;
}

print "Part 1: ", part1(), "\n";
print "Part 2: ", part2(), "\n";
