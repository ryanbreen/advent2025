#!/usr/bin/env perl
use strict;
use warnings;

# HASH algorithm: for each char, current = ((current + ASCII) * 17) % 256
sub hash_algorithm {
    my ($s) = @_;
    my $current = 0;
    for my $c (split //, $s) {
        $current = (($current + ord($c)) * 17) % 256;
    }
    return $current;
}

# Part 1: Sum of HASH values for all comma-separated steps
sub part1 {
    my ($steps) = @_;
    my $sum = 0;
    $sum += hash_algorithm($_) for @$steps;
    return $sum;
}

# Part 2: HASHMAP procedure with 256 boxes
sub part2 {
    my ($steps) = @_;

    # Each box is an array of [label, focal_length] pairs (ordered)
    my @boxes;
    for (0..255) {
        $boxes[$_] = [];
    }

    for my $step (@$steps) {
        if ($step =~ /^([a-z]+)=(\d+)$/) {
            my ($label, $focal) = ($1, $2);
            my $box_num = hash_algorithm($label);

            # Check if lens already exists in box
            my $found = 0;
            for my $lens (@{$boxes[$box_num]}) {
                if ($lens->[0] eq $label) {
                    $lens->[1] = $focal;
                    $found = 1;
                    last;
                }
            }
            # Add new lens if not found
            unless ($found) {
                push @{$boxes[$box_num]}, [$label, $focal];
            }
        }
        elsif ($step =~ /^([a-z]+)-$/) {
            my $label = $1;
            my $box_num = hash_algorithm($label);

            # Remove lens with matching label
            @{$boxes[$box_num]} = grep { $_->[0] ne $label } @{$boxes[$box_num]};
        }
    }

    # Calculate focusing power
    my $total = 0;
    for my $box_num (0..255) {
        my $slot = 1;
        for my $lens (@{$boxes[$box_num]}) {
            $total += ($box_num + 1) * $slot * $lens->[1];
            $slot++;
        }
    }

    return $total;
}

sub main {
    my $input_file = $0;
    $input_file =~ s|[^/]+$|../input.txt|;

    open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
    my $text = do { local $/; <$fh> };
    close $fh;

    # Remove newlines and split by comma
    $text =~ s/\n//g;
    $text =~ s/^\s+|\s+$//g;
    my @steps = split /,/, $text;

    print "Part 1: ", part1(\@steps), "\n";
    print "Part 2: ", part2(\@steps), "\n";
}

main();
