#!/usr/bin/env perl
use strict;
use warnings;
use v5.10;

# Day 22: Monkey Market - Pseudorandom number generation for market prices

sub next_secret {
    my ($secret) = @_;

    # Step 1: multiply by 64, mix, prune
    $secret ^= ($secret << 6);  # * 64 = << 6
    $secret &= 0xFFFFFF;        # % 16777216 = & (2^24 - 1)

    # Step 2: divide by 32, mix, prune
    $secret ^= ($secret >> 5);  # // 32 = >> 5
    $secret &= 0xFFFFFF;

    # Step 3: multiply by 2048, mix, prune
    $secret ^= ($secret << 11); # * 2048 = << 11
    $secret &= 0xFFFFFF;

    return $secret;
}

sub generate_secrets {
    my ($initial, $count) = @_;
    my @secrets = ($initial);
    my $secret = $initial;

    for (1..$count) {
        $secret = next_secret($secret);
        push @secrets, $secret;
    }

    return @secrets;
}

sub part1 {
    my @initial_secrets = @_;
    my $total = 0;

    for my $initial (@initial_secrets) {
        my $secret = $initial;
        for (1..2000) {
            $secret = next_secret($secret);
        }
        $total += $secret;
    }

    return $total;
}

sub part2 {
    my @initial_secrets = @_;

    # Map from (change1, change2, change3, change4) -> total bananas
    my %sequence_totals;

    for my $initial (@initial_secrets) {
        # Generate 2001 secrets (initial + 2000 new)
        my @secrets = generate_secrets($initial, 2000);
        my @prices = map { $_ % 10 } @secrets;

        # Calculate changes
        my @changes;
        for my $i (0..$#prices-1) {
            push @changes, $prices[$i+1] - $prices[$i];
        }

        # Track first occurrence of each 4-change sequence for this buyer
        my %seen;
        for my $i (0..$#changes-3) {
            my $seq = join(',', @changes[$i..$i+3]);

            if (!exists $seen{$seq}) {
                $seen{$seq} = 1;
                # Price we get is after these 4 changes
                $sequence_totals{$seq} += $prices[$i + 4];
            }
        }
    }

    my $max_bananas = 0;
    for my $total (values %sequence_totals) {
        $max_bananas = $total if $total > $max_bananas;
    }

    return $max_bananas;
}

# Read input
my $input_file = '../input.txt';
open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
my @initial_secrets;
while (my $line = <$fh>) {
    chomp $line;
    next unless $line =~ /\S/;
    push @initial_secrets, int($line);
}
close $fh;

# Solve both parts
say "Part 1: ", part1(@initial_secrets);
say "Part 2: ", part2(@initial_secrets);
