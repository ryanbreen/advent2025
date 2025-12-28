#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;
use File::Spec;

# Read input file
my $dir = dirname(__FILE__);
my $input_path = File::Spec->catfile($dir, '..', 'input.txt');
open(my $fh, '<', $input_path) or die "Cannot open $input_path: $!";
my @lines = <$fh>;
close($fh);
chomp(@lines);

# Card strength order (higher index = stronger)
my $CARD_STRENGTH = "23456789TJQKA";
my $CARD_STRENGTH_JOKER = "J23456789TQKA";  # J is weakest in Part 2

sub get_hand_type {
    my ($hand) = @_;

    # Count cards
    my %counts;
    $counts{$_}++ for split //, $hand;

    # Sort counts in descending order
    my @sorted_counts = sort { $b <=> $a } values %counts;

    # Determine hand type
    my $pattern = join(",", @sorted_counts);

    if ($pattern eq "5") {
        return 6;  # Five of a kind
    } elsif ($pattern eq "4,1") {
        return 5;  # Four of a kind
    } elsif ($pattern eq "3,2") {
        return 4;  # Full house
    } elsif ($pattern eq "3,1,1") {
        return 3;  # Three of a kind
    } elsif ($pattern eq "2,2,1") {
        return 2;  # Two pair
    } elsif ($pattern eq "2,1,1,1") {
        return 1;  # One pair
    } else {
        return 0;  # High card
    }
}

sub get_hand_type_with_jokers {
    my ($hand) = @_;

    my $joker_count = ($hand =~ tr/J//);

    if ($joker_count == 0) {
        return get_hand_type($hand);
    }
    if ($joker_count == 5) {
        return 6;  # Five of a kind
    }

    # Count non-joker cards
    my %counts;
    for my $c (split //, $hand) {
        $counts{$c}++ if $c ne 'J';
    }

    # Sort counts in descending order
    my @sorted_counts = sort { $b <=> $a } values %counts;

    # Add jokers to the highest count
    $sorted_counts[0] += $joker_count;

    # Determine hand type
    my $pattern = join(",", @sorted_counts);

    if ($pattern eq "5") {
        return 6;  # Five of a kind
    } elsif ($pattern eq "4,1") {
        return 5;  # Four of a kind
    } elsif ($pattern eq "3,2") {
        return 4;  # Full house
    } elsif ($pattern eq "3,1,1") {
        return 3;  # Three of a kind
    } elsif ($pattern eq "2,2,1") {
        return 2;  # Two pair
    } elsif ($pattern eq "2,1,1,1") {
        return 1;  # One pair
    } else {
        return 0;  # High card
    }
}

sub hand_key {
    my ($hand) = @_;

    my $hand_type = get_hand_type($hand);
    my @card_values = map { index($CARD_STRENGTH, $_) } split //, $hand;

    return ($hand_type, @card_values);
}

sub hand_key_with_jokers {
    my ($hand) = @_;

    my $hand_type = get_hand_type_with_jokers($hand);
    my @card_values = map { index($CARD_STRENGTH_JOKER, $_) } split //, $hand;

    return ($hand_type, @card_values);
}

sub compare_hands {
    my ($a_hand, $b_hand) = @_;

    my @a_key = hand_key($a_hand);
    my @b_key = hand_key($b_hand);

    for my $i (0 .. $#a_key) {
        return $a_key[$i] <=> $b_key[$i] if $a_key[$i] != $b_key[$i];
    }
    return 0;
}

sub compare_hands_with_jokers {
    my ($a_hand, $b_hand) = @_;

    my @a_key = hand_key_with_jokers($a_hand);
    my @b_key = hand_key_with_jokers($b_hand);

    for my $i (0 .. $#a_key) {
        return $a_key[$i] <=> $b_key[$i] if $a_key[$i] != $b_key[$i];
    }
    return 0;
}

sub part1 {
    my @hands;
    for my $line (@lines) {
        next unless $line =~ /\S/;
        my ($hand, $bid) = split /\s+/, $line;
        push @hands, [$hand, $bid];
    }

    # Sort by hand strength
    my @sorted = sort { compare_hands($a->[0], $b->[0]) } @hands;

    # Calculate total winnings
    my $total = 0;
    for my $rank (1 .. @sorted) {
        $total += $rank * $sorted[$rank - 1][1];
    }

    return $total;
}

sub part2 {
    my @hands;
    for my $line (@lines) {
        next unless $line =~ /\S/;
        my ($hand, $bid) = split /\s+/, $line;
        push @hands, [$hand, $bid];
    }

    # Sort by hand strength with joker rules
    my @sorted = sort { compare_hands_with_jokers($a->[0], $b->[0]) } @hands;

    # Calculate total winnings
    my $total = 0;
    for my $rank (1 .. @sorted) {
        $total += $rank * $sorted[$rank - 1][1];
    }

    return $total;
}

print "Part 1: " . part1() . "\n";
print "Part 2: " . part2() . "\n";
