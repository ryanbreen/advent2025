#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;
use File::Spec;

sub parse_input {
    my ($filename) = @_;
    open my $fh, '<', $filename or die "Cannot open $filename: $!";
    my @lines;
    while (<$fh>) {
        chomp;
        push @lines, $_ if $_;
    }
    close $fh;
    return @lines;
}

sub priority {
    my ($char) = @_;
    my $ord = ord($char);
    if ($ord >= ord('a') && $ord <= ord('z')) {
        return $ord - ord('a') + 1;
    } else {
        return $ord - ord('A') + 27;
    }
}

sub set_intersection {
    my @sets = @_;
    my %result;
    # Start with the first set
    %result = map { $_ => 1 } keys %{$sets[0]};
    # Intersect with each subsequent set
    for my $i (1 .. $#sets) {
        my %new_result;
        for my $key (keys %result) {
            $new_result{$key} = 1 if exists $sets[$i]->{$key};
        }
        %result = %new_result;
    }
    return keys %result;
}

sub part1 {
    my @rucksacks = @_;
    my $total = 0;
    for my $rucksack (@rucksacks) {
        my $mid = length($rucksack) / 2;
        my $first = substr($rucksack, 0, $mid);
        my $second = substr($rucksack, $mid);

        my %first_set = map { $_ => 1 } split //, $first;
        my %second_set = map { $_ => 1 } split //, $second;

        my @common = set_intersection(\%first_set, \%second_set);
        $total += priority($common[0]) if @common;
    }
    return $total;
}

sub part2 {
    my @rucksacks = @_;
    my $total = 0;
    for (my $i = 0; $i < @rucksacks; $i += 3) {
        my @group = @rucksacks[$i .. $i + 2];

        my @sets;
        for my $r (@group) {
            my %set = map { $_ => 1 } split //, $r;
            push @sets, \%set;
        }

        my @common = set_intersection(@sets);
        $total += priority($common[0]) if @common;
    }
    return $total;
}

sub main {
    my $script_dir = dirname(File::Spec->rel2abs(__FILE__));
    my $input_file = File::Spec->catfile($script_dir, '..', 'input.txt');

    my @rucksacks = parse_input($input_file);

    print "Part 1: ", part1(@rucksacks), "\n";
    print "Part 2: ", part2(@rucksacks), "\n";
}

main();
