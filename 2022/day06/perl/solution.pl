#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;
use File::Spec;

sub find_marker {
    my ($data, $window_size) = @_;
    my $len = length($data);

    for my $i ($window_size .. $len) {
        my $window = substr($data, $i - $window_size, $window_size);
        my %seen;
        $seen{$_}++ for split //, $window;
        if (scalar(keys %seen) == $window_size) {
            return $i;
        }
    }
    return -1;
}

sub part1 {
    my ($data) = @_;
    return find_marker($data, 4);
}

sub part2 {
    my ($data) = @_;
    return find_marker($data, 14);
}

sub main {
    my $script_dir = dirname(__FILE__);
    my $input_file = File::Spec->catfile($script_dir, '..', 'input.txt');

    open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
    my $data = do { local $/; <$fh> };
    close $fh;

    $data =~ s/\s+$//;  # Strip trailing whitespace

    print "Part 1: ", part1($data), "\n";
    print "Part 2: ", part2($data), "\n";
}

main();
