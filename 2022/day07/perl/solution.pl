#!/usr/bin/env perl
use strict;
use warnings;
use File::Basename;
use File::Spec;

sub parse_filesystem {
    my ($lines) = @_;
    my @path;
    my %dir_sizes;

    for my $line (@$lines) {
        if ($line =~ /^\$ cd (.+)$/) {
            my $target = $1;
            if ($target eq '/') {
                @path = ('/');
            } elsif ($target eq '..') {
                pop @path;
            } else {
                push @path, $target;
            }
        } elsif ($line =~ /^\$ ls/) {
            next;
        } elsif ($line =~ /^dir /) {
            next;
        } else {
            # It's a file with size
            my ($size) = $line =~ /^(\d+)/;
            next unless defined $size;

            # Add size to current directory and all parent directories
            for my $i (0 .. $#path) {
                my $dir_path = join('/', @path[0 .. $i]) || '/';
                $dir_sizes{$dir_path} += $size;
            }
        }
    }

    return \%dir_sizes;
}

sub part1 {
    my ($dir_sizes) = @_;
    my $total = 0;
    for my $size (values %$dir_sizes) {
        $total += $size if $size <= 100000;
    }
    return $total;
}

sub part2 {
    my ($dir_sizes) = @_;
    my $total_space = 70000000;
    my $needed_space = 30000000;
    my $used_space = $dir_sizes->{'/'};
    my $free_space = $total_space - $used_space;
    my $need_to_free = $needed_space - $free_space;

    # Find smallest directory >= need_to_free
    my $min_size;
    for my $size (values %$dir_sizes) {
        if ($size >= $need_to_free) {
            $min_size = $size if !defined($min_size) || $size < $min_size;
        }
    }
    return $min_size;
}

sub main {
    my $script_dir = dirname(File::Spec->rel2abs(__FILE__));
    my $input_file = File::Spec->catfile($script_dir, '..', 'input.txt');

    open my $fh, '<', $input_file or die "Cannot open $input_file: $!";
    my @lines = <$fh>;
    close $fh;

    chomp @lines;

    my $dir_sizes = parse_filesystem(\@lines);

    print "Part 1: ", part1($dir_sizes), "\n";
    print "Part 2: ", part2($dir_sizes), "\n";
}

main();
