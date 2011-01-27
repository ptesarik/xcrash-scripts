#! /usr/bin/perl

my $old = shift @ARGV;
my $new = shift @ARGV;

while (<>) {
    s/\b$old\b/$new/go;
    print;
}
