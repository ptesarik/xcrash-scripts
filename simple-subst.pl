#! /usr/bin/perl

my $old = shift @ARGV;
if ($old eq "-r") {		# "raw" regexp
    $old = shift @ARGV;
} else {
    $old = "\\b$old\\b";	# otherwise delimited by word boundaries
}
my $new = shift @ARGV;

while (<>) {
    s/$old/$new/go;
    print;
}
