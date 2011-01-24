#! /usr/bin/perl

use warnings;

# Generate cscope.files and cscope.out
system('make cscope < /dev/null');

# Find all calls to readmem()
my %calls;
open(CALLS, 'cscope -L -3readmem|') or die "Cannot get readmem call sites";
while(<CALLS>) {
	(my $file, my $func, my $line) = split(" ");
	push @{$calls{$file}}, $line;
}
close(CALLS);

while ((my $file, $linearray) = each %calls) {
	print "$file:";
	foreach my $line (@$linearray) {
		print " $line";
	}
	print "\n";
}
