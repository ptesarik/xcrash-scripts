#! /usr/bin/perl

my $reverse = 0;
my $macro = shift @ARGV;
if ($macro eq "-r") {
    $reverse = 1;
    $macro = shift @ARGV;
}

my $rxtrue, my $rxfalse;
if ($macro eq "0") {
    $rxtrue = "^\\s*#\\s*if\\s+\\b0\\b";
    $rxfalse = "^\\s*#\\s*if\\s+\\b1\\b";
} else {
    $rxtrue = "^\\s*#\\s*ifdef\\s+\\b$macro\\b";
    $rxfalse = "^\\s*#\\s*ifndef\\s+\\b$macro\\b";
}

if ($reverse) {
    my $rx = $rxtrue;
    $rxtrue = $rxfalse;
    $rxfalse = $rx;
}

my $remove = 0;
my $ignored = 0;
my @savedctx;
while(<>) {
    if (! /^\s*#/) {
	print unless $remove;
	next;
    }

    if (/^\s*#\s*if/) {
	push @savedctx, $remove, $ignored;
	$ignored = 1;
    }

    if (/$rxtrue/o) {
	$ignored = 0;
	$remove = 0 unless $remove < 0;
    } elsif (/$rxfalse/o) {
	$ignored = 0;
	$remove = 1 unless $remove < 0;
    } elsif ($ignored) {
	print unless $remove;
    } elsif (/^\s*#\s*else/) {
	--$remove;
    } elsif (/^\s*#\s*endif/) {
	# do nothing
    } elsif (!$remove) {
	print;
    }

    if (/^\s*#\s*endif/) {
	$ignored = pop @savedctx;
	$remove = pop @savedctx;
    }
}
