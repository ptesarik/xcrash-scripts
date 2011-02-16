#! /usr/bin/perl

my $reverse = 0;
my $macro = shift @ARGV;
if ($macro eq "-r") {
    $reverse = 1;
    $macro = shift @ARGV;
}

my $rxtrue, my $rxfalse;
if ($macro eq "0") {
    $rxtrue = "^\\s*#if\\s+\\b0\\b";
    $rxfalse = "^\\s*#if\\s+\\b1\\b";
} else {
    $rxtrue = "^\\s*#ifdef\\s+\\b$macro\\b";
    $rxfalse = "^\\s*#ifndef\\s+\\b$macro\\b";
}

if ($reverse) {
    my $rx = $rxtrue;
    $rxtrue = $rxfalse;
    $rxfalse = $rx;
}

my $output = 1;
my $inverted = 0;
my $nest = 0;
my @savedctx;
while(<>) {
    if (! /^\s*#/) {
	print if $output;
	next;
    }

    if (/^\s*#if/) {
	push @savedctx, $output, $inverted;
	++$nest;
    }

    if (/$rxtrue/o) {
	$inverted = 1;
    } elsif (/$rxfalse/o) {
	$inverted = !$output;
	$output = 0;
    } elsif (/^\s*#else/) {
	if ($output && !$inverted) {
	    print;
	} else {
	    $output = !$inverted;
	    $inverted = !$inverted;
	}
    } elsif (/\s*#endif/) {
	print if $output && !$inverted;
    } elsif ($output) {
	print;
    }

    if (/^\s*#endif/) {
	$inverted = pop @savedctx;
	$output = pop @savedctx;
	--$nest;
    }
}
