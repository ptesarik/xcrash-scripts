#! /usr/bin/perl

my $ident = shift @ARGV;
my $inblock = 0;
my $comment = '';
my $incomment = 0;
while(<>) {
    $incomment = 1 if /\/\*/;
    if ($incomment) {
	$comment .= $_;
	$incomment = 0 if /\*\//;
	next;
    }

    $inblock = 1 if /\b$ident\b\s*{$/o;
    print $comment unless $inblock;
    $comment = '';
    print unless $inblock;
    $inblock = 0 if /}/;
}
print $comment unless $inblock;
