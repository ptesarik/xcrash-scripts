#! /usr/bin/perl

use warnings;
use Parse::RecDescent;
use File::Basename;
use File::Spec::Functions;

sub replace_nonwhite
{
    (my $orig, $new) = @_;
    $orig =~ /^\s*/;
    my $prefix = $&;
    $new =~ s/^\s*/$prefix/;
    return $new;
}

sub transform
{
    my $expr = shift;
    my $args = $expr->{args};
    my $pos;
    my $fn = $expr->{fn};

    $fn =~ s/\s+$//;
    if ($fn eq "printf") {
	$pos = 0;
    } elsif ($fn eq "fprintf" || $fn eq "sprintf") {
	$pos = 1;
    } elsif ($fn eq "snprintf" || $fn eq "vsnprintf") {
	$pos = 2;
    } else {
	die "Unsupported function: ",$fn;
    }
    my $fmt = $args->[$pos];
    $fmt =~ s/(%[-0-9.*]*)(l*[dioux])/$1"PRI$2"/g;
    $fmt =~ s/(PRI[a-z]+)""$/$1/;
    $args->[$pos] = $fmt;
}

# Create a (simple) C parser
my $c_grammar = do {
    local $/ = undef;
    open my $fh, "<".catfile(dirname($0), "c-grammar.syntax")
        or die "could not open c-grammar syntax file: $!";
    <$fh>;
};
$parser = new Parse::RecDescent($c_grammar)
	or  die "Invalid C grammar";

$pattern = '\b([dfs]|v?sn|)printf\s*\(\b';
while (<>) {
    if (/^\s*("|\/\*|\/\/)/ || !/$pattern/o) {
	print;
	next;
    }

    # Chop off all that precedes printf
    if (! s/(.*?)($pattern)/$2/o ) {
	print STDERR "printf not found at line $.\n";
	print;
	next;
    }
    my $prefix = $1;
    my $expr;
    $line = $_;
    my $startline = $.;
    while (! defined($expr = $parser->call($line)) ) {
	defined(my $ext = <>)
	    or die "Unterminated expression at line $startline: $line\n";
	$line .= $ext;
    }
    transform($expr);
    print($prefix,
	  $expr->{fn}, '(', join (',', @{$expr->{args}}), ')',
	  $expr->{suffix});
}
