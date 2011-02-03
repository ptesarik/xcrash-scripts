#! /usr/bin/perl

use warnings;
use Parse::RecDescent;
use File::Basename;
use File::Spec::Functions;

sub transform
{
    my $expr = shift;
    my $args = $expr->{args};
    my $flags = $args->[2];
    my $opt = $args->[3];

    if( ($opt =~ s/^([[[:space:]\\]*)(\bMKSTR\b.*)/$2/) ) {
	my $prefix = $1;
	my $expr = $parser->call($opt);
	die "'$opt' not parseable" if !defined($expr);
	$opt = $prefix.$expr->{args}[0];
    }

    my $typecast = undef;
    if ($flags =~ /\bLONG_(DEC|HEX)\b/) {
	$typecast = 'ulong';
    } elsif ($flags =~ /\bINT_(DEC|HEX)\b/) {
	$typecast = 'uint';
    } elsif ($flags =~ /\bLONGLONG_HEX\b/) {
	$typecast = 'ulonglong';
	$opt =~ s/^([[:space:]\\]*)&/$1/;
    }
    if (defined($typecast)) {
	$opt =~ s/^([[:space:]\\]*)\([^)]+\)/$1/;
	$opt =~ s/^([[:space:]\\]*)/$1($typecast)/;
    }

    $args->[3] = $opt;
}

sub modify_file
{
	(my $infile, my $linearray) = @_;
	my $outfile = "$infile.new";

	open(INFILE, "<$infile");
	open(OUTFILE, ">$outfile") unless $dryrun;

	$rdline = 0;
	foreach my $linenum (@$linearray) {
		next if $linenum <= $rdline;	# occasional duplicates

		my $line;
		while (defined($line = <INFILE>) && ++$rdline < $linenum) {
			print OUTFILE $line unless $dryrun;
		}

		# Chop off all that precedes mkstring
		if( ! ($line =~ s/(.*?)(\bmkstring\b.*)/$2/) ) {
			print "mkstring not found at $infile:$linenum\n";
			print OUTFILE $line unless $dryrun;
			next;
		}
		my $prefix = $1;
		my $expr;
		while (! defined($expr = $parser->call($line)) ) {
			$line .= <INFILE>
				or die "Unterminated expression at $infile$linenum\n";
			++$rdline;
		}
		transform($expr);
		print OUTFILE $prefix,
			$expr->{fn}, '(', join (',', @{$expr->{args}}), ')',
			$expr->{suffix}
			    unless $dryrun;
	}
	while(<INFILE>) {
		print OUTFILE $_ unless $dryrun;
	}
	close(INFILE);
	close(OUTFILE) unless $dryrun;

	rename($outfile, $infile) unless $dryrun;
}

# Find all calls to mkstring()
my %calls;
open(CALLS, 'cscope -L -3mkstring|') or die "Cannot get mkstring call sites";
while(<CALLS>) {
	(my $file, my $func, my $line) = split(" ");
	push @{$calls{$file}}, $line;
}
close(CALLS);

# Create a (simple) C parser
my $c_grammar = do {
    local $/ = undef;
    open my $fh, "<".catfile(dirname($0), "c-grammar.syntax")
        or die "could not open c-grammar syntax file: $!";
    <$fh>;
};
$parser = new Parse::RecDescent($c_grammar)
	or  die "Invalid C grammar";

# Modify the files
while ((my $file, $linearray) = each %calls) {
    print STDERR "Processing $file\n";
    modify_file($file, $linearray);
}
