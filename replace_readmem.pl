#! /usr/bin/perl

use warnings;
use Parse::RecDescent;

$dryrun = 0;

$c_grammar=<<'EOF';

call: <skip:''> identifier '(' expression(s /,/) ')' garbage(?)
	{ { fn=>$item[2], args=>$item[4], suffix=>join('',@{$item[-1]}) } }

expression: part(s?)
	{ join('', @{$item[1]}) }
 
part:	subexp
	| '(' expression(s /,/) ')'
		{ '(' . join(',',@{$item[2]}) . ')' }
	| comment
		{ ' ' }
	|/\s+(\\\n)?/

subexp: identifier
	| operator
	| number
	| string

identifier: /([[:alpha:]_][[:alnum:]_]*)/
operator: /[][#&|.!?:+*\/%<=>-]+/

number: /[[:digit:]]+/
	| /0x[[:xdigit:]]+/

string	: m{"			# a leading delimiter
	    (			# zero or more...
	     \\.		# escaped anything
	     |			# or
	     [^"]		# anything but the delimiter
	    )*
	    "}x
	| m{'			# a leading delimiter
	    (			# zero or more...
	     \\.		# escaped anything
	     |			# or
	     [^']		# anything but the delimiter
	    )*
	    '}x

comment: m{//			# comment delimiter
	    [^\n]*		# anything except a newline
	    \n			# then a newline
	   }x
	| m{/\*			# comment opener
	    (?:[^*]+|\*(?!/))*	# anything except */
	    \*/		        # comment closer
            ([ \t]*)?           # trailing blanks or tabs
	   }x

garbage: /.+/s

EOF

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

    my $size = $args->[3];
    $size =~ s/^\s+//;
    $size =~ s/\s*$//;

    if ($size =~ m{sizeof\(
		(?'type'				# store the match
		 short|ushort|int|uint			# short & int
		 |long|ulong|unsigned long|longlong	# long & long long
		 |[[:alpha:] ]+\*			# pointer types
		)}x) {
	my $type = $+{'type'};
	$type = "ulong" if $type eq "unsigned long";
	$type = "ptr" if $type =~ /\*$/;

	my $count = 1;
	if ($size =~ /^sizeof\([^)]+\)\s*\*\s*(?'count'.*)
			|(?'count'.*)\s*\*\s*sizeof\([^)]+\)$/x) {
	    $count = $+{'count'};
	}

	$expr->{fn} = "read$type";
	$args->[3] = replace_nonwhite($args->[3], $count);
    }
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

		# Chop off all that precedes readmem
		if( ! ($line =~ s/(.*?)(\breadmem\b.*)/$2/) ) {
			print "readmem not found at $infile:$linenum\n";
			print OUTFILE $line unless $dryrun;
			next;
		}
		my $prefix = $1;
		my $expr;
		while (! ($expr = $parser->call($line)) ) {
			$line .= <INFILE>
				or die "Unterminated expression at $infile$linenum\n";
			$rdline++;
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

# Find all calls to readmem()
my %calls;
open(CALLS, 'cscope -L -3readmem|') or die "Cannot get readmem call sites";
while(<CALLS>) {
	(my $file, my $func, my $line) = split(" ");
	push @{$calls{$file}}, $line;
}
close(CALLS);

# Create a (simple) C parser
$parser = new Parse::RecDescent($c_grammar)
	or  die "Invalid C grammar";

# Modify the files
while ((my $file, $linearray) = each %calls) {
    print STDERR "Processing $file\n";
    modify_file($file, $linearray);
}
