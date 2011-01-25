#! /usr/bin/perl

use warnings;
use Parse::RecDescent;

$c_grammar=<<'EOF';

call: <skip:''> identifier '(' expression(s /,/) ')' garbage(?)
	{ { fn=>$item[2], args=>$item[4], suffix=>join('',@{$item[-1]}) } }

expression: part(s?)
	{ join('', @{$item[1]}) }
 
part:	subexp
	| '(' expression(s /,/) ')'
		{ '(' . join('',@{$item[2]}) . ')' }
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

sub transform
{
	my $expr = shift;
# Look at $expr->{args}, modify their number and type,
# change $expr->{fn} if necessary, etc.
}

sub modify_file
{
	(my $infile, my $linearray) = @_;
	my $outfile = "$infile.new";

	open(INFILE, "<$infile");
	open(OUTFILE, ">$outfile");

	$rdline = 1;
	foreach my $linenum (@$linearray) {
		next if $linenum <= $rdline;	# occasional duplicates

		my $line;
		while (($line = <INFILE>) && $rdline++ < $linenum) {
			print OUTFILE $line;
		}

		# Chop off all that precedes readmem
		if( ! ($line =~ s/(.*?)(\breadmem\b.*)/$2/) ) {
			print "readmem not found at $infile:$linenum\n";
			print $line;
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
			$expr->{suffix};
	}
	while(<INFILE>) {
		print OUTFILE $_;
	}
	close(INFILE);
	close(OUTFILE);
}

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

# Create a (simple) C parser
$parser = new Parse::RecDescent($c_grammar)
	or  die "Invalid C grammar";

# Modify the files
while ((my $file, $linearray) = each %calls) {
	modify_file($file, $linearray);
}
