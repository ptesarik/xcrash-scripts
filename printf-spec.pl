#! /usr/bin/perl

use warnings;
use Parse::RecDescent;

$c_grammar=<<'EOF';

call: <skip:''> identifier_and_ws '(' expression(s /,/) ')' garbage(?)
	{ { fn=>$item[2], args=>$item[4], suffix=>join('',@{$item[-1]}) } }

identifier_and_ws: identifier /\s*/
	{ $item[1] . $item[2] }

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
operator: /[][#~&|^.!?:+*\/%<=>-]+/

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
