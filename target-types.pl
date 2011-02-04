#! /usr/bin/perl

my $gdb_common = 0;
my $gdb_on = -1;
my $gdb_off = -1;
my $if_nesting = 0;

while (<>) {
    # Do not touch these typedefs:
    if (/^\s*typedef\b.*\b(ulonglong|[su]8|[su]16|[su]32|[su]64|[su]leb128_t)\s*;/) {
	print;
	next;
    }

    # Do not modify GDB_COMMON
    $gdb_common = 1 if $. == 1 && /defs\.h/;
    if (/^\#if/) {
	if (/#ifndef\s+GDB_COMMON/) {
	    $gdb_common = 0;
	    $gdb_on = $if_nesting;
	} elsif (/#ifdef\s+GDB_COMMON/) {
	    $gdb_common = 1;
	    $gdb_off = $if_nesting;
	}
	$if_nesting++;
    } elsif (/^\#endif/) {
	$if_nesting--;
	if ($if_nesting == $gdb_off) {
	    $gdb_common = 0;
	    $gdb_off = -1;
	} elsif ($if_nesting == $gdb_on) {
	    $gdb_common = 1;
	    $gdb_on = -1;
	}
    }

    if (!$gdb_common) {
	# long long types
	s/\bunsigned long long\b/tulonglong/g;
	s/\blong long int\b/tlonglong/g;
	s/\blong long\b/tlonglong/g;
	s/\blonglong\b/tlonglong/g;
	s/\bulonglong\b/tulonglong/g;
    }

    # long types - even GDB_COMMON
    s/\bunsigned long\b/tulong/g;
    s/\blong\b(?!\s*double)/tlong/g;
    s/\bulong\b/tulong/g;

    if (!$gdb_common) {
	# short types
	s/\bshort unsigned int\b/tushort/g;
	s/\bunsigned short\b/tushort/g;
	s/\bshort\b/tshort/g;
	s/\bushort\b/tushort/g;

	# int types
	s/\bunsigned int\b/tuint/g;
	s/\bint\b/tint/g;
	s/\buint\b/tuint/g;
    }

    print;
}
