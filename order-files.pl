#! /usr/bin/perl

# Unfortunately, the sources cannot be parsed in any order...
# The following has proved productive:
#   1. Move *.h to begin
#   2. Move defs.h to begin
#   3. Move unwind* to end
#   4. Move unwind_i.h to end
#   5. Move lkcd* to end

undef $/;
my $list = <>;
$list =~ s/\bconfigure\.c\b//;
my @list = split(/\s+/, $list);

my @begin;
my @end;

# 1. Move *.h to begin
foreach my $file (@list) {
    if ($file =~ /\.h$/) {
	push @begin, $file;
    } else {
	push @end, $file;
    }
}
@list = ( @begin, @end );
undef @begin;
undef @end;

# 2. Move defs.h to begin
foreach my $file (@list) {
    if ($file eq "defs.h") {
	push @begin, $file;
    } else {
	push @end, $file;
    }
}
@list = ( @begin, @end );
undef @begin;
undef @end;

# 3. Move unwind* to end
foreach my $file (@list) {
    if ($file =~ /^unwind/) {
	push @end, $file;
    } else {
	push @begin, $file;
    }
}
@list = ( @begin, @end );
undef @begin;
undef @end;

# 4. Move unwind_i.h to end
foreach my $file (@list) {
    if ($file eq "unwind_i.h") {
	push @end, $file;
    } else {
	push @begin, $file;
    }
}
@list = ( @begin, @end );
undef @begin;
undef @end;

# 5. Move lkcd* to end
foreach my $file (@list) {
    if ($file =~ /^lkcd/) {
	push @end, $file;
    } else {
	push @begin, $file;
    }
}
@list = ( @begin, @end );
undef @begin;
undef @end;

print join(" ", @list), "\n";
