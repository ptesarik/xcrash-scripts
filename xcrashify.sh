#! /bin/sh

set -e

# run this script in crash top-level directory to add the
# cross-platform capability

# assume that scripts are accessible the same way as this script
scriptdir=`dirname "$0"`
subst="$scriptdir"/simple-subst.pl
remove="$scriptdir"/remove-block.pl

################################################################
# Upstreamed patches first...

# Configure GDB_CONF_FLAGS from configure
# (sent upstreams already)
quilt import "$scriptdir"/configure-gdb-conf-flags.patch
quilt push

quilt import "$scriptdir"/remove-VOID_PTR.patch
quilt push

quilt import "$scriptdir"/gdb-does-not-need-syment.patch
quilt push

################################################################
# Endianity-related problems

# Re-generate cscope.files and cscope.out
rm -f cscope.files cscope.out
make cscope < /dev/null

# Find and convert calls to readmem
quilt new replace-readmem.patch
quilt add *.c *.h
"$scriptdir"/replace_readmem.pl
quilt refresh -p ab --no-timestamp

# Add readlong, readint, etc. functions
quilt import "$scriptdir"/readtype.patch
quilt push

# Modify the "facilitator" macros
quilt import "$scriptdir"/facilitators.patch
quilt push

################################################################
# Target types next...

# Introduce target types
quilt import "$scriptdir"/target-types.patch
quilt push

# Replace
rm -f cscope.files cscope.out
make cscope < /dev/null
quilt new target-types-use.patch
files=`cat cscope.files`
quilt add $files
for f in $files; do
	if [ "$f" != configure.c -a "${f#va}" == "$f" -a "${f#qemu}" == "$f" -a "$f" != kvmdump.h -a "$f" != gdb_interface.c ]; then
		"$scriptdir"/target-types.pl "$f" > "$f".new && mv "$f".new "$f"
	fi
done
quilt refresh -p ab --no-timestamp

# Type change needs some further fixups
quilt import "$scriptdir"/unwind-x86-cleanups.patch
quilt push
quilt import "$scriptdir"/target-ptr-fixup.patch
quilt push

# Introduce target timeval
quilt import "$scriptdir"/target-timeval.patch
quilt push

quilt new target-timeval-use.patch
old='struct timeval'
new='struct ttimeval'
files=*.[ch]
quilt add "$files"
for f in $files; do
	if [ "$f" != remote.c -a "$f" != s390dbf.c ]; then
	    $subst "$old" "$new" "$f" > "$f".new && mv "$f".new "$f"
	fi
done
quilt refresh -p ab --no-timestamp

# Introduce target off_t
quilt import "$scriptdir"/target-off_t.patch
quilt push

quilt new target-off_t-use.patch
old='off_t'
new='toff_t'
files=diskdump.h
quilt add "$files"
for f in $files; do
	$subst "$old" "$new" "$f" > "$f".new && mv "$f".new "$f"
done
quilt refresh -p ab --no-timestamp

# Provide a platform-independent struct pt_regs
quilt import "$scriptdir"/arch-pt-regs.patch
quilt push

# Use the new pt_regs definitions where possible
quilt new pt-regs-ppc64.patch
old='struct ppc64_pt_regs'
new='struct pt_regs_ppc64'
files=ppc64.c
quilt add "$files"
for f in $files; do
    $subst "$old" "$new" "$f" > "$f".new && mv "$f".new "$f"
done
files=defs.h
quilt add "$files"
for f in $files; do
    $remove "$old" "$f" > "$f".new && mv "$f".new "$f"
done
quilt refresh -p ab --no-timestamp

quilt new pt-regs-x86_64.patch
old='struct pt_regs'
new='struct pt_regs_x86_64'
files=unwind_x86_64.h
quilt add "$files"
for f in $files; do
    $remove "$old" "$f" | $subst "$old" "$new" > "$f".new && mv "$f".new "$f"
done
quilt refresh -p ab --no-timestamp

quilt new use-ia64_fpreg_t.patch
old='struct ia64_fpreg'
new='ia64_fpreg_t'
files="ia64.c lkcd_dump_v7.h lkcd_dump_v8.h lkcd_fix_mem.h unwind.c unwind.h"
quilt add "$files"
for f in $files; do
    $subst "$old" "$new" "$f" > "$f".new && mv "$f".new "$f"
done
quilt refresh -p ab --no-timestamp

################################################################
# Print formatters

# Introduce target print formatters
quilt import "$scriptdir"/target-printf-spec.patch
quilt push

# Replace all occurences in the source code
rm -f cscope.files cscope.out
make cscope < /dev/null
quilt new target-printf-spec-use.patch
files=`cat cscope.files`
quilt add $files
for f in $files; do
	if [ "$f" != configure.c -a "${f#va}" == "$f" -a "${f#qemu}" == "$f" -a "$f" != kvmdump.h  -a "$f" != gdb_interface.c ]; then
		echo Processing $f
		"$scriptdir"/printf-spec.pl "$f" > "$f".new && mv "$f".new "$f"
	fi
done
quilt refresh -p ab --no-timestamp

################################################################
# Build-time changes

# Allow configuring for any target
quilt import "$scriptdir"/configure-any-target.patch
quilt push
