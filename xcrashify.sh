#! /bin/sh

# run this script in crash top-level directory to add the
# cross-platform capability

# assume that scripts are accessible the same way as this script
scriptdir=`dirname "$0"`

# Add readlong, readint, etc. functions
quilt import "$scriptdir"/readtype.patch
quilt push

# Re-generate cscope.files and cscope.out
rm -f cscope.files cscope.out
make cscope < /dev/null

# Find and convert calls to readmem
quilt new replace-readmem.patch
quilt add *.c *.h
"$scriptdir"/replace_readmem.pl
quilt refresh -p ab --no-timestamp

# Introduce target types
quilt import "$scriptdir"/target-types.patch
quilt push

# Replace
quilt new target-types-use.patch
quilt add *.c *.h
for f in `cat cscope.files`; do
	if [ "$f" != configure.c ]; then
		"$scriptdir"/target-types.pl "$f" > "$f".new && mv "$f".new "$f"
	fi
done
quilt refresh -p ab --no-timestamp
