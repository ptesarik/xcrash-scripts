#! /bin/sh

# run this script in crash top-level directory to add the
# cross-platform capability

# assume that scripts are accessible the same way as this script
scriptdir=`dirname "$0"`

# Add readlong, readint, etc. functions
quilt import "$scriptdir"/readtype.patch
quilt push

# Find and convert calls to readmem
quilt new replace-readmem.patch
quilt add *.c *.h
"$scriptdir"/replace_readmem.pl
quilt refresh -p ab --no-timestamp
