#! /bin/sh

set -e

# run this script in crash top-level directory to add the
# cross-platform capability

# assume that scripts are accessible the same way as this script
scriptdir=`dirname "$0"`

rm -f cscope.files cscope.out
make cscope < /dev/null
files=`"$scriptdir"/order-files.pl cscope.files`

################################################################
# Remove unused #ifdef'd code
# The C parser will fail on some of these

quilt new ifdef-REDHAT.patch
quilt add $files
for f in $files; do
	"$scriptdir"/cleanup-ifdefs.pl REDHAT "$f" > "$f".new
	mv "$f".new "$f"
done
quilt refresh -p ab --no-timestamp

quilt new ifdef-MCLX.patch
quilt add $files
for f in $files; do
	"$scriptdir"/cleanup-ifdefs.pl MCLX "$f" > "$f".new
	mv "$f".new "$f"
done
quilt refresh -p ab --no-timestamp

quilt new remove-if-0.patch
quilt add $files
for f in $files; do
	"$scriptdir"/cleanup-ifdefs.pl -r 0 "$f" > "$f".new
	mv "$f".new "$f"
done
quilt refresh -p ab --no-timestamp

# This is necessary to remove multiple else clauses in kernel.c
quilt new remove-if-MODULES_IN_CWD.patch
quilt add $files
for f in $files; do
	"$scriptdir"/cleanup-ifdefs.pl -r MODULES_IN_CWD "$f" > "$f".new
	mv "$f".new "$f"
done
quilt refresh -p ab --no-timestamp

################################################################
# Run the C parser
"$scriptdir"/xcrashify $files
