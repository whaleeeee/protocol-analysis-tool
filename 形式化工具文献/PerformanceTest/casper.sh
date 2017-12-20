#!/bin/bash
#
# In order to run this script, one should have Casper installed.
#
# One environment variable is important:
#
#  CASPERBASE  should point to the base of the Casper distribution
#
# This script is not thread safe, as it copies the input file to
# test.spl.
#
# Argument should be without .spl to make it easier to name the output
# file.
#
# If the second argument is '-' we just copy the output to stdout
#
if [ "x" == "x$1" ]
then
	echo "Please specify input file name (without .spl extension)";
	exit 1;
fi;

MYDIR="$CASPERBASE/1.11"

INP="$1.spl"
OUT="$1.csp"

TMPINP="$MYDIR/test.spl"
TMPOUT="$MYDIR/test.csp"

cp $INP $TMPINP

CURDIR=`pwd`
cd $MYDIR
runhugs Main.lhs

cd $CURDIR

if [ "x" != "x$2" ]
then
	if [ "x-" == "x$2" ]
	then
		cat $TMPOUT;
	else
		cp $TMPOUT $2;
	fi;
else
	cp $TMPOUT $OUT;
fi;




