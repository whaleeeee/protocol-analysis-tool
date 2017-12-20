#!/bin/bash
#
# In order to run this script, one should have FDR installed.
#
# One environment variables is important:
#
#  FDRHOME     should point to the base of the FDR distribution
#  CASPERBASE  should point to the base of the Casper distribution
#

$FDRHOME/scripts/fdr2 batch $* #2> /dev/null



