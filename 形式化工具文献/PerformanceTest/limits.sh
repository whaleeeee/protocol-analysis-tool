#!/bin/sh
#
# For a 1GB RAM system:
#
# No core files by default
ulimit -c 0 > /dev/null 2>&1
# 1GB virtual memory limit
ulimit -v 1048576 > /dev/null 2>&1
# 512MB resident memory limit
ulimit -m 524288 > /dev/null 2>&1
# 128 max processes per user
ulimit -u 128 >/dev/null 2>&1

