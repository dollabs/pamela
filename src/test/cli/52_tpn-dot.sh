#!/bin/sh
#
# Copyright © 2016 Dynamic Object Language Labs Inc.
#
# This software is licensed under the terms of the
# Apache License, Version 2.0 which can be found in
# the file LICENSE at the root of this distribution.

set -e

# demonstrate processing a TPN for Graphviz
pamela -i $CODE/src/test/pamela/tpn-demo.pamela -f dot -o $RESULTS/tpn-demo.dot tpn

# NOTE: even this is too fragile as sort on Mac OS is different
# than Linux
# remove gensym artifacts, sort
# sed -e 's/node[0-9]*/node/g' $RESULTS/tpn-demo.dot | sort > $RESULTS/tpn-demo.safe.dot

# if ! diff -u $CODE/src/test/cli/tpn-demo.safe.dot $RESULTS/tpn-demo.safe.dot; then
#     exit 1
# fi

if [ ! -f $RESULTS/tpn-demo.dot ]; then
    echo "Outfile file not found: $RESULTS/tpn-demo.dot"
    exit 1
fi
