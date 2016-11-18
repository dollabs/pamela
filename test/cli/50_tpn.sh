#!/bin/sh
#
# Copyright © 2016 Dynamic Object Language Labs Inc.
#
# This software is licensed under the terms of the
# Apache License, Version 2.0 which can be found in
# the file LICENSE at the root of this distribution.

# Acknowledgement and Disclaimer:
# This material is based upon work supported by the Army Contracting
# and DARPA under contract No. W911NF-15-C-0005.
# Any opinions, findings and conclusions or recommendations expressed
# in this material are those of the author(s) and do necessarily reflect the
# views of the Army Contracting Command and DARPA.

set -e

# demonstrate processing a TPN
# FIXME
# pamela -i $CODE/src/test/pamela/plant.pamela -i $CODE/src/test/pamela/parallel-choice-tpn.pamela -f json -o $RESULTS/parallel-choice-tpn.json tpn

# NOTE Naive graph comparison does not work! :)

# remove gensym artifacts, remove blank lines, sort
# sed -e 's/\-[0-9]*//g' -e 's/}/}\n/g' $RESULTS/parallel-choice-tpn.json | sort > $RESULTS/parallel-choice-tpn.safe.json

# if ! diff -u $CODE/src/test/cli/parallel-choice-tpn.safe.json $RESULTS/parallel-choice-tpn.safe.json; then
#     exit 1
# fi

# FIXME
true
