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

dir=$(dirname $0)
set -e

# demonstrate processing a TPN

format="edn"
#NOTE sed commands below use : instead of " for edn format

pamela -i "$CODE/test/pamela/plant.pamela" \
       -i "$CODE/test/pamela/parallel-choice.tpn.pamela" \
       -f $format \
       -o "$RESULTS/${NUMBER}_parallel-choice.tpn.$format" \
       tpn

# remove gensym artifacts
# NOTE: maps should be sorted by key

# sed -e 's/\(:[a-z\-]*\)-[0-9][0-9]*/\1-1000/g' \
#     "$dir/${NUMBER}_parallel-choice.tpn.$format" | sort > \
#     "$RESULTS/${NUMBER}_EXPECTED_parallel-choice.tpn.safe.$format"

# sed -e 's/\(:[a-z\-]*\)-[0-9][0-9]*/\1-1000/g' \
#     "$RESULTS/${NUMBER}_parallel-choice.tpn.$format" | sort > \
#     "$RESULTS/${NUMBER}_parallel-choice.tpn.safe.$format"

# sed -e 's/\(:[a-z\-]*\)-[0-9][0-9]*/\1-1000/g' \
#       "$dir/${NUMBER}_parallel-choice.tpn.$format" | \
#       fgrep :tpn-type | cut -d: -f3 | cut -d, -f1 | sort > \
#       "$RESULTS/${NUMBER}_EXPECTED_parallel-choice.tpn.safe.$format"

sed -e 's/\(:[a-z\-]*\)-[0-9][0-9]*/\1-1000/g' \
      "$RESULTS/${NUMBER}_parallel-choice.tpn.$format" | \
      fgrep :tpn-type | cut -d: -f3 | cut -d, -f1 | sort > \
      "$RESULTS/${NUMBER}_parallel-choice.tpn.safe.$format"

# if ! diff -u "$RESULTS/${NUMBER}_EXPECTED_parallel-choice.tpn.safe.$format" \
#      "$RESULTS/${NUMBER}_parallel-choice.tpn.safe.$format"; then
if ! diff -u "$dir/${NUMBER}_parallel-choice.tpn.safe.$format" \
     "$RESULTS/${NUMBER}_parallel-choice.tpn.safe.$format"; then
    exit 1
fi
