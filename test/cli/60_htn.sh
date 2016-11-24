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

# demonstrate processing a HTN

roottask="flip-3"
format="edn"
#NOTE sed commands below use : instead of " for edn format

pamela -i "$CODE/test/pamela/biased-coin.pamela" \
       -t "(coin.$roottask)" \
       -f $format \
       -o "$RESULTS/${NUMBER}_$roottask" \
       htn

# remove gensym artifacts
# NOTE: maps should be sorted by key

# HTN check ---
sed -e 's/\(:[a-z\-]*\)-[0-9][0-9]*/\1-1000/g' \
    "$dir/${NUMBER}_$roottask.htn.$format" > \
    "$RESULTS/${NUMBER}_EXPECTED_$roottask.htn.safe.$format"

sed -e 's/\(:[a-z\-]*\)-[0-9][0-9]*/\1-1000/g' \
    "$RESULTS/${NUMBER}_$roottask.htn.$format" > \
    "$RESULTS/${NUMBER}_$roottask.htn.safe.$format"

if ! diff -u "$RESULTS/${NUMBER}_EXPECTED_$roottask.htn.safe.$format" \
     "$RESULTS/${NUMBER}_$roottask.htn.safe.$format"; then
    exit 1
fi

# TPN check ---
sed -e 's/\(:[a-z\-]*\)-[0-9][0-9]*/\1-1000/g' \
    "$dir/${NUMBER}_$roottask.tpn.$format" > \
    "$RESULTS/${NUMBER}_EXPECTED_$roottask.tpn.safe.$format"

sed -e 's/\(:[a-z\-]*\)-[0-9][0-9]*/\1-1000/g' \
    "$RESULTS/${NUMBER}_$roottask.tpn.$format" > \
    "$RESULTS/${NUMBER}_$roottask.tpn.safe.$format"

if ! diff -u "$RESULTS/${NUMBER}_EXPECTED_$roottask.tpn.safe.$format" \
     "$RESULTS/${NUMBER}_$roottask.tpn.safe.$format"; then
    exit 1
fi
