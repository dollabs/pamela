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

# demonstrate relative paths
cd "$RESULTS"
cp "$CODE/src/test/pamela/choice-tpn.pamela" ./
outfile="choice-tpn.json"
rm -f "$CODE/$outfile" "$outfile"

echo "Running in $(pwd -P)"
echo " "

pamela -v -v -i ../../src/test/pamela/plant.pamela -i choice-tpn.pamela -f json -o $outfile tpn

# DEBUG
if [ -e "$CODE/logs/pamela-errors.log" ]; then
    echo " "
    echo "-- pamela-errors.log --"
    cat "$CODE/logs/pamela-errors.log"
fi
if [ -e "$CODE/logs/pamela-service.log" ]; then
    echo " "
    echo "-- tail -15 pamela-service.log --"
    tail -15 "$CODE/logs/pamela-service.log"
fi

sleep 1
if ! [ -e $outfile ] || ! [ -s $outfile ]; then
    echo "error: The $outfile file was not created"
    exit 1
fi
