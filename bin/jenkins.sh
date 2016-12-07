#!/bin/sh
# jenkins.sh
#
# Copyright © 2016 Dynamic Object Language Labs Inc.
#
# This software is licensed under the terms of the
# Apache License, Version 2.0 which can be found in
# the file LICENSE at the root of this distribution.

# NOTE this script will exit on the first failure

# Acknowledgement and Disclaimer:
# This material is based upon work supported by the Army Contracting
# and DARPA under contract No. W911NF-15-C-0005.
# Any opinions, findings and conclusions or recommendations expressed
# in this material are those of the author(s) and do necessarily reflect the
# views of the Army Contracting Command and DARPA.

set -e

program=$(basename $0)

code=$(dirname $0)
cd "$code/.."
code="$(pwd -P)"
cd ".."
top="$(pwd -P)"
echo "Startup Dir: $top"
export PATH=${PATH}:/bin:$code/bin

if [ "$USER" = "jenkins" ] && [ "$(hostname)" = "LispMachine" ]; then
    echo "Running on LispMachine"
    # do NOT encourage ANSI colorization
    export TERM=dumb
fi

cd "$code"
echo " "
echo "-- $program running in $code at $(date) --"

echo " "
echo "-- environment --"

env | sort

echo " "
echo "-- pamela dependencies --"

boot show --deps

echo " "
echo "-- clean -- "

rm -rf target

# echo " "
# echo "-- update documentation -- "

# TBD
# boot codox

echo " "
echo "-- run clojure and command line tests -- "

boot all-tests
