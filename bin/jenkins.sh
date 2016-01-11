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
export PATH=${PATH}:$code/bin

# NOTE we will be testing the client here so we need to have
# firefox available to run in a real X session, a Mac "X" session,
# VNC (or similar) or Xvfb

if [ -z "$DISPLAY" ]; then
    if [ "$USER" = "jenkins" ] && [ "$(hostname)" = "LispMachine" ]; then
        echo "Running on LispMachine"
        # jenkins@LispMachine has VNC running on :1
        export DISPLAY=:1
        # do NOT encourage ANSI colorization
        export TERM=dumb
    else
        echo "ERROR: please start X, VNC, or Xvfb and set DISPLAY"
        exit 1
    fi
fi

cd "$code"
echo " "
echo "-- $program running in $code at $(date) --"

echo " "
echo "-- environment --"

env | sort

echo " "
echo "-- pamela dependencies --"

lein deps :tree

echo " "
echo "-- clean -- "

lein clean

echo " "
echo "-- update documentation -- "

lein doc

echo " "
echo "-- run clojure tests -- "

lein test

echo " "
echo "-- verify lein prod -- "
export PAMELA_MODE=prod
verify-lein-prod.sh

echo " "
echo "-- run cli tests with pamelad -- "

# ensure standalone DB
unset ES_SERVER
# ensure not repl? mode
unset PAGER
export SERVER_HOST=localhost
export SERVER_PORT=9100
export PAMELAD="${SERVER_HOST}:${SERVER_PORT}"
export PAMELA_VERBOSE=0

pamelad -v start

if cli-test; then
    pamelad -v stop
else
    echo "stopping pamelad, exiting with failure"
    pamelad -v stop
    exit 1
fi

unset PAMELAD
