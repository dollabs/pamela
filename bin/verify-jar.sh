#!/bin/sh
# verify-lein-prod.sh
#
# Copyright © 2016 Dynamic Object Language Labs Inc.
#
# This software is licensed under the terms of the
# Apache License, Version 2.0 which can be found in
# the file LICENSE at the root of this distribution.
#
# This script

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
cd "$code"
# top="$(pwd -P)"
top="."
target="$top/target"
jar="$target/uberjar/pamela.jar"
outfile="$target/uberjar/verify-lein-prod.out"

err() {
  # as log only echos we do not need to repeat this here
  # log "${program}: $*"
  echo >&2 "${program}: $*"
}


echo "build uberjar..."

lein prod

if [ ! -e "$jar" ]; then
    err "uberjar not built: $jar"
    exit 1
fi

echo "test uberjar..."

export PAMELA_VERBOSE=2
export PAMELA_MODE=prod
pamela_version="$(awk -F\" '/defproject/ { print $2;}' project.clj)"
$top/bin/pamela -V > "$outfile"

if head -1 $outfile | grep target/uberjar/pamela.jar > /dev/null; then
    echo "test used uberjar..."
else
    err "test did not use uberjar..."
    exit 1
fi

tested_version="$(tail -1 $outfile)"

if [ "$tested_version" = "$pamela_version" ]; then
    echo "version strings match"
else
    err "version string should be $pamela_version but was set to $tested_version"
    exit 1
fi
