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

# Demonstrate building the IR

pamela -i "$CODE/test/pamela/circuit.pamela" \
       -f edn \
       -o "$RESULTS/${NUMBER}_circuit.ir.edn" \
       build

if ! diff -u "$dir/${NUMBER}_circuit.ir.edn" \
     "$RESULTS/${NUMBER}_circuit.ir.edn"; then
    exit 1
fi
