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

label="flip-3"
roottask="(main.$label)"
format="edn"
#NOTE sed commands below use : instead of " for edn format

pamela -i "$CODE/test/pamela/biased-coin.pamela" \
       -t "$roottask" \
       -f $format \
       -o "$RESULTS/${NUMBER}_$label" \
       htn

if ! diff -u "$dir/${NUMBER}_$label.htn.$format" \
     "$RESULTS/${NUMBER}_$label.htn.$format"; then
    echo "HTN output differs: target/cli-test/60_flip-3.htn.edn"
    exit 1
fi

if ! diff -u "$dir/${NUMBER}_$label.tpn.$format" \
     "$RESULTS/${NUMBER}_$label.tpn.$format"; then
    echo "TPN output differs: target/cli-test/60_flip-3.tpn.edn"
    exit 1
fi

exit 0
