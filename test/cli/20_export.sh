#!/bin/sh
#
# Copyright © 2016 Dynamic Object Language Labs Inc.
#
# This software is licensed under the terms of the
# Apache License, Version 2.0 which can be found in
# the file LICENSE at the root of this distribution.

# do NOT set this as the grep pipe will mess it up
# set -e

# demonstrate sending output to STDOUT
# MUST make fix for lein-cljsbuild polluting STDOUT :(

# Acknowledgement and Disclaimer:
# This material is based upon work supported by the Army Contracting
# and DARPA under contract No. W911NF-15-C-0005.
# Any opinions, findings and conclusions or recommendations expressed
# in this material are those of the author(s) and do necessarily reflect the
# views of the Army Contracting Command and DARPA.

# FIXME
# pamela -o - -m four export | grep -v ^Compiling > $RESULTS/four.pamela

# if ! diff -u $CODE/src/test/pamela/four.pamela $RESULTS/four.pamela; then
#     exit 1
# fi

# FIXME
true
