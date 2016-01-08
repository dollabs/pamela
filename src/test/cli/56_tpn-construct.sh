#!/bin/sh
#
# Copyright © 2016 Dynamic Object Language Labs Inc.
#
# This software is licensed under the terms of the
# Apache License, Version 2.0 which can be found in
# the file LICENSE at the root of this distribution.

set -e

pamela -v -v -i $CODE/src/test/pamela/qc.pamela -i $CODE/src/test/pamela/qc-waypoints.pamela -i $CODE/src/test/pamela/qc-demo.pamela -o $RESULTS/qc.dot -c qc-demo:qc-waypoints:waypoints -f dot -g tpn
