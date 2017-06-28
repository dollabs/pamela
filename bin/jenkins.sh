#!/bin/bash
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

JAVA_VER=`java -version 2>&1 | grep 'version' | cut -d ' ' -f 3`

echo "Java version $JAVA_VER"
if [[ ^$JAVA_VER =~ "1.7" ]];
then
    echo "We have java 7"
    # https://github.com/boot-clj/boot/wiki/JVM-Options
    export BOOT_JVM_OPTIONS="-Xmx2g -client -XX:+TieredCompilation -XX:TieredStopAtLevel=1 -XX:MaxPermSize=128m -XX:+UseConcMarkSweepGC -XX:+CMSClassUnloadingEnabled -Xverify:none -Dhttps.protocols=TLSv1.2"
    echo "boot jvm options: $BOOT_JVM_OPTIONS"
fi

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
