#!/usr/bin/env bash

# Assumes pamela is in path

 echo "Compiling for Choice"
 set -x
 pamela -i over-arching-constraints.pamela -o over-arching-constraints-choice.tpn.json -c main:tpn:choic tpn
 pamela -i over-arching-constraints.pamela -o over-arching-constraints-choice-htn -t "(main.main-choice)" htn
 set +x

 echo
 echo "Compiling for Parallel"
 set -x
 pamela -i over-arching-constraints.pamela -o over-arching-constraints-parallel.tpn.json -c main:tpn:paralle tpn
 pamela -i over-arching-constraints.pamela -o over-arching-constraints-parallel-htn -t "(main.main-parallel)" htn
 set +x

 echo
 echo "Compiling for Sequence"
 set -x
 pamela -i over-arching-constraints.pamela -o over-arching-constraints-sequence.tpn.json -c main:tpn:sequenc tpn
 pamela -i over-arching-constraints.pamela -o over-arching-constraints-sequence-htn -t "(main.main-sequence)" htn
 echo "Done compile"
 echo

set +x