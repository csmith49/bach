#!/bin/bash

FOLDER=./benchmarks/$1/
FACTS=$FOLDER/facts/
GRAPHS=./graphs/

function bach {
    gtimeout 300 ./bach.native -induct ${1} -fact $FACTS -times -sample ${2} | tee $GRAPHS/${3}.csv
}

bach ${FOLDER}small.sexp 100 small
bach ${FOLDER}big_f.sexp 100 big_f
bach ${FOLDER}big_v.sexp 100 big_v
bach ${FOLDER}small.sexp 1000 big_d
bach ${FOLDER}big_fv.sexp 100 big_fv
bach ${FOLDER}big_f.sexp 1000 big_fd
bach ${FOLDER}big_v.sexp 1000 big_vd
bach ${FOLDER}small.sexp 1000 big_fvd
