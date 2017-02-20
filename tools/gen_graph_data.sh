#!/bin/bash

FOLDER=./benchmarks/$1/
FACTS=$FOLDER/facts/
GRAPHS=./graphs/
GRAMMAR=${FOLDER}${1}.sexp


function bach {
    gtimeout 300 ./bach.native -induct $GRAMMAR -fact $FACTS -times -sample ${1} | tee $GRAPHS/${2}.csv
}

bach 10 d_10
bach 50 d_50
bach 100 d_100
bach 500 d_500
bach 1000 d_1000
