#!/bin/bash
./bach.native -b $1 -interval 1000 2000 -maxdepth 7 -csv | tee ./truth/$1.csv
