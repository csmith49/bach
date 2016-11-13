#!/bin/bash

python3 ./tools/check_error.py -g ./truth/$1.csv -s 10 -i 0 1000 -b $1 -d 7 -r 1
python3 ./tools/check_error.py -g ./truth/$1.csv -s 50 -i 0 1000 -b $1 -d 7 -r 1
python3 ./tools/check_error.py -g ./truth/$1.csv -s 100 -i 0 1000 -b $1 -d 7 -r 1
python3 ./tools/check_error.py -g ./truth/$1.csv -s 500 -i 0 1000 -b $1 -d 7 -r 1
