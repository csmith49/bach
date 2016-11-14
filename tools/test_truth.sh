#!/bin/bash

echo "10 -------------"
python3 ./tools/check_error.py -g ./truth/$1.csv -s 10 -i 0 1000 -b $1 -d 7 -r 5
echo "50 -------------"
python3 ./tools/check_error.py -g ./truth/$1.csv -s 50 -i 0 1000 -b $1 -d 7 -r 5
echo "100 -------------"
python3 ./tools/check_error.py -g ./truth/$1.csv -s 100 -i 0 1000 -b $1 -d 7 -r 5
echo "500 -------------"
python3 ./tools/check_error.py -g ./truth/$1.csv -s 500 -i 0 1000 -b $1 -d 7 -r 5
