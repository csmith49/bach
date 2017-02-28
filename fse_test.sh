# constants to help out
BACH=./bach.native
TRUTH_START=2500
TRUTH_DEPTH=6
TARGET=./fse
ITERS="1 2 3 4 5 6 7 8 9 10"
SIZES="450 475"

# how to actually run bach
bach() {
    # expects to be called as bach benchmark size iteration
    echo -n "CHECKING: ${1} w/ fact size ${2}..."
    $BACH -csv -b $1 -interval 0 $TRUTH_START -maxdepth $TRUTH_DEPTH -sample $2 > ${TARGET}/${1}_${2}_${3}.csv
    echo "done."
}

# initialize target folder
mkdir -p $TARGET

# print some stuff
echo "Provided targets: $@"

for iter in $ITERS
do
    echo "--> Beginning iteration ${iter} <---"
    for bm in $@
    do
        for size in $SIZES
        do
            bach $bm $size $iter
        done
    done
done
