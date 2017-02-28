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
    echo -n "TRUTHING: ${1}..."
    $BACH -csv -b $1 -interval 1000 2000 -maxdepth $TRUTH_DEPTH > ${TARGET}/${1}_truth.csv
    echo "done."
}

count() {
    echo -n "Counting: ${1}..."
    $BACH -b $1 -interval $TRUTH_START 5000 -maxdepth $TRUTH_DEPTH -noisy | grep -w "Checking:" | wc -l
}

# initialize target folder
mkdir -p $TARGET

# print some stuff
echo "Provided targets: $@"

for bm in $@
do
    bach $bm
done
