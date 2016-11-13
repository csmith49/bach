from subprocess import STDOUT, check_output, TimeoutExpired
import argparse
import sys
import csv

def load_gt(filename):
    with open(filename) as f:
        return set(line.split("\t")[0] for line in f.readlines())

def do_it(cmd, seconds):
    try:
         output = check_output(cmd, stderr=STDOUT, timeout=seconds, shell=True)
         return output.decode(sys.stdout.encoding)
    except TimeoutExpired as e:
        return e.output.decode(sys.stdout.encoding)

def grab_results(results):
    out = []
    for line in results.split("\n"):
        if line:
            try:
                out.append(line.split("\t")[0])
            except:
                pass
        else:
            pass
    return set(out)

def error_analysis(observations, ground):
    # compute t1 error (false positives) and t2 error (true negatives)
    fp_count = len(observations - ground)
    tn_count = len(ground - observations)
    return fp_count, tn_count, len(observations)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="When this baby gets up to 88MPH...")
    parser.add_argument('-t', '--timeout', type=int, default=1000)
    parser.add_argument('-g', '--ground', required=True)
    parser.add_argument('-s', '--size', type=int, required=True)
    parser.add_argument('-i', '--interval', type=int, nargs=2, required=True)
    parser.add_argument('-r', '--repeat', type=int, default=10)
    parser.add_argument('-d', '--depth', type=int, default=10)
    parser.add_argument('-b', '--benchmark', required=True)

    args = parser.parse_args()

    cmd = "./bach.native -csv -b {benchmark} -interval {interval[0]} {interval[1]} -maxdepth {depth} -sample {size}"

    ground = load_gt(args.ground)

    for i in range(args.repeat):
        data = do_it(cmd.format(**args.__dict__), args.timeout)
        results = grab_results(data)
        print(error_analysis(results, ground))
