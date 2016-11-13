from subprocess import STDOUT, check_output, TimeoutExpired
import argparse
import sys
import csv

def do_it(cmd, seconds):
    try:
         output = check_output(cmd, stderr=STDOUT, timeout=seconds, shell=True)
         return output.decode(sys.stdout.encoding)
    except TimeoutExpired as e:
        return e.output.decode(sys.stdout.encoding)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Helper for timing Bach.")
    parser.add_argument('args', metavar='xxx', nargs='*')
    parser.add_argument('-t', '--timeout', type=int, default=240)
    parser.add_argument('-o', '--output', default='out.csv')

    args = parser.parse_args()

    cmd = ["./bach.native", "-times"] + args.args

    results = do_it(" ".join(cmd) , args.timeout)

    with open(args.output, 'w', newline='') as f:
        writer = csv.writer(f)
        for line in results.split("\n"):
            if line.startswith("TIME"):
                l = line.split("\t")
                writer.writerow([float(l[1]), int(l[2])])
