import argparse
import csv
import matplotlib.pyplot as plt

class TimeLine(object):
    def __init__(self, filename):
        # go ahead and read all the data from filename
        self._rows = []
        with open(filename, newline='') as f:
            reader = csv.reader(f, delimiter='\t')
            for row in reader:
                self._rows.append(row)
        # peel off and convert the relevent info
        self.time = [float(r[1]) for r in self._rows]
        self.count = [int(r[2]) for r in self._rows]
        # and get a name
        self.name = filename.split(".")[0]
    def plot(self, **kwargs):
        plt.plot(self.time, self.count, label=self.name, **kwargs)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="We make graphs.")
    parser.add_argument("inputs", nargs="+")

    args = parser.parse_args()

    for filename in args.inputs:
        line = TimeLine(filename)
        line.plot()

    plt.xlabel("time (seconds)")
    plt.xlim(0, 300)
    plt.ylabel("formulas visited")
    plt.legend()

    plt.show()