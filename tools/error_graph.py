from __future__ import division
import seaborn as sns
import numpy as np
import argparse

DATA = "./fse"

def load_gt(bm):
    with open("{}/{}_truth.csv".format(DATA, bm), "r") as f:
        return set(line.split("\t")[0] for line in f.readlines())

def load_data(bm, size, iter):
    with open("{}/{}_{}_{}.csv".format(DATA, bm, size, iter), "r") as f:
        data = f.read()
        return grab_results(data)

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

# HOLD THE DATA

class ErrorFrame(object):
    def __init__(self, bm, obs, gt):
        if bm == "sets":
            self._count = 1564
        elif bm == "finitefield":
            self._count = 1262
        else:
            raise Exception
        self._obs = obs
        self._gt = gt
    @property
    def tp(self):
        return len(self._obs.intersection(self._gt))
    @property
    def fp(self):
        return len(self._obs - self._gt)
    @property
    def fn(self):
        return len(self._gt - self._obs)
    @property
    def tn(self):
        return self._count - (self.tp + self.fp + self.fn)

# get a bunch of error frames
def load_sequence(bm, index=1):
    gt = load_gt(bm)
    out = []
    for i in range(25, 525, 25):
        out.append(ErrorFrame(bm, load_data(bm, i, index), gt))
    return out

def convert_sequence(s, f):
    return list(map(f, s))

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("bm")
    args = parser.parse_args()
    bm = args.bm

    sns.set_context("paper", font_scale=1.5)
    sns.plt.gcf().subplots_adjust(bottom=0.15)


    p_series = []
    n_series = []
    for i in range(1, 6):
        s = load_sequence(bm, i)
        p_series.append(convert_sequence(s, lambda e: e.fp / (e.fp + e.tp)))
        n_series.append(convert_sequence(s, lambda e: e.fn / (e.tn + e.fn)))
    # series = zip(*series)

    sns.set_style("white")

    sns.tsplot(p_series, color="b", condition="Positive")
    sns.tsplot(n_series, color="r", condition="Negative")

    sns.despine()
    base_ticks = range(25, 525, 25)
    filtered_ticks = [str(x) if x % 100 == 0 else "" for x in base_ticks]
    ticks = zip(*enumerate(filtered_ticks))
    sns.plt.xticks(*ticks)
    # sns.plt.ylim(-.05, .3)

    sns.plt.ylabel("Error rates")
    sns.plt.xlabel("Observations per function")
    sns.plt.legend()
    sns.plt.savefig("{}.png".format(bm))
