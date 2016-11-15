from subprocess import STDOUT, check_output, TimeoutExpired
import argparse
import sys
import csv
from itertools import combinations


# ----------------------------------------
# utility functions
# ----------------------------------------

def from_sexp(string):
    sexp = [[]]
    word = ''
    in_str = False
    for char in string:
        if char == '(' and not in_str:
            sexp.append([])
        elif char == ')' and not in_str:
            if word:
                sexp[-1].append(word)
                word = ''
            temp = sexp.pop()
            sexp[-1].append(temp)
        elif char in (' ', '\n', '\t') and not in_str:
            if word:
                sexp[-1].append(word)
                word = ''
        elif char == '\"':
            in_str = not in_str
        else:
            word += char
    return sexp[0][0]

def to_sexp(li):
    if not isinstance(li, list):
        return str(li)
    else:
        return "({})".format(" ".join(map(to_sexp, li)))

def do_it(cmd, seconds):
    try:
         output = check_output(cmd, stderr=STDOUT, timeout=seconds, shell=True)
         return output.decode(sys.stdout.encoding)
    except TimeoutExpired as e:
        return e.output.decode(sys.stdout.encoding)
# ----------------------------------------
# wrapper for config files
# ----------------------------------------

class ConfigFile(object):
    def __init__(self, s):
        self._rep = from_sexp(s)
    def __repr__(self):
        return to_sexp(self._rep)
    def signature(self):
        return list(filter(lambda x: x[0] == "signature", self._rep))[0][1:][0]
    def update_signature(self, s):
        f = lambda x: ["signature"] + [s] if x[0] == "signature" else x
        self._rep = list(map(f, self._rep))

def split_signature(sexpr, k=2):
    config = ConfigFile(sexpr)
    sig = config.signature()
    if len(sig) > k:
        for combo in combinations(sig, k):
            config.update_signature(list(combo))
            yield repr(config)
    else:
        yield repr(config)

# ----------------------------------------
# and a wrapper for the results
# ----------------------------------------
class Results(object):
    def __init__(self, line):
        self._line = line
        items = line.split("\t")
        self.formula = items[0]
        self.positive_evidence = int(items[1])
        self.size = int(items[2])
        self.num_vars = int(items[3])
        self.num_holes = int(items[4])
        self.abducible_size = int(items[5])
    def score(self):
        return self.positive_evidence / self.size
    def __repr__(self):
        ret = [self.formula, self.positive_evidence, self.size, self.num_vars, self.num_holes, self.abducible_size]
        ret = map(lambda x: str(x), ret)
        return '\t'.join(ret)

def gather_results(output):
    out = []
    lines = list(set(output.split("\n")))
    for line in lines:
        try:
            out.append(Results(line))
        except:
            pass
    return out

# ----------------------------------------
# now we do the fun stuff
# ----------------------------------------
CMD = "./bach.native -induct /tmp/tmp{depth}.sexp -fact {fact_dir} -mindepth {depth} -csv"

def bach(config, fact_dir, depth, time, abduce):
    cmd = CMD.format(fact_dir=fact_dir, depth=depth)
    if abduce:
        cmd += " -abduce"
    with open(config) as f:
        signature = f.read()
    result_strings = []
    for sexp in split_signature(signature):
        with open("/tmp/tmp"+str(depth)+".sexp", "w") as f:
            f.write(sexp)
        result_strings.append(do_it(cmd, time))
    return gather_results("\n".join(result_strings))

# ----------------------------------------
# and make it all work
# ----------------------------------------

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Don't feed them after midnight.")
    parser.add_argument("grammar")
    parser.add_argument("facts")
    parser.add_argument("-t", "--timeout", default=1000)
    parser.add_argument("-d", "--depth", default=0)
    parser.add_argument("-a", "--abduce", default=False)

    args = parser.parse_args()

    results = bach(args.grammar, args.facts, args.depth, float(args.timeout), args.abduce)

    for result in sorted(results, key=lambda r: r.score(), reverse=True):
        print(repr(result))
