import argparse
from itertools import combinations

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

class ConfigFile(object):
    def __init__(self, s):
        self._rep = from_sexp(s)
    def __repr__(self):
        return to_sexp(self._rep)
    def signature(self):
        return list(filter(lambda x: x[0] == "signature", self._rep))[0][1:][0]
    def update_signature(self, s):
        f = lambda x: ["signature"] + s if x[0] == "signature" else x
        self._rep = list(map(f, self._rep))

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Don't feed them after midnight.")
    parser.add_argument("-o", "--output", default="./config")
    parser.add_argument("input")

    args = parser.parse_args()

    with open(args.input) as f:
        config = ConfigFile(f.read())

    sig = config.signature()

    if len(sig) > 2:
        for i, (x, y) in enumerate(combinations(sig, 2)):
            with open("{}{}.sexp".format(args.output, i) , "w") as f:
                config.update_signature([x, y])
                f.write(repr(config))
