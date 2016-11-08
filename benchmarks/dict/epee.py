from argparse import ArgumentParser
from inspect import signature
from importlib import machinery
from csv import writer
import os
import math
import numpy as np

# store generators in a map, use decorator to tag them
# label them with add_generator anywhere after this section
_GENERATORS = {}

def add_generator(key):
    def deco(f):
        _GENERATORS[key] = f()
        return f
    return deco

def generate(*args):
    return [next(_GENERATORS[arg]) for arg in args]

# machinery for loading annotated functions from a file
class Function(object):
    def __init__(self, f):
        self._function = f
        params = signature(f).parameters
        self.inputs = [params[p].annotation for p in list(params)]
        self.name = self._function.__name__
    def __call__(self, *args):
        return self._function(*args)

def load_functions(path):
    module = machinery.SourceFileLoader('sig', path).load_module()
    functions = []
    for name in filter(lambda s: "__" not in s, dir(module)):
        #if "set_" not in name: continue
        functions.append(Function(getattr(module, name)))
    return functions

# entry point - args is the structure provided by argparse
def main():
    parser = ArgumentParser(prog="epee", description="generating CSV files for Bach")
    parser.add_argument('functions')
    parser.add_argument('-v', '--verbose', action='store_true')
    parser.add_argument('-d', '--delimiter', type=str, default="\t")
    parser.add_argument('-c', '--count', type=int, default=1000)
    parser.add_argument('-o', '--output', default="output")
    parser.add_argument('-e', '--error', default="")
    args = parser.parse_args()

    functions = load_functions(os.path.join(os.getcwd(), args.functions))

    for f in functions:
        out = "{}/{}.facts".format(args.output, f.name)
        filename = os.path.join(os.getcwd(), out)
        os.makedirs(os.path.dirname(filename), exist_ok=True)
        with open(filename, 'w', newline='') as csvfile:
            csvwriter = writer(csvfile, delimiter=args.delimiter)
            for i in range(args.count):
                inputs = generate(*f.inputs)
                try:
                    output = f(*inputs)
                except Exception as e:
                    if args.verbose:
                        print (*inputs)
                        print("epee exception " + str(e))
                    output = args.error
                csvwriter.writerow(inputs + [output])

def norm_int(mean, std):
    x = np.random.normal(mean, std, 1)
    return int(x[0])

# sample generators go here
@add_generator('point')
def int_gen():
    from random import randint
    while True:
        x = norm_int(0, 3)
        y = norm_int(0, 3)
        yield (x,y)

@add_generator('dict')
def fp_gen():
    from random import randint
    while True:
        size = abs(norm_int(0,3))
        d = {}
        for i in range(1,size+1):
            d[randint(0,2)] = randint(0,1)
        yield d

@add_generator('posint')
def fp_gen():
    from random import randint
    while True:
        a = abs(norm_int(0, 1))
        yield a


if __name__ == "__main__":
    main()
