from argparse import ArgumentParser
from inspect import signature
from importlib import machinery
from csv import writer
import os

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
                        print(e)
                    output = args.error
                csvwriter.writerow(inputs + [output])

# sample generators go here
@add_generator('int')
def int_gen():
    from random import randint
    while True:
        yield randint(-32768, 32767)

@add_generator('fp17')
def fp_gen():
    from random import randint
    while True:
        yield randint(0, 16)

@add_generator('denom')
def denom_gen():
    from random import randint
    while True:
        yield randint(-1, 5)

if __name__ == "__main__":
    main()