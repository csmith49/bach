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

# GENERATORS

# for dict
@add_generator('dict')
def dict_gen():
    from random import randint
    while True:
        size = randint(0, 3)
        d = {}
        for i in range(1, size + 1):
            key = randint(0, 2)
            value = randint(0, 1)
            d[key] = value
        yield d

# for finitefield (actually does fp_199)
@add_generator('fp17')
def fp_gen():
    from random import randint
    while True:
        yield randint(0, 198)

# for geometry
@add_generator('point')
def point_gen():
    from random import randint
    while True:
        x = randint(-1, 1)
        y = x
        yield (x, y)

@add_generator('rect')
def rect_gen():
    from random import randint
    while True:
        x = randint(-1, 1)
        y = x
        size = randint(1, 6)
        yield (x, y, size)

@add_generator('int')
def int_gen():
    from numpy import random
    while True:
        a = random.normal(0, 2, 1)
        yield int(a[0])

@add_generator('rad')
def rad_gen():
    from numpy import random
    while True:
        a = random.normal(0, 2, 1)
        yield int(a[0])

@add_generator('posint')
def posint_gen():
    from random import randint
    while True:
        a = randint(1, 3)
        yield a

# for list
@add_generator('cons_int')
def consint_gen():
    from random import randint
    while True:
        yield randint(0, 2)

@add_generator('list')
def list_gen():
    from random import randint
    while True:
        size = randint(0, 3)
        yield [randint(0, 2) for r in range(size)]

# for matrix
@add_generator('tensor')
def tensor_gen():
    from random import randint
    while True:
        x1 = randint(-1, 2)
        y1 = randint(-1, 1)
        x2 = randint(-1, 1)
        y2 = randint(-1, 1)
        yield (x1, x2, y1, y2)

# for queue
@add_generator('queue')
def queue_gen():
    from random import randint
    while True:
        size = randint(0, 3)
        s = []
        for i in range(1, size + 1):
            value = randint(0, 1)
            s.append(value)
        yield s

# for sets
@add_generator('set')
def set_gen():
    from random import randint
    from numpy import random
    while True:
        size = abs(int(random.normal(0, 3, 1)[0]))
        s = set([])
        for i in range(1, size + 1):
            value = randint(0, 2)
            s.add(value)
        yield s

# for strings
@add_generator('string')
def string_gen():
    from random import randint
    while True:
        size = randint(0,4)
        s = ""
        for i in range(1, size + 1):
            x = randint(0, 2)
            if x == 0:
                s += "1"
            if x == 1:
                s += "a"
            if x == 2:
                s += "A"
        if s == "": s = "@"
        yield s

# for trig
@add_generator('arc')
def arc_gen():
    from random import randint
    while True:
        yield randint(-10, 10)

@add_generator('radian')
def radian_gen():
    from random import randint
    while True:
        yield randint(-10, 10)


# if called as a standalone, execute as expected
if __name__ == "__main__":
    main()
