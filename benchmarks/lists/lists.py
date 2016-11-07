def hd(x : "list"):
    return x[0]

def tl(x : "list"):
    return x[1:]

def sort(x : "list"):
    return list(sorted(x))

def rev(x : "list"):
    return list(reversed(x))

def cons(x : "cons_int", xs : "list"):
    return [x] + xs

def concat(x : "list", y : "list"):
    return x + y

def length(x : "list"):
    return len(x)
