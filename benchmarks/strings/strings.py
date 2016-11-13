def concat(x : "string", y : "string"):
    res = x + y
    res.replace("@","")
    if res == "": return "@"
    return res

def reverse(x : "string"):
    return ''.join(reversed(x))

def is_prefix(p : "string", x : "string"):
    return x.startswith(p)

def length(x : "string"):
    return len(x)

def to_caps(x : "string"):
    return x.upper()

def to_lower(x : "string"):
    return x.lower()

def left_strip(x : "string"):
    xn = x.replace("1"," ")
    res = xn.lstrip()
    res = res.replace(" ","1")
    if res == "": return "@"
    return res

def right_strip(x : "string"):
    xn = x.replace("1"," ")
    res = xn.rstrip()
    res = res.replace(" ","1")
    if res == "": return "@"
    return res


def all_strip(x : "string"):
    x = x.replace("@","")
    xn = x.replace("1"," ")
    res = xn.strip()
    res = res.replace(" ","1")
    if res == "": return "@"
    return res

