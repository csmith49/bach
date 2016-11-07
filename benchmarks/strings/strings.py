def concat(x : "string", y : "string"):
    return x + y

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
    return x.lstrip()

def right_strip(x : "string"):
    return x.rstrip()

def all_strip(x : "string"):
    return x.strip()
