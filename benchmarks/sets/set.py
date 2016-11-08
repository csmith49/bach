
def set_contains(s: "set", x: "int"):
    return x in s

def set_add(s: "set", x: "int"):
    s2 = s.copy()
    s2.add(x)
    return s2

def set_discard(s: "set", x: "int"):
    s2 = s.copy()
    s2.discard(x)
    return s2

def set_clear(s: "set"):
    s2 = s.copy()
    s2.clear()
    return s2

def set_difference(s: "set", t: "set"):
    s2 = s.copy()
    t2 = t.copy()
    return s2 - t2

def set_union(s: "set", t: "set"):
    s2 = s.copy()
    t2 = t.copy()
    return s2 | t2

def set_issubset(s: "set", t: "set"):
    return s.issubset(t)

def set_issuperset(s: "set", t: "set"):
    return s.issuperset(t)

def set_length(s: "set"):
    return len(s)
