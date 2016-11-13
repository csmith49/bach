
def dict_haskey(d: "dict", x: "int"):
    return x in d

def dict_update(d: "dict", x: "int", v: "int"):
    d2 = d.copy()
    d2[x] = v
    return d2

def dict_get(d: "dict", x: "int"):
    return d.get(x,"Error")

def dict_clear(d: "dict"):
    d2 = d.copy()
    d2.clear()
    return d2

def dict_len(d: "dict"):
    return len(d)
