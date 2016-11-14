def ff_add(x : "fp17", y : "fp17"):
    return (x + y) % 199

def ff_mult(x : "fp17", y : "fp17"):
    return (x * y) % 199

def ff_add_inv(x : "fp17"):
    return (0 - x) % 199

def ff_mult_inv(x : "fp17"):
    if x == 0:
        return "Error"
    else:
        return pow(x, 197, 199)
