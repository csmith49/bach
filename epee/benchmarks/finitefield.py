def ff_add(x : "fp17", y : "fp17"):
    return (x + y) % 17

def ff_mult(x : "fp17", y : "fp17"):
    return (x * y) % 17

def ff_add_inv(x : "fp17"):
    return (0 - x) % 17

def ff_mult_inv(x : "fp17"):
    return pow(x, 15, 17)
