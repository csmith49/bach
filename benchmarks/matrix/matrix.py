def __mkTensor(l):
    from sympy import Matrix
    (x1,x2,y1,y2) = l
    return Matrix([[x1,x2],[y1,y2]])

def __getList(t):
    (x1,x2,y1,y2) = map(lambda x: x, list(t))
    return (x1,x2,y1,y2)

def matrix_mult(t1: "tensor", t2: "tensor"):
    m1 = __mkTensor(t1)
    m2 = __mkTensor(t2)

    res = m1 * m2
    return __getList(res)

def matrix_inv(t1: "tensor"):
    m1 = __mkTensor(t1)
    try:
        res = m1.inv()
        return __getList(res)
    except:
        return "Error"

def matrix_is_symmetric(t1: "tensor"):
    m1 = __mkTensor(t1)
    return m1.is_symmetric()

def matrix_det(t1: "tensor"):
    m1 = __mkTensor(t1)
    return m1.det()

def matrix_transpose(t1: "tensor"):
    m1 = __mkTensor(t1)
    res = m1.T
    return __getList(res)

def matrix_is_lower(t1: "tensor"):
    m1 = __mkTensor(t1)
    res = m1.is_lower
    return res

def matrix_is_upper(t1: "tensor"):
    m1 = __mkTensor(t1)
    res = m1.is_upper
    return res
