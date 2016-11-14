# helper function
def createPoly(x,y,size):
    from sympy import geometry
    a = (x,y)
    b = (x + size, y)
    c = (x + size, y + size)
    d = (x, y + size)
    return geometry.Polygon(a,b,c,d)

def canonical(poly):
    (xmin, ymin, xmax, ymax) = poly.bounds
    return (xmin,ymin,xmax-xmin)

def geometry_encloses_point(r : "rect", p: "point"):
    x,y,size = r
    poly = createPoly(x,y,size)
    return poly.encloses_point(p)

def geometry_encloses(r1 : "rect", r2: "rect"):
    x,y,size = r1
    poly1 = createPoly(x,y,size)

    x,y,size = r2
    poly2 = createPoly(x,y,size)

    return poly1.encloses(poly2)

def geometry_rotate(r : "rect", angle: "rad"):
    from sympy import pi
    x,y,size = r
    poly = createPoly(x,y,size)

    return canonical(poly.rotate(angle*pi/2))

def geometry_scale(r : "rect", scale: "posint"):
    from sympy import pi
    x,y,size = r
    poly = createPoly(x,y,size)

    return canonical(poly.scale(scale,scale))

def geometry_translate(r : "rect", trans: "int"):
    from sympy import pi
    x,y,size = r
    poly = createPoly(x,y,size)

    return canonical(poly.translate(trans,trans))


def geometry_area(r: "rect"):
    x,y,size = r
    poly1 = createPoly(x,y,size)

    return poly1.area
