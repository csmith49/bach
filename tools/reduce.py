from z3 import *

d = FiniteDomainSort('D', 2)

x = Const(1, BitVecSort(3))
y = Const(2, BitVecSort(3))
z = Const(3, BitVecSort(3))
w = Const(4, BitVecSort(3))
a = Const(5, BitVecSort(3))
m = Const(6, BitVecSort(3))
n = Const(7, BitVecSort(3))
t = Const(8, BitVecSort(3))
p = Const(9, BitVecSort(3))
q = Const(10, BitVecSort(3))
r = Const(11, BitVecSort(3))
b = Const(12, BitVecSort(3))
vs = [x,y,n,z,t,w,a,b,m,p,q,r]

valid = Function('valid', BitVecSort(3), BitVecSort(3))
sat = Function('sat', BitVecSort(3), BitVecSort(3))
f_neg = Function('f_neg', BitVecSort(3), BitVecSort(3))
f_and = Function('and', BitVecSort(3), BitVecSort(3), BitVecSort(3))
f_or = Function('or', BitVecSort(3), BitVecSort(3), BitVecSort(3))

ff_mult_inv = Function('f_mult_inv', BitVecSort(3), BitVecSort(3))
ff_add_inv = Function('f_add_inv', BitVecSort(3), BitVecSort(3))
ff_mult = Function('f_mult', BitVecSort(3), BitVecSort(3), BitVecSort(3))
ff_add = Function('f_add', BitVecSort(3), BitVecSort(3), BitVecSort(3))

hd = Function('hd', BitVecSort(3), BitVecSort(3))
concat = Function('concat', BitVecSort(3), BitVecSort(3), BitVecSort(3))
cons = Function('cons', BitVecSort(3), BitVecSort(3), BitVecSort(3))

geometry_encloses = Function('geometry_encloses', BitVecSort(3), BitVecSort(3), BitVecSort(3))
geometry_encloses_point = Function('geometry_encloses_point', BitVecSort(3), BitVecSort(3), BitVecSort(3))

trig_sin = Function('trig_sin', BitVecSort(3), BitVecSort(3))
trig_arcsin = Function('trig_arcsin', BitVecSort(3), BitVecSort(3))
trig_cos = Function('trig_cos', BitVecSort(3), BitVecSort(3))
trig_arccos = Function('trig_arccos', BitVecSort(3), BitVecSort(3))

matrix_mult = Function('matrix_mult', BitVecSort(3), BitVecSort(3), BitVecSort(3))
matrix_is_lower = Function('matrix_islower', BitVecSort(3), BitVecSort(3))
matrix_is_upper = Function('matrix_isupper', BitVecSort(3), BitVecSort(3))
matrix_is_symmetric = Function('matrix_isupper', BitVecSort(3), BitVecSort(3))
matrix_inv = Function('matrix_inv',  BitVecSort(3), BitVecSort(3))
matrix_transpose = Function('matrix_transpose',  BitVecSort(3), BitVecSort(3))
matrix_det = Function('matrix_det',  BitVecSort(3), BitVecSort(3))

set_difference = Function('set_difference', BitVecSort(3), BitVecSort(3), BitVecSort(3))
set_issubset = Function('set_issubset', BitVecSort(3),  BitVecSort(3), BitVecSort(3))
set_issuperset = Function('set_issuperset', BitVecSort(3), BitVecSort(3), BitVecSort(3))

left_strip = Function('left_strip', BitVecSort(3), BitVecSort(3))
right_strip = Function('right_strip', BitVecSort(3), BitVecSort(3))
reverse = Function('reverse', BitVecSort(3), BitVecSort(3))
all_strip = Function('all_strip', BitVecSort(3), BitVecSort(3))
length = Function('length', BitVecSort(3), BitVecSort(3))

#predicates
identity = Function('identity', BitVecSort(3), BoolSort())
tr = Function('tr', BitVecSort(3), BoolSort())
fls = Function('fls', BitVecSort(3), BoolSort())
twoPiApart = Function('twoPi', BitVecSort(3), BitVecSort(3), BoolSort())
halfPiApart = Function('halfPi', BitVecSort(3), BitVecSort(3), BoolSort())
invi = Function('invi', BitVecSort(3), BitVecSort(3), BoolSort())
invr = Function('invr', BitVecSort(3), BitVecSort(3), BoolSort())

class AstRefKey:
    """ Wrapper for allowing Z3 ASTs to be stored into Python Hashtables """

    def __init__(self, n):
        self.n = n

    def __hash__(self):
        return self.n.hash()

    def __eq__(self, other):
        return self.n.eq(other.n)

    def __repr__(self):
        return str(self.n)


def askey(n):
    assert isinstance(n, AstRef)
    return AstRefKey(n)


def get_vars_(f):
    r = set()

    def collect(f):
        if is_const(f):
            if f.decl().kind() == Z3_OP_UNINTERPRETED and not askey(f) in r:
                r.add(askey(f))
        else:
            for c in f.children():
                collect(c)
    collect(f)
    return r


def get_vars(f):
    fs = simplify(f)
    return set(map(lambda x: x.n, get_vars_(fs)))


allforms = {}

f = open('specs.out','r')

for l in f:
    l = l.replace("\n","")
    lt = l.split("\t")   
    ln = lt[0].split("|")
    bi = "b"
    if "===" in ln[0]:
        zstr = ln[0].split("===")
    elif "<==" in ln[0]:
        bi = "r"
        zstr = ln[0].split("<==")
    else:
        bi = "l"
        zstr = ln[0].split("==>")
    

    g = ln[1].replace("=","==")
    if g == "\n": g = True
    else: g = eval("And("+g+")")

    zstr[0] = zstr[0].replace("=","==")
    zstr[1] = zstr[1].replace("=","==")
    assert(len(zstr) == 2)

    left = "And(" + zstr[0] + ")"
    left = eval(left)
    right = "And(" + zstr[1] + ")"
    right = eval(right)
    
    if "True" not in zstr[0] and "True" not in zstr[1]:
        if len(get_vars(left).intersection(get_vars(right))) == 0: continue
    
    if bi == "b":
        form = left == right
    elif bi == "l":
        form = Implies(left,right)
    else:
        form = Implies(right,left)

    form = Implies(g,form)
    allforms[l] = form
    
unique = {}

def allunique():
    uf = []
    for l in unique:
        uf = uf + [ForAll(vs,unique[l])]

    return And(*uf)

def conjAll():
    uf = []
    for l in allforms:
        uf = uf + [allforms[l]]
    
    return ForAll(vs,And(*uf))


def impl(f1,f2):
    s = Solver()
    s.set(auto_config=False, mbqi=False)
    s.set("timeout", 2000)

    f1 = ForAll(vs,f1)
    f2 = ForAll(vs,f2)
    a = And(f1,Not(f2))

    s.add(a)
    if s.check() == unsat:
        return True
    
    return False


unique = {}
for l in allforms:
    there = False
    for l2 in unique:


        #print "z3 :checking", allforms[l]
        #print "z3 :   and", unique[u]
        if impl(unique[l2], allforms[l]):
            there = True
            break

    if there: continue

    unique[l] = allforms[l]

for l in  unique: print l

f.close()


#s = Solver()
#s.set(mbqi=False)
#
#s.add(conjAll())
#
#core = []
#coremap = {}
#i = 0
#for l in allforms:
#    b = Const('b'+str(i))
#    i = i +1
#    core.append(b)
#    coremap[l] = b
#    s.add(b == ForAll(vs,allforms[l]))
#    #s.add(Implies(b,Not(b)))
#
#print coremap
#s.check(core)
#unsatcore =  s.unsat_core()
#
#print "unsat core", unsatcore
#
#for l in allforms:
#    b = coremap[l]
#    if b in unsatcore:
#        unique[l] = allforms[l]
#
#for l in unique: print l
#f.close()
