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
vs = [x,y,n,z,t,w,a,m,p,q,r]

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
set_difference = Function('set_difference', BitVecSort(3), BitVecSort(3), BitVecSort(3))
set_issubset = Function('set_issubset', BitVecSort(3),  BitVecSort(3), BitVecSort(3))
set_issuperset = Function('set_issuperset', BitVecSort(3), BitVecSort(3), BitVecSort(3))

allforms = {}

f = open('specs.out','r')

for l in f:
    print l
    l = l.replace("|","")
    bi = "b"
    if "===" in l:
        zstr = l.split("===")
    elif "<==" in l:
        bi = "r"
        zstr = l.split("<==")
    else:
        bi = "l"
        zstr = l.split("==>")
    

    zstr[0] = zstr[0].replace("=","==")
    zstr[1] = zstr[1].replace("=","==")
    assert(len(zstr) == 2)

    left = "And(" + zstr[0] + ")"
    left = eval(left)
    right = "And(" + zstr[1] + ")"
    right = eval(right)
    
    if bi == "b":
        form = left == right
    elif bi == "l":
        form = Implies(left,right)
    else:
        form = Implies(right,left)

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
    #s.set(auto_config=False, mbqi=False)
    s.set("timeout", 2000)

    f1 = ForAll(vs,f1)
    f2 = ForAll(vs,f2)
    a = And(f1,Not(f2))

    s.add(a)
    print "calling z3"
    if s.check() == unsat:
        print "tru done caalling z3"
        return True
    
    s.reset()
    s.set("timeout", 2000)
    s.add(Not(f2))
    print "calling z3--checking taautology"
    if s.check() == unsat:
        print "tru done caalling z3"
        return True
   
    print "done caalling z3"
    return False


unique = {}
for l in allforms:
    there = False
    for l2 in unique:
        #print "checking", l
        #print "  and", u


        #print "z3 :checking", allforms[l]
        #print "z3 :   and", unique[u]
        if impl(unique[l2], allforms[l]):
            #print "true"
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
