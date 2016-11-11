from z3 import *

x = Int('x')
y = Int('y')
z = Int('z')
w = Int('w')
a = Int('a')
vs = [x,y,z,w,a]

valid = Function('valid', IntSort(),  IntSort())
sat = Function('sat', IntSort(), IntSort())
f_neg = Function('f_neg', IntSort(), IntSort())
f_and = Function('and', IntSort(), IntSort(), IntSort())
f_or = Function('or', IntSort(), IntSort(), IntSort())
ff_mult_inv = Function('f_mult_inv', IntSort(),  IntSort())
ff_add_inv = Function('f_add_inv', IntSort(),  IntSort())
ff_mult = Function('f_mult', IntSort(), IntSort(),  IntSort())
ff_add = Function('f_add', IntSort(), IntSort(),  IntSort())
hd = Function('hd', IntSort(),  IntSort())
concat = Function('concat', IntSort(), IntSort(), IntSort())
cons = Function('cons', IntSort(), IntSort(), IntSort())


f = open('specs.out','r')
allforms = {}


for l in f:
    print l
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
    s.set(auto_config=False, mbqi=False)
    s.set("timeout", 2000)

    f1 = ForAll(vs,f1)
    f2 = ForAll(vs,f2)
    a = And(f1,Not(f2))

    s.add(a)
    print "calling z3"
    if s.check() == unsat:
        print "tru done caalling z3"
        return True
    
    print "done caalling z3"
    return False


unique = {}
for l in allforms:
    there = False
    for u in unique:
        #print "checking", l
        #print "  and", u


        #print "z3 :checking", allforms[l]
        #print "z3 :   and", unique[u]
        if impl(unique[u], allforms[l]):
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
#    b = Bool('b'+str(i))
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
