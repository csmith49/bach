from z3 import *

x = Int('x')
y = Int('y')
z = Int('z')
w = Int('w')
vs = [x,y,z,w]

valid = Function('valid', IntSort(),  IntSort())
sat = Function('sat', IntSort(), IntSort())
neg = Function('neg', IntSort(), IntSort())



f = open('specs.out','r')
allforms = {}

for l in f:
    bi = True
    if "===" in l:
        zstr = l.split("===")
    else:
        zstr = l.split("<==")
        bi = False
    
    zstr[0] = zstr[0].replace("=","==")
    zstr[1] = zstr[1].replace("=","==")
    assert(len(zstr) == 2)

    left = "And(" + zstr[0] + ")"
    left = eval(left)
    right = "And(" + zstr[1] + ")"
    right = eval(right)

    if bi:
        form = left == right
    else:
        form = Implies(right,left)

    allforms[l] = form
 
def impl(f1,f2):
    s = Solver()
    f1 = ForAll(vs,f1)
    f2 = ForAll(vs,f2)
    a = And(f1,Not(f2))

    s = Solver()
    s.add(a)
    if s.check() == unsat:
        return True
    
    return False


unique = {}
for l in allforms:
    there = False
    for u in unique:
        if impl(allforms[l], unique[u]):
            there = True
            break

    if there: continue

    unique[l] = allforms[l]

print unique
f.close()
