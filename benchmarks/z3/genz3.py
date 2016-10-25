from z3 import *
import itertools


land = []
lor = []
neg = []
issat = []
isvalid = []
#

threshold = 300
depth = 3

def f_sat(a):
    s = Solver()
    s.add(a)
    return s.check() != unsat

vc = 0
def f_valid(a):
    global vc
    vc = vc + 1
    print vc
    
    s = Solver()
    s.add(a)
    if s.check() == unsat:
        return False
    s.reset()
    s.add(Not(a))
    if s.check() == sat:
        return False
    return True


x = Bool('x')
y = Bool('y')
m = {}
m[0] = [x,y,True,False]

def generateForms():

    
    for i in range(1,depth):
        m[i] = []

        for a in m[i-1]:
            neg.append((a,Not(a)))
            m[i].append(Not(a))
            
            issat.append((a,f_sat(a)))
            isvalid.append((a, f_valid(a)))

        for (a,b) in itertools.product(m[i-1], repeat=2): 
            if len(land) > threshold: return 
            conj = And(a,b)
            disj = Or(a,b)
            
            land.append((a,b,conj))
            lor.append((a,b,disj))

            m[i].append(conj)
            m[i].append(disj)

def tab(t):
    tp = map(lambda x: str(x), t)
    return "\t".join(tp)

def toFile(name, l):
    f = open("./facts/"+name+".facts", 'w')
    for t in l:
        #print tab(t)
        f.write(tab(t)+"\n")

generateForms()


print len(neg)
print len(land)
print len(lor)
print len(issat)
print len(isvalid)
            
allforms = []
for i in range(1,depth+1):
    for a in m[i-1]:
        allforms.append(a)


eq = {}
h = {}
count = 0

def ineq(f,e):
    for fp in e:
        if f_valid (f == fp):
            return True

    return False

count = 0
for f in allforms:
    found = False
    for i in eq:
        if ineq(f,eq[i]):
            eq[i].append(f)
            h[f] = i
            found = True
            break

    if found == False:
        count = count + 1
        eq[count] = [f]
        h[f] = count

print eq
print h

land = map(lambda (a,b,c): (h[a],h[b],h[c]), land)
lor = map(lambda (a,b,c): (h[a],h[b],h[c]), lor)
neg = map(lambda (a,b): (h[a],h[b]), neg)
issat = map(lambda (a,b): (h[a],b), issat)
isvalid = map(lambda (a,b): (h[a],b), isvalid)

print land
print lor
print neg
print issat
print isvalid

toFile("and",land)
toFile("or",lor)
toFile("neg",neg)
toFile("sat",issat)
toFile("valid",isvalid)
