import math 

def trig_sin(x : "rad"):
    res = math.sin((x/2)*math.pi)
    if (res < 0.001 and res > -0.001):
        return 0
    else: return int(res)

def trig_cos(x : "rad"):
    res = math.cos((x/2)*math.pi)
    
    if (res < 0.001 and res > -0.001):
        return 0
    else: return int(res)

def trig_arcsin(x : "int"):
    if x > 1 or x < -1: return "Error" 
    res =  math.asin(x)
    
    if (round(res,2) == 1.57):
        return  1
    if (round(res,2) == -1.57):
        return  -1
 
    if (round(res,2) == 3.14):
        return  2
    if (round(res,2) == -3.14):
        return  -2

    return res

def trig_arccos(x : "int"):
    if x > 1 or x < -1: return "Error" 
    res = math.acos(x)

    if (round(res,2) == 1.57):
        return  1
    if (round(res,2) == -1.57):
        return  -1
 
    if (round(res,2) == 3.14):
        return  2
    if (round(res,2) == -3.14):
        return  -2

    return int(res)
