def __mkQueue(l):
    from queue import Queue
    s = Queue()
    for x in l:
        s.put(x)
    return s

def __mkList(q):
    l = []
    for i in range(1,q.qsize()+1):
        l.append(q.get())
    return l

def queue_put(s: "queue", x: "int"):
    s2 = __mkQueue(s)
    s2.put(x)
    return __mkList(s2)

def queue_get(s: "queue"):
    try:
        q = __mkQueue(s)
        return q.get_nowait()
    except Exception as e:
        return "Error"

def queue_qsize(s: "queue"):
    return __mkQueue(s).qsize()
