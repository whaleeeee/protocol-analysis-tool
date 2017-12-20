#!/usr/bin/python


def bincoeff(n,r, \
    cache={}):
    try:
        return cache[(n,r)]
    except KeyError:
        x = 1
        for i in range(r+1, n+1):
            x *= i
        for i in range(1, n-r+1):
            x /= i
        cache[(n,r)] = x
        return x

def pascal(n,r):
    return bincoeff(n,r)

def power(a,b):
    r = 1
    for i in range(0,b):
        r = r * a
    return r

def chooseback(n,k):
    return bincoeff(n+k-1,k)

def losanitsch(n,k,cache={}):
    """
    Losanitsch's triangle
    Cf. http://mathworld.wolfram.com/LosanitschsTriangle.html
    """
    if (n==0) or (k==0) or (k==n):
        return 1
    else:
        try:
            return cache[(n,k)]
        except KeyError:
            x = losanitsch(n-1,k-1) 
            x += losanitsch(n-1,k)
            if (n % 2 == 0) and (k % 2 == 1):
                x -= bincoeff( (n/2)-1,(k-1)/2 )
            cache[(n,k)] = x
            return x

def parseClock(ss):
    """
    Parse a clock string into a float
    """
    line = ss.split(":")
    store = 0
    for sx in line:
        store = store * 60
        store = store + float(sx)
    return store

