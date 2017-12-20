#!/usr/bin/python

from misc import *
import Scenario

def triangle():
    for n in range(0,11):
        for k in range(0,n+1):
            print losanitsch(n,k),
        print

def diagonal(n,k):
    s = "Diagonal (%i,%i): " % (n,k)
    for x in range(0,10):
        s += "%i " % losanitsch(n,k)
        n += 1
        k += 1
    print s

def cn(r,a):
    return r * a * (a+1)**(r-1)

def casLos(p,r):
    return losanitsch(cn(r,2)+p-1,p)

def main():
    for r in range(1,10):
        for p in range(1,10):
            print Scenario.countscenarios(r,p,1),
            print pascal(cn(r,1)+p-1,p),
            print 
        print

if __name__ == "__main__":
    main()

