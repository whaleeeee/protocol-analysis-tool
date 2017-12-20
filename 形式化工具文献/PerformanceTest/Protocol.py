#!/usr/bin/python

protocols = []

import Tool
import Test

maxnamelen = 0
maxclaimlen = 0

class Protocol(object):

    """ It is really the class of protocol generators """

    def __init__(self,name,tool,models=None,claims=None):

        global maxnamelen,maxclaimlen

        self.name = name
        if len(name) > maxnamelen:
            maxnamelen = len(name)

        self.toolname = tool
        self.claims = claims
        for cl in self.claims:
            if len(cl) > maxclaimlen:
                maxclaimlen = len(cl)
        self.models = models

        Tool.register(self.toolname,self)

    def __repr__(self):
        s = "%s: %s %s %s" % (self.name,self.toolname,self.models,self.claims)
        return s





def getProtocolList():
    global protocols

    return protocols

def register(prot):
    global protocols

    if not prot in protocols:
        protocols.append(prot)

