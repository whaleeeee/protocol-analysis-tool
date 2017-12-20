#!/usr/bin/python

#
# This is the spl version for Casper/FDR.
#
# Note that the state space of Casper is a bit weird, as it's not
# completely symbolic at the moment, but not really a full scenario
# either.
# You define the agent playing it, and you fix the role. Other roles are
# then fixed by the process. This is a form scenario-based verification.
#
# For one run, authentication)
#   I(A)
#   I(B) (?)
#   R(A)
#   R(B) (?)
#
# It means we don't have the additional burden of choosing the other
# agents. This distroys a number of our options. It's easy to cut them
# out after generation though (just filter out everything that is not
# ?(?,A,A,A,...)
#
import Protocol

class nspk_spl(Protocol.Protocol):

    def __init__(self,tool,models=[],fixedversion=False):
        self.fixedversion = fixedversion

        claims = ["sAna","sAnb","sBna","sBnb","aA","aB"]

        if self.fixedversion == True:
            name = "fix_nspk"
        else:
            name = "nspk"
        Protocol.Protocol.__init__(self,name,tool,models,claims)

    def roleList(self):
        return ["INITIATOR","RESPONDER"]

    def roleCount(self):
        return len(self.roleList())

    def generate(self,model,claim,scen):

        # For each run, we need to define the nonce strings here.
        narr = []
        nc = 1
        for run in scen:
            narr.append(",N%i" % nc)
            nc += 1
        narrstr = "".join(narr)

        scenstr = scen.spl(self.roleList(),narr)

        if self.fixedversion == True:
            fixstring = ",R"
        else:
            fixstring = ""

        prot = """
-- Needham Schroeder (Lowe) Public Key Protocol, 3 message version

#Free variables

I, R : Agent
na, nb : Nonce
PK : Agent -> PublicKey
SK : Agent -> SecretKey
InverseKeys = (PK, SK)

#Processes

INITIATOR(I,na) knows PK, SK(I)
RESPONDER(R,nb) knows PK, SK(R)

#Protocol description

0.    -> I : R
1.  I -> R : {na, I}{PK(R)}
2.  R -> I : {na, nb %s}{PK(I)}
3.  I -> R : {nb}{PK(R)} 
    """ % (fixstring)

        prot += """
#Specification

"""

        if claim == "sAna":
            prot += "Secret(I, na, [R])\n"
        elif claim == "sAnb":
            prot += "Secret(I, nb, [R])\n"
        elif claim == "sBna":
            prot += "Secret(R, na, [I])\n"
        elif claim == "sBnb":
            prot += "Secret(R, nb, [I])\n"
        elif claim == "aA":
            prot += "Agreement(I,R,[na,nb])\n"
        elif claim == "aB":
            prot += "Agreement(R,I,[na,nb])\n"
        else:
            raise Exception, ("Don't know claim %s" % claim)
        prot += """
#Actual variables

A, B, Mallory : Agent
Nm %s : Nonce

#Functions

symbolic PK, SK

#System

%s

#Intruder Information

Intruder = Mallory
IntruderKnowledge = {A, B, Mallory, Nm, PK, SK(Mallory)}

    """ % (narrstr,scenstr)

        return (prot,"")

def registerall(fixedversion=False):
    Protocol.register(nspk_spl("casperfdr",["Scen"],fixedversion))

registerall(False)
registerall(True)

if __name__ == "__main__":
    import Scenario

    sl = Scenario.genall(2,2,1)
    x = nspk_spl("casperfdr",["Scen"],False)
    (prot,args) = x.generate("Scen","sBnb",sl[0])

    print prot
    print args



