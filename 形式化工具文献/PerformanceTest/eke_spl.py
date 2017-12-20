#!/usr/bin/python

#
# This is the spl version of EKE for Casper/FDR.
#
# Note that the state space of Casper is a bit weird, as it's not
# completely symbolic at the moment, but not really a full scenario
# either.
# You define the agent playing it, and you fix the role. Other roles are
# then fixed by the process. This is a form of scenario-based verification.
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

class eke_spl(Protocol.Protocol):

    def __init__(self,tool,models=[]):

        claims = ["sAk","sBk","aA","aB"]

        Protocol.Protocol.__init__(self,"eke",tool,models,claims)

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

        scenstr = scen.spl(self.roleList(),narr,insertpks=True)
        # Count and generate needed keys
        initkey = 1
        pubkeys = []
        seckeys = []
        pubkeydef = ""
        seckeydef = ""
        invkeydef = ""
        for run in scen:
            if run.rolename() == 'I':
                pubkeys.append("KI%s" % (initkey))
                seckeys.append("KJ%s" % (initkey))
                pubkeydef += pubkeys[-1] + ","
                seckeydef += seckeys[-1] + ","
                invkeydef += "(%s,%s), " % (pubkeys[-1],seckeys[-1])
                initkey += 1

        prot = """
-- Encrypted Key Exchange

#Free variables
A, B : Agent
na, nb : Nonce
k1 : PubKey 
k2 : SecKey
r : SessionKey
P : Agent x Agent -> Password
InverseKeys = (k1,k2), (P,P), (r,r)

#Processes
INITIATOR(A,k1,k2,na) knows P(A,B)
RESPONDER(B,r,nb) knows P(A,B)

#Protocol description
0.    -> A : B
1.  A -> B : {k1}{P(A,B)}
2.  B -> A : {{r}{k1}}{P(A,B)}
3.  A -> B : {na}{r}
4.  B -> A : {na,nb}{r}
5.  A -> B : {nb}{r}

#Specification
"""

        if claim == "sAk":
            prot += "Secret(A, r, [B])\n"
        elif claim == "sBk":
            prot += "Secret(B, r, [A])\n"
        elif claim == "aA":
            prot += "Agreement(A,B,[na,nb,r])\n"
        elif claim == "aB":
            prot += "Agreement(B,A,[na,nb,r])\n"
        else:
            raise Exception, ("Don't know claim %s" % claim)
        prot += """
#Actual variables
A, B, Mallory : Agent
Na, Nb : Nonce
Nm %s : Nonce
%s Km1 : PubKey
%s Km2 : SecKey
R, Rm : SessionKey
InverseKeys = %s (Km1,Km2), (R,R), (Rm,Rm)

#Functions
symbolic P

#Equivalences
forall AA, BB : Agent . P(AA,BB) = P(BB,AA)

#System

%s
INITIATOR(Alice, K1, K2, Na)
RESPONDER(Alice, R, Nb)

#Intruder Information

Intruder = Mallory
IntruderKnowledge = {A, B, Mallory, Nm, Km1, Km2, Rm, P(Alice, Mallory), P(Bob,Mallory)}
    """ % (narrstr,pubkeydef,seckeydef,invkeydef,scenstr)

        return (prot,"")

Protocol.register(eke_spl("casperfdr",["Scen"]))

if __name__ == "__main__":
    import Scenario

    sl = Scenario.genall(2,5,1)
    x = eke_spl("casperfdr",["Scen"])
    (prot,args) = x.generate("Scen","sBk",sl[7])

    print prot
    print args



