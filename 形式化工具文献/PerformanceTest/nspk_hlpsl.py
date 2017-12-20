#!/usr/bin/python

import Protocol

class nspk_hlpsl(Protocol.Protocol):

    def __init__(self,tool,models=[],fixedversion=False):
        self.fixedversion = fixedversion

        claims = ["sAna","sAnb","sBna","sBnb","aA","aB"]

        if self.fixedversion == True:
            name = "fix_nspk"
        else:
            name = "nspk"
        Protocol.Protocol.__init__(self,name,tool,models,claims)

    def roleList(self):
        return ["prole1","prole2"]

    def roleCount(self):
        return len(self.roleList())

    def generate(self,model,claim,scen):
        if self.fixedversion == True:
            fixstring = ".B"
        else:
            fixstring = ""

        prot = """
    role prole1 (A, B: agent,             
                Ka, Kb: public_key,      
                SND, RCV: channel (dy)) 
    played_by A def=

      local State : nat, 
            Na, Nb: text

      init State := 0

      transition  
       
        0.  State  = 0 /\ RCV(start) =|> 
            State':= 2 /\ Na' := new() /\ SND({Na'.A}_Kb)
                       /\ witness(A,B,bob_alice_na,Na')


        2.  State  = 2 /\ RCV({Na.Nb'%s}_Ka) =|> 
            State':= 4 /\ SND({Nb'}_Kb) 
                       /\ request(A,B,alice_bob_nb,Nb')
                       /\ secret(Na,naA,{A,B})
                       /\ secret(Nb',nbA,{A,B})

    end role



    role prole2 (A, B: agent,      
             Ka, Kb: public_key,      
             SND, RCV: channel (dy)) 
    played_by B def=

      local State : nat, 
            Na, Nb: text

      init State := 1

      transition 

        1.  State  = 1 /\ RCV({Na'.A}_Kb) =|> 
            State':= 3 /\ Nb' := new() /\ SND({Na'.Nb'%s}_Ka)
                       /\ witness(B,A,alice_bob_nb,Nb')

        3.  State  = 3 /\ RCV({Nb}_Kb) =|> 
            State':= 5 /\ request(B,A,bob_alice_na,Na)
                       /\ secret(Na,naB,{A,B})
                       /\ secret(Nb,nbB,{A,B})

    end role



    role environment() def=
        local SA, RA: channel (dy)

        const a, b         : agent,
              ka, kb, ki   : public_key,
              naA, naB, nbA, nbB,
              alice_bob_nb,
              bob_alice_na : protocol_id

        intruder_knowledge = {a, b, ka, kb, ki, inv(ki)}

        composition
        """ % (fixstring,fixstring)

        prot += scen.hlpsl(self.roleList(),postfix=",SA,RA")

        prot += """

    end role


    goal
    """
        if claim == "sAna":
            prot += "   secrecy_of naA\n"
        elif claim == "sBna":
            prot += "   secrecy_of naB\n"
        elif claim == "sAnb":
            prot += "   secrecy_of nbA\n"
        elif claim == "sBnb":
            prot += "   secrecy_of nbB\n"
        elif claim == "aA":
            prot += "   authentication_on alice_bob_nb\n"
        elif claim == "aB":
            prot += "   authentication_on bob_alice_na\n"
        else:
            raise Exception, ("Don't know claim %s" % claim)

        prot +="""
    end goal


    environment()
        """
        return (prot,"")

def registerall(fixedversion=False):
    Protocol.register(nspk_hlpsl("ofmc",["Scen"],fixedversion))
    Protocol.register(nspk_hlpsl("satmc",["Scen"],fixedversion))
    Protocol.register(nspk_hlpsl("cl-atse",["Scen"],fixedversion))
    Protocol.register(nspk_hlpsl("ta4sp",["RepScen"],fixedversion))

registerall(False)
registerall(True)

if __name__ == "__main__":
    import Scenario

    sl = Scenario.genall(2,2,1)
    x = nspk_hlpsl("ofmc",["Scen"],True)
    (prot,args) = x.generate("Scen","aA",sl[0])

    print prot
    print args



