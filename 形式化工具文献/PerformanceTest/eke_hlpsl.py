#!/usr/bin/python

import Protocol

class eke_hlpsl(Protocol.Protocol):

    def __init__(self,tool,models=[]):

        claims = ["sAk","sBk","aA","aB"]
        name = "eke"

        Protocol.Protocol.__init__(self,name,tool,models,claims)

    def roleList(self):
        return ["eke_Init","eke_Resp"]

    def roleCount(self):
        return len(self.roleList())

    def generate(self,model,claim,scen):

        prot = """
role eke_Init (A,B: agent,
               Kab: symmetric_key,
               Snd,Rcv: channel(dy))
played_by A
def=

  local State   : nat,
        Ea      : public_key,
        Na,Nb,K : text

  const sec_k1 : protocol_id

  init  State := 0

  transition

   1. State = 0
      /\ Rcv(start)
      =|> 
      State' := 1
      /\ Ea' := new()
      /\ Snd({Ea'}_Kab) 

   2. State = 1
      /\ Rcv({{K'}_Ea}_Kab)
      =|> 
      State' := 2
      /\ Na' := new()
      /\ Snd({Na'}_K') 
      /\ secret(K',sec_k1,{A,B})
      /\ witness(A,B,na,Na')

   3. State = 2
      /\ Rcv({Na.Nb'}_K) 
      =|> 
      State' := 3
      /\ Snd({Nb'}_K) 
      /\ request(A,B,nb,Nb')

end role



role eke_Resp (A,B: agent,
               Kab: symmetric_key,
               Snd,Rcv: channel(dy))
played_by B
def=

  local State   : nat,
        Na,Nb,K : text,
        Ea      : public_key

  const sec_k2 : protocol_id

  init  State := 0

  transition

   1. State = 0 /\ Rcv({Ea'}_Kab)
      =|> 
      State' := 1
      /\ K' := new()
      /\ Snd({{K'}_Ea'}_Kab) 
      /\ secret(K',sec_k2,{A,B})

   2. State = 1 /\ Rcv({Na'}_K)
      =|> 
      State' := 2
      /\ Nb' := new()
      /\ Snd({Na'.Nb'}_K)
      /\ witness(B,A,nb,Nb')

   3. State = 2
      /\ Rcv({Nb}_K)
      =|> 
      State' := 3
      /\ request(B,A,na,Na)

end role



role session(A,B: agent,
             Kab,Kaa,Kbb,Kai,Kbi: symmetric_key)
def=

  local SA, RA, SB, RB: channel (dy)

  composition
     eke_Init(A,B,Kab,SA,RA)
  /\ eke_Resp(A,B,Kab,SB,RB)

end role

role environment()
def=

  local SA, RA, SB, RB: channel (dy)

  const a, b   : agent,
        kab,kaa,kai,kbi,kbb    : symmetric_key,
        na, nb : protocol_id

  intruder_knowledge={a,b,kai,kbi}

  composition
        """

        def symkey(role,al):
            a = al[0]
            b = al[1]
            if a > b:
                a = al[1]
                b = al[0]
            return "k%s%s" % (a,b)

        prot += scen.hlpsl(self.roleList(),postfix=",SA,RA",symkey=symkey)

        prot += """
end role

goal

 % Confidentiality (G12)
 % secrecy_of sec_k1, sec_k2

 % Message authentication (G2)
 % EKE_Init authenticates EKE_Resp on nb
 % authentication_on nb

 % Message authentication (G2)
 % EKE_Resp authenticates EKE_Init on na
 % authentication_on na

    """
        if claim == "sAk":
            prot += "   secrecy_of sec_k1\n"
        elif claim == "sBk":
            prot += "   secrecy_of sec_k2\n"
        elif claim == "aA":
            prot += "   authentication_on nb\n"
        elif claim == "aB":
            prot += "   authentication_on na\n"
        else:
            raise Exception, ("Don't know claim %s" % claim)

        prot +="""
    end goal


    environment()
        """
        return (prot,"")

def registerall():
    Protocol.register(eke_hlpsl("ofmc",["Scen"]))
    Protocol.register(eke_hlpsl("satmc",["Scen"]))
    Protocol.register(eke_hlpsl("cl-atse",["Scen"]))
    Protocol.register(eke_hlpsl("ta4sp",["RepScen"]))

registerall()

if __name__ == "__main__":
    import Scenario

    sl = Scenario.genall(2,2,1)
    x = eke_hlpsl("ofmc",["Scen"])
    (prot,args) = x.generate("Scen","sAk",sl[1])

    print prot
    print args



