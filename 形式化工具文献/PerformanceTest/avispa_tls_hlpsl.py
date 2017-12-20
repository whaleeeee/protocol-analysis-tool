#!/usr/bin/python

import commands
import tempfile

import Protocol

class avispa_tls_hlpsl(Protocol.Protocol):

    def __init__(self,tool,models=[]):
        claims = ["sAsk","sAck","sBsk","sBck","aA","aB"]
        name = "avispa_tls"
        Protocol.Protocol.__init__(self,name,tool,models,claims)

    def roleList(self):
        return ["alice","bob"]

    def roleCount(self):
        return len(self.roleList())

    def generate(self,model,claim,scen):

        prot = """
role alice(A, B : agent,  
           H, PRF, KeyGen: hash_func,
           Ka, KbDummy, Ks: public_key,  %% Ks is the public key of a T3P (ie. CA)
           SND, RCV: channel (dy))
played_by A
def=

   local Na, Sid, Pa, PMS: text,
         Nb: text,
         State: nat,
         Finished: hash(hash(text.text.text).agent.agent.text.text.text),
         ClientK, ServerK: hash(agent.text.text.hash(text.text.text)),
         Kb: public_key,
         M: hash(text.text.text)

   const secA_clientk, secA_serverk, secB_clientk, secB_serverk : protocol_id

   init  State := 0

   transition

   1.  State = 0
       /\ RCV(start)
       =|>
       State' := 2
       /\ Na' := new()
       /\ Pa' := new()
       /\ Sid' := new()
       /\ SND(A.Na'.Sid'.Pa')

   % Since we abstract away from the negotiation
   % of cryptographic algorithms, here I simply assume
   % that the server must send back Pa.  (Essentially 
   % modelling that the client makes only one offer.)


   2.  State = 2
       /\ RCV(Nb'.Sid.Pa.{B.Kb'}_(inv(Ks)))
       =|>
       State' := 3
       /\ PMS' := new()
       /\ M' := PRF(PMS'.Na.Nb')
       /\ Finished' := H(PRF(PMS'.Na.Nb').A.B.Na.Pa.Sid)
       /\ ClientK' := KeyGen(A.Na.Nb'.PRF(PMS'.Na.Nb'))
       /\ ServerK' := KeyGen(B.Na.Nb'.PRF(PMS'.Na.Nb'))
       /\ SND({PMS'}_Kb'.
              {A.Ka}_(inv(Ks)).
              {H(Nb'.B.PMS')}_(inv(Ka)).
              {H(PRF(PMS'.Na.Nb').
               A.B.Na.Pa.Sid)
              }_KeyGen(A.Na.Nb'.PRF(PMS'.Na.Nb')))
       /\ witness(A,B,na_nb2,Na.Nb')

   4.  State = 3
       /\ RCV({Finished}_ServerK)
       =|>
       State' := 5
       /\ request(A,B,na_nb1,Na.Nb)
       /\ secret(ClientK,secA_clientk,{A,B})
       /\ secret(ServerK,secA_serverk,{A,B})

end role



role bob(A, B : agent,
         H, PRF, KeyGen: hash_func,
         KaDummy, Kb, Ks: public_key,
         SND, RCV: channel (dy))
played_by B
def=

   local Na, Nb, Sid, Pa, PMS: text,
         State: nat,
         Ka: public_key,
         ClientK, ServerK: hash(agent.text.text.hash(text.text.text))

   init  State := 1

   transition

   1.  State = 1
       /\ RCV(A.Na'.Sid'.Pa')
       =|>
       State' := 3
       /\ Nb' := new()
       /\ SND(Nb'.Sid'.Pa'.{B.Kb}_(inv(Ks)))
       /\ witness(B,A,na_nb1,Na'.Nb')

   2.  State = 3
       /\ RCV({PMS'}_Kb.{A.Ka'}_(inv(Ks)).
              {H(Nb.B.PMS')}_(inv(Ka')).
              {H(PRF(PMS'.Na.Nb).
               A.B.Na.Pa.Sid)
              }_KeyGen(A.Na.Nb.PRF(PMS'.Na.Nb)))
       =|>
       State' := 5
       /\ SND({H(PRF(PMS'.Na.Nb).
               A.B.Na.Pa.Sid)
              }_KeyGen(B.Na.Nb.PRF(PMS'.Na.Nb)))
       /\ request(B,A,na_nb2,Na.Nb)
       /\ ClientK' := KeyGen(A.Na.Nb.PRF(PMS'.Na.Nb))
       /\ ServerK' := KeyGen(B.Na.Nb.PRF(PMS'.Na.Nb))
       /\ secret(ClientK',secB_clientk,{A,B})
       /\ secret(ServerK',secB_serverk,{A,B})

end role



role session(A,B: agent,
             Ka, Kb, Ks: public_key,
             H, PRF, KeyGen: hash_func)
def=

   local  SA, SB, RA, RB: channel (dy)

   composition
                alice(A,B,H,PRF,KeyGen,Ka,Kb,Ks,SA,RA)
           /\   bob(A,B,H,PRF,KeyGen,Ka,Kb,Ks,SB,RB)

end role



role environment()
def=
   local SA, RA: channel (dy)

   const na_nb1, na_nb2 : protocol_id,
         h, prf, keygen : hash_func,
         a, b           : agent,
         ka, kb, ki, ks : public_key,
         secA_serverk, secA_clientK, secB_serverk,secB,clientK : protocol_id

   intruder_knowledge = { a, b, ka, kb, ks, ki, inv(ki),
                          {i.ki}_(inv(ks)) }  

   composition

        """

        prot += scen.hlpsl(self.roleList(),prefix="h,prf,keygen,",postfix=",ks,SA,RA")

        prot += """
end role

goal
    """
        #claims = ["sAsk","sAck","sBsk","sBck","aA","aB"]
        if claim == "sAsk":
            prot += "   secrecy_of secA_serverk\n"
        elif claim == "sBsk":
            prot += "   secrecy_of secB_serverk\n"
        elif claim == "sAck":
            prot += "   secrecy_of secA_clientk\n"
        elif claim == "sBck":
            prot += "   secrecy_of secB_clientk\n"
        elif claim == "aA":
            prot += "   authentication_on na_nb1\n"
        elif claim == "aB":
            prot += "   authentication_on na_nb2\n"
        else:
            raise Exception, ("Don't know claim %s" % claim)

        prot +="""

end goal

environment()

        """
        return (prot,"")

def registerall():
    Protocol.register(avispa_tls_hlpsl("ofmc",["Scen"]))
    Protocol.register(avispa_tls_hlpsl("satmc",["Scen"]))
    Protocol.register(avispa_tls_hlpsl("cl-atse",["Scen"]))
    Protocol.register(avispa_tls_hlpsl("ta4sp",["RepScen"]))

registerall()

if __name__ == "__main__":
    import Scenario

    sl = Scenario.genall(2,2,1)
    x = avispa_tls_hlpsl("ofmc",["Scen"])
    (prot,args) = x.generate("Scen","aB",sl[0])

    print prot
    print args



