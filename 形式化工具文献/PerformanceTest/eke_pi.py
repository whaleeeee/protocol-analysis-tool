#!/usr/bin/python

"""
Provides the spdl input for the ProVerif tool for the EKE protocol

Author: Pascal Lafourcade and Cas Cremers

Thanks to Bruno Blanchet for assistance.
"""

import Protocol

class eke_pi(Protocol.Protocol):

    def __init__(self):
        models = ["Traces"]
        claims = ["sAk","sBk","aA","aB"]
        name = "eke"
        Protocol.Protocol.__init__(self,name,"proverif",models,claims)

    def roleList(self):
        return ["I","R"]

    def roleCount(self):
        return len(self.rolelist())

    def generate(self,model="Traces",claim="sAk",runs=0):
        prot = """

(* Bellovin, Merritt, Oakland 92, section 2.1
   Version in which A and B talk to anybody *)

free c.

(* Symmetric cryptography
   One does not know whether decryption succeeds or not
   For use with weak secrets *)

fun enc/2.
fun dec/2.
equation dec(enc(x,y),y) = x.
equation enc(dec(x,y),y) = x.

(* Symmetric cryptography
   One knows whether decryption succeeds or not *)

fun senc/2.
reduc sdec(senc(x,y),y) = x.

(* Public key cryptography *)

fun penc/2.
fun pk/1.
reduc pdec(penc(x,pk(y)),y) = x.

(* Host name *)

fun host/2.   (* host(P1,P2) is the dishonest host that uses password P1
                 to talk to host1 and password P2 to talk to host2. *)

free host1, host2. (* host1 and host2 are two honest hosts *)
private free P12, P11, P22. (* P12 is the password between host1 and host2,
                           P11 is the password between host1 and itself
                   P22 is the password between host2 and itself *)

private free startA, startB. (* Channels for starting initiator and responder *)
private free p. (* Channel for sending names of honest hosts *)

private free secretACa, secretACb, secretBCa, secretBCb, secretBk, secretAk, secretAskea, secretApkea, secretBpkea.

(* Security claims to verify *)

        """

        if claim.startswith("s"):
            qstr = claim[1:]
            prot += "query attacker:secret%s." % qstr
        elif claim == "aA":
            prot += "query ev:endAfull(x1,x2,x3,x4) ==> ev:beginBfull(x1,x2,x3,x4)."
        elif claim == "aB":
            prot += "query ev:endBfull(x1,x2,x3,x4) ==> ev:beginAfull(x1,x2,x3,x4)."
        else:
            assert False, "Unrecognized claim %s" % claim

        # query attacker:secretACa;
        #       attacker:secretACb;
        #       attacker:secretAk;
        #       attacker:secretAskea;
        #       attacker:secretApkea;
        #       attacker:secretBCa;
        #       attacker:secretBCb;
        #       attacker:secretBk;
        #       attacker:secretBpkea.
        # 
        # query ev:endAfull(x1,x2,x3,x4) ==> ev:beginBfull(x1,x2,x3,x4).
        # query ev:endBfull(x1,x2,x3,x4) ==> ev:beginAfull(x1,x2,x3,x4).
         
        prot += """
                                             
(* Code for the initiator hostA talking to responder hostB with password P *)

let processA =
    !
    in(startA, (hostA, hostB, P));
    new sEA;
    let EA = pk(sEA) in
    out(c, (hostA, enc(EA, P)));
    in(c,m2);
    let R = pdec(dec(m2,P),sEA) in
    new challengeA;
    out(c, senc((challengeA), R));
    in(c, m4);
    let (=challengeA, challengeB) = sdec(m4, R) in

    (* signal done *)
    event beginAfull(hostA,hostB,challengeA,challengeB); 

    out(c, senc((challengeB), R));

        (* If I am talking to an honest principal *)    
    in(p, m);
        if hostB  = m  then 
    event endAfull(hostA,hostB,challengeA,challengeB);
    out(c, senc(secretACa, challengeA));
    out(c, senc(secretACb, challengeB));
    out(c, senc(secretAk, R));
    out(c, senc(secretAskea, sEA));
    out(c, senc(secretApkea, EA)).

(* Code for the responder hostB talking to initiator hostA with password P *)

let processB =
    !
    in(startB, (hostA, hostB, P));
    in(c, (=hostA, m));
    let EA = dec(m, P) in
    new R;
    out(c, enc(penc(R, EA), P));
    in(c,m3);
    let (challengeA) = sdec(m3, R) in
    new challengeB;

    (* signal done *)
        event beginBfull(hostA,hostB,challengeA,challengeB); 

    out(c, senc((challengeA, challengeB), R));
    in(c, m5);
    if sdec(m5, R) = (challengeB) then

    (* If I am talking to an honest principal *)    
    in(p, mf);
        if hostA  = mf  then 
    event endBfull(hostA,hostB,challengeA,challengeB);
    out(c, senc(secretBCa, challengeA));
    out(c, senc(secretBCb, challengeB));
    out(c, senc(secretBk, R));
    out(c, senc(secretBpkea, EA)).

process 
    processA | processB | 

        """

        if claim.startswith('s'):
            prot += """
    (* Output honest participants on channel p *)
    (!out(p, host1)) |

    (* Initiator host1 talking to host1 *)
    (!out(startA, (host1, host1, P11))) |
    (* Initiator host1 talking to the adversary *)
    (!in(c, (P1,P2)); out(startA, (host1, host(P1,P2), P1))) | 

    (* Responder host1 talking to host1 *)
    (!out(startB, (host1, host1, P11))) |
    (* Responder host1 talking to the adversary *)
    (!in(c, (P1,P2)); out(startB, (host(P1,P2), host1, P1)))
            """
        elif claim.startswith('a'):
            prot += """
    (* Output honest participants on channel p *)
    (!out(p, host1)) | (!out(p, host2)) |

    (* Initiator host1 talking to host2 *)
    (!out(startA, (host1, host2, P12))) |
    (* Initiator host1 talking to host1 *)
    (!out(startA, (host1, host1, P11))) |
    (* Initiator host1 talking to the adversary *)
    (!in(c, (P1,P2)); out(startA, (host1, host(P1,P2), P1))) | 

    (* Initiator host2 talking to host1 *)
    (!out(startA, (host2, host1, P12))) |   
    (* Initiator host2 talking to host2 *)
    (!out(startA, (host2, host2, P22))) |
    (* Initiator host2 talking to the adversary *)
    (!in(c, (P1,P2)); out(startA, (host2, host(P1,P2), P2))) | 

    (* Responder host2 talking to host1 *)
    (!out(startB, (host1, host2, P12))) |
    (* Responder host2 talking to host2 *)
    (!out(startB, (host2, host2, P22))) |
    (* Responder host2 talking to the adversary *)
    (!in(c, (P1,P2)); out(startB, (host(P1,P2), host2, P2))) |

    (* Responder host1 talking to host2 *)
    (!out(startB, (host2, host1, P12))) |
    (* Responder host1 talking to host1 *)
    (!out(startB, (host1, host1, P11))) |
    (* Responder host1 talking to the adversary *)
    (!in(c, (P1,P2)); out(startB, (host(P1,P2), host1, P1)))
            """
        else:
            assert False, "Unrecognized claim prefix for '%s'" % (claim)

        args = ""
        return (prot,args)

Protocol.register(eke_pi())

if __name__ == "__main__":
    x = eke_pi()
    (prot,args) =  x.generate("Traces","sAk",2)
    print prot
    print args



