#!/usr/bin/python

"""
Provides the spdl input for the ProVerif tool for the NSPK as well as
Lowe's fixed version.

Author: Cas Cremers / Bruno Blanchet / Pascal Lafourcade
"""

import Protocol

class nspk_pi(Protocol.Protocol):

    def __init__(self,fixedversion=False):
        models = ["Traces"]
        claims = ["sAna","sAnb","sBna","sBnb","aA","aB"]
        self.fixedversion = fixedversion
        if self.fixedversion == False:
            name = "nspk"
        else:
            name = "fix_nspk"
        Protocol.Protocol.__init__(self,name,"proverif",models,claims)

    def roleList(self):
        return ["I","R"]

    def roleCount(self):
        return len(self.rolelist())

    def generateSecrecy(self,model="MaxProc",claim="sNa",runs=0):
        if self.fixedversion == True:
            fix1 = ",B"
            fix2 = ",=X"
            fix3 = ",A"
        else:
            fix1 = ""
            fix2 = ""
            fix3 = ""
        prot = """
(* Needham-Schroeder BASIC *)

(* Because we only consider secrecy, we just need one agent A *)

(*

A -> B : {Na,A}pk(B)
B -> A : {Na,Nb%s}pk(A)
A -> B : {Nb}pk(B)

*)


free c.

(* Public key cryptography *)

fun pk/1.
private fun sk/1.
(* just encryption, no signing *)
fun encrypt/2.
reduc decrypt(encrypt(x,pk(y)),sk(y)) = x.

(* Symmetric key cryptography *)
fun symcrypt/2.
reduc symdecrypt(symcrypt(z,j),j) = z.

(* Effectively the claim signals *)
private free secretAna, secretAnb, secretBna, secretBnb.

(* Security claims to verify *)
""" % (fix1)
        
        qstr = claim[1:]
        prot += "query attacker:secret%s." % (qstr)

        prot += """

let processA =
	(* Choose the other host *)
        in(c, X);
	new Na;
	out(c, encrypt((Na,A),pk(X)));
	in(c,m2);
	let (=Na, nb %s) = decrypt(m2, sk(A)) in
	out(c, encrypt(nb,pk(X)));
	if X = A then
		out(c, symcrypt(secretAna, Na));
		out(c, symcrypt(secretAnb, nb)).

let processBbyA =
	in(c,m1);
	let (na,Y) = decrypt(m1, sk(A)) in
	new Nb;
	out(c, encrypt((na, Nb %s), pk(Y)));
	in(c,m3);
	let (=Nb) = decrypt(m3, sk(A)) in
	if Y = A then
		out(c, symcrypt(secretBna, na));
		out(c, symcrypt(secretBnb, Nb)).

process
	new A; 
	new I;

	out(c,A);
	out(c,I);
	out(c,sk(I));

	((!processA) | (!processBbyA))

        """ % (fix2,fix3)

        args = ""
        return (prot,args)

    def generateAuthentication(self,model="MaxProc",claim="aA",runs=0):
        if self.fixedversion == True:
            fix1 = ",B"
            fix2 = ",=X"
            fix3 = ", pkA"
        else:
            fix1 = ""
            fix2 = ""
            fix3 = ""
        prot = """
(* Needham-Schroeder BASIC *)

(*
A -> B : {Na,A}pk(B)
B -> A : {Na,Nb%s}pk(A)
A -> B : {Nb}pk(B)
*)

free c.

private free p.

(* Public key cryptography *)

fun pk/1.
private fun sk/1.

(* just encryption, no signing *)
fun encrypt/2.
reduc decrypt(encrypt(x,pk(y)),y) = x.

(* Symmetric key cryptography *)
fun symcrypt/2.
reduc symdecrypt(symcrypt(z,j),j) = z.

(* Effectively the claim signals *)
private free secretAna, secretAnb, secretBna, secretBnb.

(* Security claims to verify *)
(*
query attacker:secretAna;
      attacker:secretAnb;
      attacker:secretBna;
      attacker:secretBnb.
*)
        """ % (fix1)
        if claim == "aA":
            prot += "query ev:endAfull(x1,x2,x3,x4) ==> ev:beginBfull(x1,x2,x3,x4).\n"
        else:
            prot += "query ev:endBfull(x1,x2,x3,x4) ==> ev:beginAfull(x1,x2,x3,x4).\n"
        prot += """

let processI =

    (* Choose the other host *)
    in(c, pkX);

    (* send message 1 *)
    new Na;
    out(c, encrypt((Na,pkA),pkX));

    (* read message 2 *)
    in(c,m2);
    let (=Na, nb%s) = decrypt(m2, skA) in

    (* signal done *)
    event beginAfull(pkA,pkX,Na,nb); 

    (* send message 3 *)
    out(c, encrypt((nb),pkX));

        in(p,m);
        if m  = pkX  then 

    out(c, symcrypt(secretAna, Na));
    out(c, symcrypt(secretAnb, nb));
    event endAfull(pkA,pkX,Na,nb).
        
let processR =
        (* read message 1 *)
        in(c,m1);
        let (na,pkpartner) = decrypt(m1, skA) in

            (* send message 2 *)
            new Nb;

            (* signal done *)
            event beginBfull(pkpartner,pkA,na,Nb);

            out(c, encrypt((na, Nb%s), pkpartner));

            (* read message 3 *)
            in(c,m3);
            let (=Nb) = decrypt(m3, skA) in

                
(* if we are all honest, security properties can be expected to hold *)

        in(p,m);
        if m  = pkpartner  then 
            out(c, symcrypt(secretBna, na));
            out(c, symcrypt(secretBnb, Nb));
            event endBfull(pkpartner,pkA,na,Nb).
        
    process
    !(
    new skA; 
    let pkA = pk(skA) in
    out(c,pkA);
        ((!processI) | (!processR)) | !(out(p,pkA)))

        """ % (fix2,fix3)

        args = ""
        return (prot,args)

    def generate(self,model="MaxProc",claim="aA",runs=0):

        if claim[0] == 'a':
            return self.generateAuthentication(model,claim,runs)
        else:
            return self.generateSecrecy(model,claim,runs)

Protocol.register(nspk_pi(False))
Protocol.register(nspk_pi(True))

if __name__ == "__main__":
    x = nspk_pi(False)
    (prot,args) =  x.generate("MaxProc","aA")
    print prot
    print args

        
