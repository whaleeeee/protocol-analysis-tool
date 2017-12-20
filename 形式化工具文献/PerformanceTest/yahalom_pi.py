#!/usr/bin/python

"""
Provides the Pi input for the Yahalom protocol.

Modeler: Pascal Lafourcade
"""

import Protocol

class yahalom_pi(Protocol.Protocol):

    def __init__(self):
        models = ["Traces"]
        claims = ["sAk","sAna","sAnb","sBk","sBna","sBnb"]
        name = "yahalom"
        Protocol.Protocol.__init__(self,name,"proverif",models,claims)

    def roleList(self):
        return ["I","R","S"]

    def roleCount(self):
        return len(self.rolelist())

    def generate(self,model="Traces",claim="sAk",runs=0):
        prot = """
(*Yahalom  *)

(* 

A -> B : A, N_A
B -> S : B, { A, N_A, N_B }_Kbs
S -> A : { B, K_ab, N_A, N_B }_Kas, { A, K_ab }_Kbs
A -> B : { A, K_ab }_Kbs, { N_B }_Kab

*)

free c.

(* Public key cryptography *)

private fun serverkey/1.
(* just encryption, no signing *)

(* Symmetric key cryptography *)
fun symcrypt/2.
reduc symdecrypt(symcrypt(z,j),j) = z.

(* Effectively the claim signals *)
private free secretAna, secretAnb, secretBna, secretBnb, secretAk, secretBk.

not serverkey(B).
not serverkey(A).

(* Security claims to verify *)
        """

        qstr = claim[1:]
        prot += "query attacker:secret%s." % (qstr)

        prot += """

let processA =
	(* Choose the other host *)
	new Na;
        out(c, (A, Na));

	in(c,(ma1,ma2));
	let (b, k, =Na, nb) = symdecrypt(ma1, serverkey(A)) in	
	out(c, (m2, symcrypt(nb,k)));

	if b = A then
		out(c, symcrypt(secretAk, k));
		out(c, symcrypt(secretAna, Na));
		out(c, symcrypt(secretAnb, nb))
		else	if b = B then
			out(c, symcrypt(secretAk, k));
			out(c, symcrypt(secretAna, Na));
			out(c, symcrypt(secretAnb, nb)).

let processB =
	in(c, (a, na));
	new Nb;
	out(c, (B, symcrypt((a, na, Nb), serverkey(B))));

	in(c,(mb1,mb2));
	let (=a, k ) = symdecrypt(mb1, serverkey(B)) in
	let (=Nb) = symdecrypt(mb2, k) in

	if a = A then
		out(c, symcrypt(secretBna, na));
		out(c, symcrypt(secretBnb, Nb))
		else	if  a = B then
			out(c, symcrypt(secretBk, k));
			out(c, symcrypt(secretBna, na));
			out(c, symcrypt(secretBnb, Nb)).

let processS =
	in(c,(b,ms1));
	let (a,na,nb) = symdecrypt(ms1, serverkey(b)) in
	new k;
	out(c, (symcrypt((b, k, na, nb), serverkey(a)), (symcrypt((a,k),serverkey(b))) ) ).

process
	new A; 
	new B;
	new S;
	new I;

	out(c,A);
	out(c,B);
	out(c,S);
	out(c,I);	
	out(c,serverkey(I));

	((!processA) | (!processB) | (!processS))


        """
        args = ""
        return (prot,args)


Protocol.register(yahalom_pi())

if __name__ == "__main__":
    x = yahalom_pi()
    (prot,args) =  x.generate("Traces")
    print prot
    print args

