#!/usr/bin/python

"""
Provides the spdl input for the Scyther tool for the EKE protocol

Author: Cas Cremers
"""

import Protocol

class eke_spdl(Protocol.Protocol):

    def __init__(self):
        models = ["MaxProc","Traces"]
        claims = ["sAk","sBk","aA","aB"]
        name = "eke"
        Protocol.Protocol.__init__(self,name,"scyther",models,claims)

    def roleList(self):
        return ["I","R"]

    def roleCount(self):
        return len(self.rolelist())

    def generate(self,model="MaxProc",claim="aA",runs=0):
        prot = """
/* 
 * Encrypted Key Exchange
 *
 * k is a password, assumed to be known by both parties (and therefore
 * it has two parameters for the involved parties.
 *
 * In this model, we have that k(A,B) = k(B,A). As we have no means to
 * express this equality directly, we have introduced a protocol
 * "@swapkey", which effectively allows the intruder to exploit this
 * symmetry. This 'hack' is a correct model of the behaviour for this
 * protocol under the assumption of typed messages. If messages are not
 * typed, we would also have to consider the possible occurrence of the
 * key inside further subterms of an encrypted message. This is
 * currently out of scope for Scyther but also for any tool not
 * correctly supporting equational reasoning (which is most).
 *
 * Attack: An attack on authentication occurs with two runs of the
 * protocol, but is only found within a statespace including four runs.
 * The reason for this is that two runs are instantiated with the
 * 'swapkey' protocol for the attack.
 */

// Password infrastructure

secret k: Function;
usertype Key;

secret sk: Function;
const pk: Function;
inversekeys (pk,sk);

protocol @swapkey(X,Y)
{
	role X
	{
		var m: Ticket;

		read_!1(Y,X, {m}k(X,Y) );
		send_!2(X,Y, {m}k(Y,X) );
	}
	role Y
	{
	}
}

// The protocol description

protocol eke(I,R)
{
	role I
	{
		const Ea,Ca: Nonce;
		var Cb: Nonce;
		var K: Key;

		send_1(I,R, { pk(Ea) }k(I,R) );
		read_2(R,I, { { K }pk(Ea) }k(I,R) );
		send_3(I,R, { Ca }K );
		read_4(R,I, { Ca,Cb }K );
		send_5(I,R, { Cb }K );

		claim_sAk(I,Secret,K);
		claim_aA(I,Nisynch);
	}	
	
	role R
	{
		var Ca: Nonce;
		const Cb: Nonce;
		const K: Key;
		var PK: Ticket;

		read_1(I,R, { PK }k(I,R) );
		send_2(R,I, { { K }PK }k(I,R) );
		read_3(I,R, { Ca }K );
		send_4(R,I, { Ca,Cb }K );
		read_5(I,R, { Cb }K );

		claim_sBk(R,Secret,K);
		claim_aB(R,Nisynch);
	}
}

// The agents in the system

const Alice,Bob: Agent;

// An untrusted agent, with leaked information

const Eve: Agent;
untrusted Eve;
const ne: Nonce;
compromised k(Eve,Alice);
compromised k(Alice,Eve);
compromised sk(Eve);

        """
        args = ""
        args += " --filter=eke,%s " % (claim)
        if model == "Traces":
            args += "--max-runs=0"
        elif model == "MaxProc":
            ## Because we have the additional swapkey role, we add two
            args += "--max-runs=%i" % (runs+2)
            #args = "--max-runs=%i" % (runs)
        else:
            print "Don't know model %s." % (model)

        return (prot,args)


Protocol.register(eke_spdl())

if __name__ == "__main__":
    x = eke_spdl()
    (prot,args) =  x.generate("MaxProc","aB",2)
    print prot
    print args

