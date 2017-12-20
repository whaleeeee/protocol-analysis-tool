#!/usr/bin/python

"""
Provides the spdl input for the Scyther tool for the TLS protocol as modeled in the AVISPA project.

Author: Cas Cremers
"""

import commands
import tempfile

import Protocol

class avispa_tls_spdl(Protocol.Protocol):

    def __init__(self):
        models = ["MaxProc","Traces"]
        claims = ["sAsk","sAck","sBsk","sBck","aA","aB"]
        name = "avispa_tls"
        Protocol.Protocol.__init__(self,name,"scyther",models,claims)

    def roleList(self):
        return ["a","b"]

    def roleCount(self):
        return len(self.rolelist())

    def generate(self,model="MaxProc",claim="aA",runs=0):
        cppprot = """
/*
 * This is a model of the TLS version as modeled by Paulson
 * 
 * Slightly modified to correspond exactly to the version in the Avispa
 * repository by Paul Hankes Drielsma.
 *
 * The .cpp file cannot be fed into scyther directly; rather, one needs
 * to type:
 *
 *  cpp tls-paulson.cpp >tls-paulson.spdl
 *
 * in order to generate a valid spdl file for scyther.
 *
 * This allows for macro expansion, as seen in the next part.
 *
 */

#define CERT(a) { a,pk(a) }sk(Terence)
#define MSG0 a,na,sid,pa
#define MSG1 nb,sid,pb
#define MSG2 CERT(b)
#define MSG3 CERT(a)
#define MSG4 { pms }pk(b)
#define MSG5 { hash(nb,b,pms) }sk(a)
#define MSGS MSG0,MSG1,MSG2,MSG3,MSG4,MSG5
#define M PRF(pms,na,nb)
#define Finished hash(M,MSGS)
#define CLIENTK keygen(a,na,nb,M)
#define SERVERK keygen(b,na,nb,M)
#define MSG6 { Finished }CLIENTK
#define MSG7 { Finished }SERVERK

usertype Params, Bool, SessionID;

const pk,hash: Function;
secret sk,unhash: Function;
inversekeys(pk,sk);
inversekeys(hash,unhash);

const PRF: Function;
secret unPRF: Function;
inversekeys(PRF,unPRF);

const keygen: Function;
secret unkeygen: Function;
inversekeys(keygen, unkeygen);

const pa,pb: Params;
const false,true: Bool;

const Alice, Bob, Eve: Agent;
const Terence: Agent;

protocol tlspaulson-avispa(a,b)
{
    role a
    {
        const na: Nonce;
        const sid: SessionID;
        const pms: Nonce;
        var nb: Nonce;
        var pb: Params;

        send_1( a,b, MSG0 );
        read_2( b,a, MSG1,MSG2 );
        send_4( a,b, MSG3,MSG4,MSG5,MSG6 );
        read_8( b,a, MSG7 );

        claim_sAsk(a, Secret, SERVERK);
        claim_sAck(a, Secret, CLIENTK);
        claim_aA(a, Niagree);

    }   
    
    role b
    {
        var na: Nonce;
        var sid: SessionID;
        var pms: Nonce;
        const nb: Nonce;
        const pb: Params;

        read_1( a,b, MSG0 );
        send_2( b,a, MSG1,MSG2 );
        read_4( a,b, MSG3,MSG4,MSG5,MSG6 );
        send_8( b,a, MSG7 );

        claim_sBsk(b, Secret, SERVERK);
        claim_sBck(b, Secret, CLIENTK);
        claim_aB(b, Niagree);
    }
}


untrusted Eve;
compromised sk(Eve);
const ne: Nonce;
const side: SessionID;
const pe: Params;

compromised CERT(Alice);
compromised CERT(Bob);

        """

        fh = tempfile.NamedTemporaryFile()
        fname = fh.name
        fh.write(cppprot)
        fh.write("\n")
        fh.flush()

        cmd = "cpp %s" % (fname)
        prot = commands.getoutput(cmd)
        fh.close()

        args = ""

        args += " --filter=tlspaulson-avispa,%s " % (claim)

        if model == "Traces":
            args += "--max-runs=0"
        elif model == "MaxProc":
            args += "--max-runs=%i" % (runs)
        else:
            Error.showWarning("Don't know model %s." % (model))
        return (prot,args)


Protocol.register(avispa_tls_spdl())

if __name__ == "__main__":
    x = avispa_tls_spdl()
    (prot,args) =  x.generate("MaxProc","aB",2)
    print prot
    print args

