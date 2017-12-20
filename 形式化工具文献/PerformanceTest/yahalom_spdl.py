#!/usr/bin/python

"""
Provides the spdl input for the Yahalom protocol.

Author: Cas Cremers
"""

import Protocol

class yahalom_spdl(Protocol.Protocol):

    def __init__(self):
        models = ["MaxProc","Traces"]
        claims = ["sAk","sAna","sAnb","sBk","sBna","sBnb"]
        name = "yahalom"
        Protocol.Protocol.__init__(self,name,"scyther",models,claims)

    def roleList(self):
        return ["I","R","S"]

    def roleCount(self):
        return len(self.rolelist())

    def generate(self,model="MaxProc",claim="sAk",runs=0):
        prot = """
# Yahalom
#
# Modelled after the description in the SPORE library
# http://www.lsv.ens-cachan.fr/spore/yahalom.html
#
#

secret k : Function;

usertype SessionKey;

protocol yahalom(I,R,S)
{
    role I
    {
        const Ni: Nonce;
        var Nr: Nonce;
        var T: Ticket;
        var Kir: SessionKey;

        send_1(I,R, I,Ni);
        read_3(S,I, {R,Kir,Ni,Nr}k(I,S), T );
        send_4(I,R, T, {Nr}Kir );

        claim_sAk(I, Secret,Kir);
        claim_sAna(I, Secret,Ni);
        claim_sAnb(I, Secret,Nr);
    }

    role R
    {
        const Nr: Nonce;
        var Ni: Nonce;
        var T: Ticket;
        var Kir: SessionKey;

        read_1(I,R, I,Ni);
        send_2(R,S, R, {I,Ni,Nr}k(R,S) );
        read_4(I,R, {I,Kir}k(R,S) , {Nr}Kir );

        claim_sBk(R, Secret,Kir);
        claim_sBna(R, Secret,Ni);
        claim_sBnb(R, Secret,Nr);
    }

    role S
    {
        const Kir: SessionKey;
        var Ni,Nr: Nonce;

        read_2(R,S, R, {I,Ni,Nr}k(R,S) );
        send_3(S,I, {R,Kir,Ni,Nr}k(I,S), {I,Kir}k(R,S) );
    }
}

const Alice,Bob,Simon : Agent;

const Eve: Agent;
untrusted Eve;

compromised k(Eve,Simon);


        """
        if model == "Traces":
            args = "--max-runs=0"
        elif model == "MaxProc":
            args = "--max-runs=%i" % (runs)
        else:
            print "Don't know model %s." % (model)
        args += " --filter=yahalom,%s " % (claim)
        return (prot,args)


Protocol.register(yahalom_spdl())

if __name__ == "__main__":
    x = yahalom_spdl()
    (prot,args) =  x.generate("MaxProc")
    print prot
    print args

