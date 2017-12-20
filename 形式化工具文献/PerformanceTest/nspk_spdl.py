#!/usr/bin/python

"""
Provides the spdl input for the Scyther tool for the NSPK as well as
Lowe's fixed version.

Author: Cas Cremers
"""

import Protocol

class nspk_spdl(Protocol.Protocol):

    def __init__(self,fixedversion=False):
        models = ["MaxProc","Traces"]
        claims = ["sAna","sAnb","sBna","sBnb","aA","aB"]
        self.fixedversion = fixedversion
        if self.fixedversion == False:
            name = "nspk"
        else:
            name = "fix_nspk"
        Protocol.Protocol.__init__(self,name,"scyther",models,claims)

    def roleList(self):
        return ["I","R"]

    def roleCount(self):
        return len(self.rolelist())

    def generate(self,model="MaxProc",claim="aA",runs=0):
        if self.fixedversion == True:
            fixstring = ",R"
        else:
            fixstring = ""
        prot = """
    /* 
     * Needham-Schroeder protocol
     */

    // PKI infrastructure

    const pk: Function;
    secret sk: Function;
    inversekeys (pk,sk);

    // The protocol description

    protocol ns3(I,R)
    {
        role I
        {
            const ni: Nonce;
            var nr: Nonce;

            send_1(I,R, {ni,I}pk(R) );
            read_2(R,I, {ni,nr%s}pk(I) );
            send_3(I,R, {nr}pk(R) );
    """ % (fixstring)
        prot += "\t\t"
        if claim == "sAna":
            prot += "claim_sAna(I,Secret,ni);"
        elif claim == "sAnb":
            prot += "claim_sAnb(I,Secret,nr);"
        elif claim == "aA":
            prot += "claim_aA(I,Nisynch);"
        prot += """
        }	
        
        role R
        {
            var ni: Nonce;
            const nr: Nonce;

            read_1(I,R, {ni,I}pk(R) );
            send_2(R,I, {ni,nr%s}pk(I) );
            read_3(I,R, {nr}pk(R) );
            """ % (fixstring)
        if claim == "sBna":
            prot += "claim_%s(R,Secret,ni);\n" % claim
        elif claim == "sBnb":
            prot += "claim_%s(R,Secret,nr);\n" % claim
        elif claim == "aB":
            prot += "claim_%s(R,Nisynch);\n" % claim
        prot += """
        }
    }

    // The agents in the system

    const Alice,Bob: Agent;

    // An untrusted agent, with leaked information

    const Eve: Agent;
    untrusted Eve;
    const ne: Nonce;
    compromised sk(Eve);
        """
        args = ""
        if model == "Traces":
            args = "--max-runs=0"
        elif model == "MaxProc":
            args = "--max-runs=%i" % (runs)
        else:
            print "Don't know model %s." % (model)
        return (prot,args)


Protocol.register(nspk_spdl(False))
Protocol.register(nspk_spdl(True))

if __name__ == "__main__":
    x = nspk_spdl(False)
    (prot,args) =  x.generate("MaxProc","aB",2)
    print prot
    print args

