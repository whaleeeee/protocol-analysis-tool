#!/usr/bin/python

# System dependencies
import sys
import time
import getopt

# Other modules needed for testing
import Protocol
import Test
import Scenario
import TestConfig
import Error
import Report

# The imported protocol generators
protocols = TestConfig.getProtocols()
if "nspk" in protocols:
    import nspk_spdl
    import nspk_hlpsl
    import nspk_pi
    import nspk_spl
if "avispa_tls" in protocols:
    import avispa_tls_spdl
    import avispa_tls_hlpsl
    import avispa_tls_pi
if "eke" in protocols:
    import eke_spdl
    import eke_hlpsl
    import eke_pi
    import eke_spl


def gatherClaims():
    prots = {}
    for p in Protocol.protocols:
        if not p.name in prots.keys():
            prots[p.name] = []
        for c in p.claims:
            if not c in prots[p.name]:
                prots[p.name].append(c)
    for k in prots.keys():
        prots[k].sort()
    return prots

def getAgents(c):
    if c.startswith("a"):
        # Authentication
        agents = 2
    elif c.startswith("s"):
        # Secrecy
        agents = 1
    else:
        # What is it?
        raise Exception, ("Unknown claim type %s" % (c))
    return agents

def verify(p,c,maxproc=2,timeout=None):

    for m in p.models:
        t = None
        comment = "MaxProc%i" % maxproc
        if m == "Traces":
            t = Test.Test(p.name,c,p.toolname,m,timeout=timeout,comment=comment)
        elif m == "MaxProc":
            t = Test.Test(p.name,c,p.toolname,m,maxproc,timeout=timeout,comment=comment)
        elif m == "Scen":
            agents = getAgents(c)
            sl = Scenario.ScenarioSet(p.roleCount(),maxproc,fill=True,agentcount=agents).list
            t = Test.Test(p.name,c,p.toolname,m,sl,timeout=timeout,comment=comment)
        elif m == "RepScen":
            agents = getAgents(c)
            ss = Scenario.ScenarioSet(p.roleCount(),1,fill=True,agentcount=agents)
            ss.cover()
            sl = ss.list
            t = Test.Test(p.name,c,p.toolname,m,sl,timeout=timeout,comment=comment)
        if t:
            Report.report(t)

def getProtocols(pname,c):
    pl = []
    for p in Protocol.protocols:
        if (p.name == pname) and (c in p.claims):
            pl.append(p)
    return pl

def doProtocol(prots,pn,fclaim=None,maxproc=2):
    cl = prots[pn]
    cl.sort()
    cl.reverse()        # to start with secrecy (smaller scenarios than auth, hence improving tool.minTime initial behaviour)
    for c in cl:
        if (fclaim == None) or c.startswith(fclaim):
            Report.replog("** Claim: %s,%s ***" % (pn,c), False)
            pl = getProtocols(pn,c)
            for p in pl:
                # Apply filtering
                if TestConfig.filter(pn,c,p.toolname,maxproc):
                    # Get timeout value from config
                    timeout = TestConfig.getTimeout(pn,c,p.toolname,maxproc)
                    verify(p,c,maxproc=maxproc,timeout=timeout)

def usage():
    (runmin,runmax,runstep) = TestConfig.getMaxProc()
    print """
------------------------------------------------------------------------
performanceTest.py

Security protocol analyzers performance test scripts.
By Cas Cremers & Pascal Lafourcade

--protocol=P        Filter on protocol P
--claims=S          Filter on claim prefix S
--tools=T1[,T2]     Filter on tools T1..TN

-o,--output=F       Send output to file F

--run-max=I         Maximum number of runs I [%i]
--run-step=I        Step number for runs I [%i]

--output-if         Write any generated IF language constructs to a
                    file.
------------------------------------------------------------------------
""" % (runmax,runstep)

def main():
    try:
        opts, args = getopt.getopt(sys.argv[1:], "ho:", ["help",
            "protocol=","claims=","run-max=","run-step=",
            "output=","output-if","tools="])
    except:
        print "Could not parse arguments."
        usage()
        sys.exit(1)
    if args != []:
        print "Superflous arguments %s not recognized." % args
        usage()
        sys.exit(1)

    # Init
    Report.replog("Scanning main")
    prots = gatherClaims()
    Report.replog(repr(prots)+"\n")

    # Parse options
    (runmin,runmax,runstep) = TestConfig.getMaxProc()
    filter = None
    fclaim = None
    TestConfig.setGlobalOptions()   # Init global dict
    for o,a in opts:
        if o in ("-h","--help"):
            usage()
            sys.exit()
        elif o == "--protocol":
            filter = a
        elif o == "--claims":
            fclaim = a
        elif o == "--tools":
            go = TestConfig.setGlobalOption("tools",a.split(","))
        elif o == "--run-max":
            runmax = int(a)
            if runmax < runmin:
                runmin = runmax
        elif o == "--run-step":
            runstep = int(a)
            if runstep <= 0:
                assert False, "Run step number must be more than 0"
        elif o in ["-o","--output"]:
            Report.setFile(a)
        elif o == "--output-if":
            TestConfig.addGlobalOption(o)
        else:
            uo = "Unhandled option '%s'" % o
            assert False, uo

    proc = runmin
    while proc <= runmax:
        to = TestConfig.getTimeout(maxproc=proc)
        Report.separator()
        Report.replog("Testing performance for MaxProc(%i) from range (%i,%i,%i)" % (proc,runmin,runmax,runstep ))
        Report.replog("Using timeout of %i seconds" % (to))
        Report.replog("Current time and date: %s" % (time.asctime()))
        Report.separator()

        if filter in prots.keys():
            doProtocol(prots,filter,fclaim,maxproc=proc)
        else:
            for pn in prots.keys():
                doProtocol(prots,pn,maxproc=proc)
        proc += runstep


if __name__ == "__main__":
    main()

