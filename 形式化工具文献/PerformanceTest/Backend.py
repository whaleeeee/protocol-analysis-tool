#!/usr/bin/python

import commands
import tempfile

import pexpect  # Needed for the timeout things

def doit(tc,cmd1,cmd2):
    """
    The filename will be sandwiched between the cmd's
    """
    global timeout

    tc.time = None

    fh = tempfile.NamedTemporaryFile()
    fname = fh.name
    fh.write(tc.protocol)
    fh.flush()

    cmd = cmd1 + fname + cmd2
    tc.command = cmd
    def onTimeout(d):
        # Caused by the timeout, I guess
        tc.time = tc.maxtime
        tc.timeout = True
        return True

    if tc.maxtime:
        tc.output = pexpect.run(cmd,timeout=tc.maxtime,events={pexpect.TIMEOUT:onTimeout})
    else:
        tc.output = pexpect.run(cmd)

def ofmc(tc):
    """
    0 attack
    1 maybe attack (maybe false attack)
    2 tool gave garbage
    3 maybe correct (within bounds no attack)
    4 correct
    """
    return avispa(tc,"--ofmc")

def satmc(tc):
    return avispa(tc,"--satmc")

def clatse(tc):
    return avispa(tc,"--cl-atse")

def ta4sp(tc):
    for i in range(0,5):
        res = avispa(tc,"--ta4sp --level=%i" % (i))
        # de-obfuscate TA4SP answers
        if res == 0:
            # Attacks might be false ones
            res = 1
        elif res == 3:
            # Proof is proof
            res = 4
        if (res == 1) or (res == 4):
            return res
    """ If not, we are stuck with the infamous inconclusive """
    return 2
        

def avispa(tc,mode):
    """
    Use any of the avispa tools
    """
    cmd1 = "avispa "
    cmd2 = " %s %s " % (mode, tc.args)
    doit(tc,cmd1,cmd2)

    if tc.timeout:
        return 2

    out = tc.output.splitlines()
    result = None
    timers = ["Computation","encodingTime","solvingTime"]
    for i in range(0,len(out)):
        if out[i].find("SUMMARY") != -1:
            result = out[i+1].strip()

        tm = out[i].strip().split()
        if len(tm) >= 2:
            for timer in timers:
                if tm[0].find(timer) != -1:
                    tc.addTime(float( tm[1] ))
            if tm[0] == "searchTime:":
                tc.addTime(float( tm[1][:-1] ))

    if tc.time == None:
        if result != "INCONCLUSIVE":
            txt = "Could not extract time data from output '%s'" % tc.output
            Error.showWarningOnce(txt)

    if result:
        if result == "SAFE":
            return 3
        elif result == "UNSAFE":
            return 0
        elif result == "INCONCLUSIVE":
            return 2

    # no results? we don't know then
    return 2
