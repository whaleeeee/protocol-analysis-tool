#!/usr/bin/python

"""

Perform a real test.

"""

import sys
import time

import Protocol
import Tool
import Error

from Result import *


maxmodellen = 0
maxtoollen = 0
maxparamlen = 0


class Test(object):

    """
    Test object

    Upon creation, the test will be performed if possible.
    """

    def __init__(self,protocol,claim,tool,model,parameter=None,timeout=-1,subcase=None,comment=None):

        global maxtoollen

        self.verified = False
        self.error = None
        self.result = INCONCLUSIVE

        self.protocol = protocol
        self.protgen = None
        self.claim = claim
        self.toolname = tool
        if len(tool) > maxtoollen:
            maxtoollen = len(tool)
        self.model = model
        self.parameter = parameter
        self.subcase = subcase
        self.timeout = timeout
        self.timeleft = timeout
        self.starttime = time.time()
        self.comment = comment

        found = False
        for p in Protocol.protocols:
            if (p.name == protocol) and (p.toolname == tool):
                if p.tool:
                    found = True
                    self.protgen = p
                    self.tool = p.tool
                    break

        if not found:
            self.error = "Could not find protocol %s for tool %s" % (protocol,tool)
            self.parameter = ""
            self.duration = None
            self.durationWall = None
            self.memory = None
            return

        if not claim in p.claims:
            self.error = "Could not find claim %s in %s" % (claim,protocol)
            return

        if not model in p.models:
            """
            Some exceptions exists that can be simulated
            """
            if (model == "MaxProc") and ("Scen" in p.models):
                # Simulate it TODO, special case function needs to be
                # called
                pass
            else:
                self.error = "Cannot verify protocol in model %s" % model
                return
        self.verify()


    def __repr__(self):

        global maxtoollen,maxmodellen,maxparamlen

        s = "%-*s  %-*s" % (Protocol.maxnamelen,self.protocol,Protocol.maxclaimlen,self.claim)

        s += "  %s" % self.humanResult(self.verified)

        if len(self.toolname) > maxtoollen:
            maxtoollen = len(self.toolname)
        s += "  %-*s" % (maxtoollen,self.toolname)

        if len(self.model) > maxmodellen:
            maxmodellen = len(self.model)
        s += "  %-*s" % (maxmodellen,self.model)

        if self.parameter:
            ps = str(self.parameter)
            if len(ps) > maxparamlen:
                maxparamlen = len(ps)
                filler = ""
            else:
                filler = " " * (maxparamlen - len(ps))
            s += "  (%s)%s" % (ps,filler)
        else:
            s += "  ()%s" % (" " * maxparamlen)

        if self.comment:
            s += "  {%s}" % self.comment
        else:
            s += "  {}"
        if not self.verified:
            s += "\t[%s]" % self.error

        if self.duration:
            s += "\t<%.2fs>" % self.duration
        else:
            s += "\t<0s>"

        """ TODO misc information """
        if self.durationWall:
            s += "\t<%.2fs>" % self.durationWall
        else:
            s += "\t<0s>"
        if self.memory:
            s += "\t<%sKB>" % self.memory
        else:
            s += "\t<?KB>"

        return s

    def humanResult(self,verified=True):
        if verified == True:
            x = self.result
        else:
            x = -1
        return resultString(x)


    def verify(self):
        """
        If the parameter is a list,
        we unfold it and combine the results
        """
        if type(self.parameter)==list:
            self.result = self.verifyList()
        else:
            self.result = self.verifyOne()


    def verifyOne(self,subtractor=0):
        """
        Returns a result answer as defined in Result.py
        """
        self.duration = 0
        self.durationWall = 0
        self.memory = 0

        if self.timeleft == 0:
            return TIMEOUT

        # Determine input
        try:
            (prot,args) = self.protgen.generate(self.model,self.claim,self.parameter)
        except Error.NoSplRunError:

            self.duration = 0
            self.durationWall = 0
            self.memory = 0

            self.verified = True
            self.prot = ""
            self.args = ""
            return PROOF

        self.prot = prot
        self.args = args

        # Go and verify that if time allows
        self.tool.set_generator(self.protgen,self.model,self.claim,self.parameter)
        result = self.tool.verify(prot,args,self.timeleft)

        # Compute the actual time.
        # In case of multiple tests, subtract the subtractor (>= 0).
        self.duration = self.tool.duration - subtractor
        if (self.duration < 0):
            # If the time is less than the subtractor, we effectively
            # have an update for the subtractor for the next round, but
            # for now we measure 0 time.
            self.duration = 0

        # Propagate wall time and memory use
        self.durationWall = self.tool.durationWall
        self.memory = self.tool.memory

        # Compute what's left
        if self.timeleft != -1:
            if self.timeleft > self.duration:
                self.timeleft -= self.duration
            else:
                self.timeleft = 0
        self.verified = True
        return result


    def verifyList(self):
        """
        Unfolds a list and combines the results
        """

        parlist = self.parameter

        totalDur = 0
        totalDurWall = 0
        maxMem = 0
        firstSub = True

        results = []
        for par in parlist:

            self.parameter = par
            """
            Subtract forking time if not the first.
            """
            if firstSub:
                firstSub = False
                subs = 0
            else:
                subs = self.tool.minTime

            result = self.verifyOne(subtractor=subs)
            results.append(result)

            """
            Update global counters
            """
            totalDur += self.duration
            totalDurWall += self.durationWall
            if self.memory > maxMem:
                maxMem = self.memory

            """
            For attacks (the minimum element) we can abort prematurely
            """
            if result == ATTACK:
                break

        """
        New computation for duration.
        """
        self.duration = totalDur
        self.durationWall = totalDurWall
        self.memory = maxMem

        if len(results) == 0:
            return INCONCLUSIVE
        else:
            return min(results)


