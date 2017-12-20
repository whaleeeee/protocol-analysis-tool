#!/usr/bin/python

# Sys imports
import os
import commands
import tempfile
import time
import sys

import pexpect  # Needed for the timeout things

# Test script components
from misc import parseClock
from Result import *
import Check
import TestConfig
import Error
import Report

tools = []
hlpsltools = []

class Tool(object):

    def __init__(self,name,prefix="",postfix=""):

        self.name = name
        self.protocols = []
        self.extension = ""
        self.protgen = None
        self.commentPrefix = prefix
        self.commentPostfix = postfix
        self.minTime = TestConfig.getMinTime(self.name)

    def set_generator(self,protgen,model,claim,parameter):

        self.protgen = protgen
        self.model = model
        self.claim = claim
        self.parameter = parameter

    def register(self,protgen):
        if not protgen in self.protocols:
            self.protocols.append(protgen)

    def verify(self,prot,args,timeout=-1):
        """
        Input is a protocol, extra args, and timeout, output should be a result
        """
        
        self.prot = prot
        self.args = args

        self.preprocess()

        self.duration = 0

        fh = tempfile.NamedTemporaryFile(suffix=self.extension)
        fname = fh.name
        fh.write(self.prot)
        fh.flush()

        """ Make a timer file """
        fhtime = tempfile.NamedTemporaryFile(suffix=".timer")
        fhtimename = fhtime.name
        timer = "time --quiet -f \"\\n"
        timer += "@@%U\\n"
        timer += "@@%S\\n"
        timer += "@@%M\\n"
        timer += "@@%E\\n"
        timer += "\" --output " + fhtimename + " --append "

        subtool = self.toolcommand(fname)
        cmd = timer + subtool
        self.command = cmd

        def onTimeout(d):
            # Caused by the timeout, I guess
            self.timeoutflag = True
            return True

        self.starttime = time.time()
        self.timeoutflag = False

        if timeout != -1:
            self.output = pexpect.run(cmd,timeout=timeout,events={pexpect.TIMEOUT:onTimeout})
        else:
            self.output = pexpect.run(cmd)

        """ Compute real used time (user+sys) """
        timeres = []
        log = []
        for l in fhtime.readlines():
            cl = l.strip()
            log.append(cl)
            if cl.startswith("@@"):
                timeres.append(cl[2:])
        if len(timeres) >= 4:
            try:
                timeUser = float(timeres[0])        # seconds..
            except:
                wrongexitstring = "Command exited with non-zero status"
                if timeres[0].startswith(wrongexitstring):
                    self.reportError(timeres[0])
                    raise Error.WrongExit
                print cmd
                print timeres
                assert False, "Could not parse time from '%s' of %s." % (timeres[0], self.name)
                sys.exit(1)
            timeSys = float(timeres[1])         # seconds..
            timeMem = timeres[2]                # In KiloBytes
            timeClock = parseClock(timeres[3])  # hours:minutes:seconds.xx, converted to seconds

            """ Old computation """
            #self.duration = time.time() - self.starttime
            measurement = timeUser + timeSys    # time for measurements

            self.duration = measurement

            self.durationWall = timeClock       # time for comparing
            self.memory = timeMem               # max memory usage

            """ Record minimal time for this tool """
            if TestConfig.getMinTime(self.name) == None:
                changed = False
                if self.minTime == None:
                    changed = True
                else:
                    if measurement < self.minTime:
                        changed = True
                if changed == True:
                    self.minTime = measurement
                    rst = "For tool %s, minimal time detected on %s: %gs" \
                            % (self.name,TestConfig.machineName(),self.minTime)
                    Report.replog(rst)

        else:
            """
            No time output is caused only by external killing of the
            subprocess by the timeout.
            """
            if not self.timeoutflag:

                # TODO abort for other reason?
                assert False, "Timeout flag not set, but could not parse time output from\n%s" \
                        % (log)
            self.timeoutflag = True
            
        fhtime.close()

        if self.timeoutflag == True:
            self.result = TIMEOUT
            self.duration = timeout
            self.durationWall = timeout
            self.memory = 0
        else:
            self.result = self.analyse()

        self.cleanUp()
        self.detectError()
        return self.result

    def analyse(self):
        """ returns whatever happens """
        return  self.extractResult(self.output)

    def __repr__(self):
        return "<%s>" % self.name

    def preprocess(self):
        """
        Maybe preprocess self.prot and self.args

        Would be the hook to convert hlpsl to if :)
        """
        return

    def ewrite(self,ext,data):
        fn = "error.%s" % (ext)
        print "Generating error file %s" % (fn)
        fh = open(fn,'w')
        fh.write(data)
        fh.close()

    def detectError(self):
        if self.output.find("ERROR") != -1:
            self.reportError()

    def reportError(self,txt=""):
        print "ERROR %s for %s" % (txt,self.name)
        self.ewrite("input",self.prot)
        self.ewrite("output",self.output)
        self.ewrite("command",self.command)
        raise Exception, "Tool error: report in files"

    def cleanUp(self):
        pass

class CasperFDR(Tool):

    def __init__(self):

        Tool.__init__(self,"casperfdr","-- ")
        self.extension = ".spl"

    def toolcommand(self,filename):
        cmd = "./fdr2.sh %s %s" % (filename,self.args)
        return cmd

    def extractResult(self,txt):
        out = txt.splitlines()

        i = len(out)-1
        last = ""
        while (i > 0) and (last == ""):
            last = out[i].strip()
            i = i - 1

        if last == "xfalse":
            return ATTACK
        elif last == "true":
            return SAFE
        elif last == "Killed":
            return INCONCLUSIVE
        else:
            print out
            estr = "Don't know how to interpret FDR's [%s]" % (last)
            raise Exception, estr

    def preprocess(self):
        """
        Maybe preprocess self.prot and self.args

        Hook to convert spl to csp
        """
        self.spl = self.prot

        fh = tempfile.NamedTemporaryFile(suffix=".spl")
        fout = tempfile.NamedTemporaryFile(suffix=".csp")
        fname = fh.name
        fh.write(self.spl)
        fh.flush()
        foutname = fout.name

        cmd = "./casper.sh %s %s" % (fname[:-4],foutname)
        pexpect.run(cmd)
        protlines = fout.readlines()
        fout.close()
        fh.close()

        #if protlines.find("error") != -1:
        #    print "ERROR for mycasper.sh"
        #    self.ewrite("spl",self.spl)
        #    self.ewrite("output",protlines)
        #    raise Exception, "Tool error: report in files"

        # Stupid parsers trip over DOS-like endings. I mean, really.
        s = ""
        for l in protlines:
            s += "%s\n" % l
        self.prot = s

    def reportError(self,txt=""):
        print "ERROR %s for %s" % (txt,self.name)
        self.ewrite("spl",self.spl)
        self.ewrite("csp",self.prot)
        self.ewrite("output",self.output)
        raise Exception, "Tool error: report in files"

    def cleanUp(self):
        """
        FDR can remain in memory and screw up the other tests.
        """
        fdrcmd = "fdr2tix"
        cmd = "pkill -9 -f %s" % (fdrcmd)
        commands.getoutput(cmd)

class Avispa(Tool):

    def __init__(self,name,makeIF=None):
        global hlpsltools

        if makeIF:
            self.makeIF = makeIF
        else:
            self.makeIF = False

        if self not in hlpsltools:
            hlpsltools.append(self)
        Tool.__init__(self,name,"% ")


        # Correct extension (although avispa does not complain for hlpsl,
        # only for if)
        if self.makeIF == True:
            self.extension = ".if"
        else:
            self.extension = ".spdl"

    def toolcommand(self,filename):
        if self.makeIF:
            cmd = "avispa %s --no-hlpsl2if --%s %s" % (filename,self.name,self.args)
        else:
            cmd = "avispa %s --%s %s" % (filename,self.name,self.args)
        return cmd

    def extractResult(self,txt):
        out = txt.splitlines()
        for i in range(0,len(out)):
            if out[i].find("SUMMARY") != -1:
                result = out[i+1].strip()
                return self.interpretResult(result)
            if out[i].find("error") != -1:
                self.reportError("'error' in output")
        Error.showWarningOnce("Could not extract result from '%s'" % out)
        return INCONCLUSIVE

    def analyse(self):
        ## Below is ancient timer extraction, which could not be trusted anyway
        #timers = ["Computation","encodingTime","solvingTime"]
        #out = self.output.splitlines()
        #for i in range(0,len(out)):

        #    tm = out[i].strip().split()
        #    if len(tm) >= 2:

        #        for timer in timers:

        #            if tm[0].find(timer) != -1:
        #                self.addTime(float( tm[1] ))

        #        if tm[0] == "searchTime:":

        #            self.addTime(float( tm[1][:-1] ))

        return Tool.analyse(self)

    def interpretResult(self,word):
        if word == "SAFE":
            return SAFE
        elif word == "UNSAFE":
            return ATTACK
        else:
            return INCONCLUSIVE

    def preprocess(self):
        """
        Maybe preprocess self.prot and self.args

        Hook to convert hlpsl to if :)
        """
        if self.makeIF == True:
            self.hlpsl = self.prot

            fh = tempfile.NamedTemporaryFile(suffix=".hlpsl")
            fname = fh.name
            fh.write(self.hlpsl)
            fh.flush()

            cmd = "hlpsl2if --nowarnings --stdout %s" % (fname)
            self.prot = pexpect.run(cmd)
            fh.close()
            if self.prot.find("ERROR") != -1:
                print "ERROR for hlpsl2if"
                self.ewrite("hlpsl",self.hlpsl)
                self.ewrite("output",self.prot)
                raise Exception, "Tool error: report in files"

            # Stupid parsers trip over DOS-like endings. I mean, really.
            pl = self.prot.splitlines()
            s = ""
            for l in pl:
                s += "%s\n" % l
            self.prot = s

            # If needed, write out 
            if TestConfig.inGlobalOptions('--output-if'):
                ifname = "%s_%s.if" % (self.protgen.name,self.claim)
                fh = open(ifname, 'w')
                fh.write(self.prot)
                fh.close()

    def reportError(self,txt=""):
        print "ERROR %s for %s" % (txt,self.name)
        if self.makeIF == True:
            self.ewrite("hlpsl",self.hlpsl)
            self.ewrite("if",self.prot)
        else:
            self.ewrite("hlpsl",self.prot)
        self.ewrite("output",self.output)
        raise Exception, "Tool error: report in files"

    def cleanUp(self):
        """
        Some AVISPA tools (cl-atse,ofmc) might remains in memory, ugly, and it might destroy
        the other tests.
        """
        
        """
        TODO potentially very unsafe, if you put Avispa in say /usr/bin.
        The assumption here is that AvispaPath will include "avispa"
        somewhere" to limit the damage.
        """
        avpath = TestConfig.getAvispaPath()
        cmd = "pkill -9 -f %s" % (avpath)
        commands.getoutput(cmd)

        """
        Second, some seem to leave a mess of files (Sat-MC?)
        """
        def extclean(path):
            cmd = "rm -f %stmp*.sate" % path
            commands.getoutput(cmd)
            cmd = "rm -f %stmp*.res" % path
            commands.getoutput(cmd)
            cmd = "rm -f %stmp*dimacs*" % path
            commands.getoutput(cmd)

        extclean("")
        try:
            avispabase = os.environ["AVISPA_PACKAGE"]
            extclean("%s/testsuite/results/" % avispabase)
        except:
            pass

class Ofmc(Avispa):

    def __init__(self,makeIF=None):
        Avispa.__init__(self,"ofmc",makeIF)

class Satmc(Avispa):

    def __init__(self,makeIF=None):
        Avispa.__init__(self,"satmc",makeIF)

    #def toolcommand(self,filename):
    #    cmd = Avispa.toolcommand(self,filename)
    #    cmd += " --output_dir=/tmp/"
    #    return cmd

    def cleanUp(self):
        """
        SATMC sometimes remains in memory, ugly, and it might destroy
        the other tests.
        """
        cmd = "killall satmc"
        commands.getoutput(cmd)
        Avispa.cleanUp(self)

class Clatse(Avispa):

    def __init__(self,makeIF=None):
        Avispa.__init__(self,"cl-atse",makeIF)

class Ta4sp(Avispa):

    def __init__(self,makeIF=None):
        self.level = 0
        Avispa.__init__(self,"ta4sp",makeIF)

    def register(self,protgen):
        cl = []
        for c in protgen.claims:
            if c.startswith("s"):
                cl.append(c)

        protgen.claims = cl
        Avispa.register(self,protgen)

    def toolcommand(self,filename):
        s = Avispa.toolcommand(self,filename)
        s += " --level=%i" % (self.level)
        return s

    def verify(self,prot,args,timeout=-1):
        self.level = None

        timed = False
        if timeout:
            if timeout >= 0:
                timed = True

        while True:

            self.level = TestConfig.ta4NextLevel(self.level,timed)
            if self.level == None:
                return INCONCLUSIVE
            else:
                res = Avispa.verify(self,prot,args,timeout)
                if timeout > self.duration:
                    timeout = timeout - self.duration
                if res != INCONCLUSIVE:
                    return res


    def interpretResult(self,word):
        if word == "SAFE":
            return PROOF
        elif word == "UNSAFE":
            return UNSAFE
        else:
            return INCONCLUSIVE

class Scyther(Tool):

    def __init__(self):
        Tool.__init__(self,"scyther","// ")
        self.extension = ".spdl"

    def toolcommand(self,filename):
        return "scyther-linux --plain %s %s" % (self.args,filename)

    def analyse(self):
        """ 
        If correct, there should be only one claim output here
        """
        out = self.output.splitlines()
        for l in out:
            if l.find("Fail") != -1:
                return ATTACK
            elif l.find("Ok") != -1:
                if l.find("proof") != -1:
                    return PROOF
                else:
                    return SAFE
        return INCONCLUSIVE

class Proverif(Tool):

    def __init__(self):
        Tool.__init__(self,"proverif","(* "," *)")
        self.extension = ".pi"

    def toolcommand(self,filename):
        return "analyzer -in pi %s" % (filename)

    def analyse(self):
        """
        Parse Proverif output. Hmm.
        """
        out = self.output.splitlines()
        for l in out:
            l = l.strip()
            if l.find("RESULT") != -1:
                if l.endswith("true."):
                    return PROOF
                elif l.endswith("false."):
                    return ATTACK
                elif l.endswith("proven."):
                    return INCONCLUSIVE

        return INCONCLUSIVE

def initTools():
    global tools

    if Check.AVISPA:
        # Avispa tools take True to pre-generate IF
        # This shaves off quite a percentage for the faster tools
        if Check.HLPSL2IF:
            genif = True
        else:
            genif = False

        tools.append(Ofmc(genif))
        tools.append(Satmc(genif))
        tools.append(Clatse(genif))
        tools.append(Ta4sp(genif))

    if Check.SCYTHER:
        tools.append(Scyther())

    if Check.PROVERIF:
        tools.append(Proverif())

    if Check.CASPER and Check.FDR:
        tools.append(CasperFDR())

def registerToolGen(tool,protgen):
    tool.register(protgen)
    protgen.tool = tool

def register(toolname,protgen):
    global tools

    for t in tools:
        if t.name == toolname:
            registerToolGen(t,protgen)
            return
    protgen.tool = None
    Error.showWarningOnce("Could not find tool %s" % toolname)

def getTools():
    global tools

    return tools

"""
Global code, inits tools
"""

initTools()

tl = []
for t in tools:
    tl.append(t.name)
Report.replog("Registered tools: %s" % tl)

"""
Invocation on command-line?
"""

if __name__ == "__main__":
    #global tools

    ## It will be printed anyway...
    #for t in tools:
    #    print t.name
    pass

