#!/usr/bin/python

"""
    Configuration file for the tests to be run.
"""

# System dependencies
#import sys
import commands

# Other modules needed for testing
#import Protocol
#import TimeOut
#import Test
#import Scenario

GLOBALOPTIONS = {}

def filter(protocol,claim,tool,maxproc):
    """
    Returns true iff this test is allowed.
    """
    if tool not in getTools():
        return False
    if inGlobalOptions("tools"):
        go = getGlobalOptions()
        tl = go["tools"]
        #print tool, tl
        if tool not in tl:
            return False

    #if not claim.startswith("s"):
    #    return False

    #print protocol,claim,tool,maxproc

    return True


def getTimeout(protocol=None,claim=None,tool=None,maxproc=None):
    """
    Timeout value in seconds per protocol per claim test
    """
    timeout = 60 * 20
    return timeout

def machineName():
    """
    Get machine name
    """
    cmd = "uname -n"
    return commands.getoutput(cmd)

def getMinTime(toolname):
    """
    For each tool, set a default startup time. This is connected to the
    machine. The default is None, in which case it is computed.
    """
    return None

def getAvispaPath():
    """
    Returns a part of the avispa tool path that is unique to them. It
    will be used as a filter for pkill, so be very careful!

    The best would be to include the full avispa path, but that is not
    very portable.
    """
    return "avispa-1.1/bin/backends"


def ta4NextLevel(level=None,timed=True):
    """
    Determines ta4sp level traversal procedure.

    Initially it is called with None. If it returns None, ta4sp
    traversal stops and inconclusive is returned. If it returns
    something else (an integer) this is interpreted as the next level.
    """

    def avispaDefault(level,timed):
        """ [0]
        """
        if level == None:
            return 0
        else:
            return None

    def ourDefault(level,timed):
        """ timed: [0,1,2,3,4,5,6,7,...]
            not timed: [0,1,2,3,4]
        """
        if level == None:
            return 0
        else:
            if timed:
                return level + 1
            else:
                if level >= 5:
                    return None
                else:
                    return level + 1

    return ourDefault(level,timed)


def getMaxProc():

    """
    The smallest to largest MaxProc space explored, and step counter.

    Note that the minimum now must always be one, in order to have a
    correct value for tool.minTime during computations.
    """
    return (1,7,1)


def getProtocols():
    """
    Known:
      nspk (includes fix_nspk)
      tls
      eke
    """
    all = ["nspk","avispa_tls","eke"]
    return all
    #return ["nspk","eke"]
    #return ["nspk"]


def getTools():
    """
    Known:

      ta4sp
      ofmc
      satmc
      cl-atse
      scyther
      proverif
      casperfdr
    """
    all = ["ta4sp","cl-atse","satmc","ofmc","proverif","scyther","casperfdr"]
    return all

def setGlobalOptions(go={}):
    """
    Set the dict of global options
    """
    global GLOBALOPTIONS

    GLOBALOPTIONS = go

def setGlobalOption(key,val):
    """
    Set a dict entry in global options
    """
    global GLOBALOPTIONS

    GLOBALOPTIONS[key] = val

def getGlobalOptions():
    """
    Return the dict of global options
    """
    global GLOBALOPTIONS

    return GLOBALOPTIONS

def inGlobalOptions(opt):
    """
    Returns true iff opt in global options
    """
    global GLOBALOPTIONS

    return (opt in GLOBALOPTIONS.keys())

def addGlobalOption(opt):
    """
    Add another option
    """
    global GLOBALOPTIONS

    if not inGlobalOptions(opt):
        GLOBALOPTIONS[opt] = True

