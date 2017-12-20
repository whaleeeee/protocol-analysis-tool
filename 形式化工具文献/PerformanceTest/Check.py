#!/usr/bin/python

"""
Check whether all requirements exist for running the test set
"""

try:
    import pexpect
except:
    raise Except, "Could not find Python Pexpect module. We need that for running the tests, so please get it from http://pexpect.sourceforge.net"
import commands

# Import test components
import Error

def findProgram(toolname,name,required=False):
    """
    Check whether the program exists.
    Returns the full path if so.
    If not, and it is required, we generate an error, otherwise return
    None
    """
    # Exploit the nice 'which' of the pexpect module (although we really
    # need pexpect for the timeouts of the tests)
    out = pexpect.which(name)
    if out:
        return out
    else:
        if required:
            raise Exception, ("Command %s not found" % name)
        else:
            Error.showWarningOnce("Cannot find %s program needed for the %s tool." % (name,toolname))
            return None

def check():
    global AVISPA,HLPSL2IF
    global SCYTHER,CPP
    global PROVERIF
    global CASPER,FDR

    # Avispa
    AVISPA  = findProgram("Avispa","avispa")
    HLPSL2IF = findProgram("Avispa","hlpsl2if")
    # Casper/FDR
    CASPER  = findProgram("CasperFDR","./casper.sh")
    FDR     = findProgram("CasperFDR","./fdr2.sh")
    # Scyther
    SCYTHER = findProgram("Scyther","scyther-linux")
    CPP     = findProgram("Scyther","cpp")
    # ProVerif
    PROVERIF = findProgram("ProVerif","analyzer")

check()

