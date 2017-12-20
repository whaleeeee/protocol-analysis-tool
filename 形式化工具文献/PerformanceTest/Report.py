"""
A simple reporting wrapper
"""
import sys

import TestConfig

toStdOut = True
toFile = None
defaultFile = None

def setFile(filename):
    """
    Set an output file.

    By default, disables stdout.
    """

    global toFile
    global toStdOut

    toFile = open(filename,"w")
    toStdOut = False


def report(obj="",addnewline=True):
    """
    Output for the tests.

    Emulates print by adding newline.
    Currently always writes to 
    """
    global toStdOut
    global toFile
    global defaultFile

    txt = "%s" % (obj)
    if addnewline:
        txt += "\n"
    if toStdOut:
        sys.stdout.write(txt)
    if toFile != None:
        toFile.write(txt)
        toFile.flush()  # Always immediately flush to see errors early

    """
    Always write to log
    """
    if defaultFile == None:
        filename = "test-%s.log" % (TestConfig.machineName())
        defaultFile = open(filename,"w")
    defaultFile.write(txt)
    defaultFile.flush()

def replog(txt="",addspace=True,addnewline=True):
    """
    Log output for the tests.
    """
    s = "*"
    if addspace:
        s += " "
    s += str(txt)
    report (s,addnewline)

def separator():
    """
    Split lines
    """
    replog("*" * 59,False)

