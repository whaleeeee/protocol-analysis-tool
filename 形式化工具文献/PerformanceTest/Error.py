#
# Defines the errors in this test set.
#
import sys

warningsShown = []

class NoSplRunError(Exception):
    """ Raised when the a run is turned into spl code which does not have the appropriate type.

    Attributes:
        None.
    """

    def __str__(self):
        return "This run object cannot be converted into an SPL specification for Casper/FDR."
 
class WrongExit(Exception):
    """ Raised when a test command exists wrongly.

    Attributes:
        The exact return string.
    """

    def __str__(self):
        return "Wrong exit of command."

def printError(txt):
    """
    Print to stdErr

    Add newline to mimick print command.
    """
    sys.stderr.write(txt + '\n')

def showWarning(txt):
    """
    Write a warning to stderr.

    """
    printError("WARNING: " + txt)

def showWarningOnce(txt):
    """
    Show a textual warning once, but do not repeat it.
    """
    global warningsShown

    if txt not in warningsShown:
        warningsShown.append(txt)
        showWarning(txt)
        

    

