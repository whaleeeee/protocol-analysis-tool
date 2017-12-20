"""
Simply Python constants for result values
"""

"""
In case of a combination, the smallest is considered (for 'and'
operations). Therefore we define TIMEOUT < INCONCLUSIVE, as a timeout
might influence the result if we have not attacks.
"""
NOTHING = 0
ATTACK = 1
UNSAFE = 2
TIMEOUT = 3
INCONCLUSIVE = 4    # Excludes timeouts
SAFE = 5
PROOF = 6

claimtextmap = {
    NOTHING:"________",
    ATTACK:"attack_!",
    UNSAFE:"attack_?",
    TIMEOUT:"timeout!",
    INCONCLUSIVE:"inconcl.",
    SAFE:"correct?",
    PROOF:"correct!",
}

def resultString(res):
    """
    Turn a result code into a string
    """
    global claimtextmap

    if res in claimtextmap:
        return claimtextmap[res]
    else:
        return claimtextmap[0]


def stringResult(str):
    """
    Turn a result string back into a code
    """
    global claimtextmap

    for res in claimtextmap.keys():
        if claimtextmap[res] == str:
            return res
    return NOTHING

