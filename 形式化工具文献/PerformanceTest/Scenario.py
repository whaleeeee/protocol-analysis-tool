#!/usr/bin/python


"""

Procedure to generate a list of scenarios, or to count them

"""

from misc import *
import Error

scenarioListCache = {}


class Run(object):

    """
    A run, or thread, or process definition

    A run is essentially a list of |roles|+1 elements.
    The first denotes the role (0..roles-1), the second the actor
    agent (0 or 1) and the others the agents for the other roles
    (0,1,2 where 2 denotes the intruder)
    """

    def __init__(self,roles,default=None,agentcount=2):
        """
        Init object. Needs at least the number of roles.
        """

        self.rolecount = roles
        self.agentcount = agentcount
        if not default:
            self.setZero()
        else:
            self.list = default
            self.check()
    
    def copy(self):
        nl = []
        for i in range(0,self.rolecount+1):
            nl.append(self.list[i])
        return Run(self.rolecount,nl,self.agentcount)

    def setZero(self):
            self.list = []
            for i in range(0,self.rolecount+1):
                self.list.append(0)

    def imax(self,i):
        if i == 0:
            return self.rolecount
        elif i == 1:
            return self.agentcount
        else:
            return self.agentcount+1

    def check(self):
        if len(self.list) != (self.rolecount+1):
            raise Exception, "Run object list does not correspond to rolecount+1"
        for pos in range(0,self.rolecount+1):
            if self.list[pos] < 0:
                raise Exception, "Run object inconsistency"
            elif self.list[pos] >= self.imax(pos):
                raise Exception, "Run object inconsistency"

    def role(self):
        return self.list[0]

    def rolename(self):
        if self.rolecount <= 3:
            return ['I','R','S'][self.role()]
        else:
            return "R%i" % (self.role()+1)

    def roletoindex(self,i):
        """ 
        convert the role position i to an index in the list. This is needed
        because the actor is now always at position 1, which does not 
        correspond neatly to the normal syntax.
        """
        if i < self.role():
            return i + 2
        elif i > self.role():
            return i + 1
        else:
            return 1

    def agent(self,i=0):
        return self.list[self.roletoindex(i)]

    def agentname(self,i=0):
        if self.agent(i) == self.agentcount:
            return 'E'
        else:
            deflist = ['A','B','C','D']
            return deflist[self.agent(i)]

    def increase(self):

        pos = self.rolecount
        while pos >= 0:
            self.list[pos] = self.list[pos] + 1
            if self.list[pos] < self.imax(pos):
                return True

            #overflow on this position
            self.list[pos] = 0
            pos = pos - 1
        return False

    def spdl(self):
        s = self.rolename() + "("
        for i in range(0,self.rolecount):
            if i > 0:
                s += ","
            s += self.agentname(i)
        return s + ")"

    def spl(self,rolenames,ntext=""):
        """
        Raises "noSplRun" if this is an illegal run for the spl spec.
        """
        for i in range(1,self.rolecount):
            if self.list[i+1] > 0:
                raise Error.NoSplRunError

        s = ""
        s += rolenames[self.role()] + "("
        s += self.agentname(0)
        s += ntext
        s += ")"
        return s

    def hlpsl(self,rolenames=None,prefix="",postfix="",symkey=None):
        s = ""
        if rolenames:
            # Provided by protocol generator
            s += rolenames[self.role()] + "("
        else:
            # Default names = prole1, prole2, ...
            s += "prole%i (" % (self.role()+1)

        honestagents = ['a','b','c']
        agentlist = honestagents[:self.agentcount] + ['i']
        for i in range(0,self.rolecount):
            s += agentlist[self.agent(i)]
            s += ","
        s += prefix
        if symkey:
            al = []
            for i in range(0,self.rolecount):
                al.append(agentlist[self.agent(i)])
            s += symkey(self.role(),al)
        else:
            for i in range(0,self.rolecount):
                s += "k"
                s += agentlist[self.agent(i)]
                if i < (self.rolecount-1):
                    s += ","
        s += postfix
        s += ")"
        return s

    def __str__(self):
        return self.spdl()

    def __repr__(self):
        return str(self)

    def inverted(self):
        newrun = self.copy()
        if self.agentcount == 2:
            for i in range(1,len(newrun.list)):
                x = newrun.list[i]
                if x == 0:
                    newrun.list[i] = 1
                elif x == 1:
                    newrun.list[i] = 0
        return newrun

    
    def __cmp__(self,other):
        if self.rolecount < other.rolecount:
            return -1
        elif self.rolecount > other.rolecount:
            return 1
        else:
            for i in range(0,self.rolecount+1):
                if self.list[i] < other.list[i]:
                    return -1
                elif self.list[i] > other.list[i]:
                    return 1
            return 0


class Session(list):

    def __str__(self):
        s = "SES["
        first = True
        for i in range(0,len(self)):
            if not first:
                s += ","
            s += str(self[i])
            first = False
        s += "]"
        return s


class Scenario(list):

    """
    The Scenario object is a list (collection) of Run objects
    """

    def __init__(self,roles,runs,dlist=[]):
        self.rolecount = roles
        self.runcount = runs
        list.__init__(self,dlist)
    
    def spdl(self):

        s = "["
        for x in self:
            if len(s) > 1:
                s += ","
            s += str(x)
        return s + "]"

    def __str__(self):

        return self.spdl()

    def copy(self):
        ns = Scenario(self.rolecount,self.runcount)
        for x in self:
            ns.append(x)
        return ns

    def inverted(self):
        inv = self.copy()
        for i in range(0,len(inv)):
            inv[i] = inv[i].inverted()
        inv.sort()
        return inv

    def __cmp__(self,other):
        """
        Assumes the runs in the scenarios are sorted
        """
        return self.mycmp(self,other)

    def mycmp(self,s1,s2):
        if len(s1) < len(s2):
            return -1
        elif len(s1) > len(s2):
            return 1
        else:
            for i in len(s1):
                r = s1[i].__cmp__(s2[i])
                if r != 0:
                    return r
            return 0

    def hlpsl(self,rolenames=None,prefix="",postfix="",symkey=None):
        s = "\n"
        first = True
        for run in self:
            s += "       "
            if first:
                s += "   "
                first = False
            else:
                s += "/\ "
            s += run.hlpsl(rolenames,prefix=prefix,postfix=postfix,symkey=symkey)
            s += "\n"
        return s

    def spl(self,rolenames=None,narr=[],insertpks=False):
        s = ""
        i = 0
        initkey = 1
        for run in self:
            pref = ""
            if insertpks:
                if run.rolename() == 'I':
                    """
                    Now it's an initiator too.
                    """
                    pref = ",KI%s,KJ%s" % (initkey,initkey)
                    initkey += 1
            s += run.spl(rolenames,pref + narr[i])
            s += "\n"
            i += 1
        return s


def genall_fresh(rolecount,runcount,prefix,agentcount):
    """ Generate all scenarios that start with the prefix """

    global scenarioListCache

    #key = (rolecount,runcount)

    if not prefix:
        prefix = Scenario(rolecount,runcount)
        start = Run(rolecount,agentcount=agentcount)
    else:
        start = prefix[-1].copy()

    if len(prefix) == runcount:
        return [prefix]
    else:
        res = []
        while True:
            nprefix = prefix.copy()
            nprefix.append(start.copy())
            res = res + genall_fresh(rolecount,runcount,nprefix,agentcount)
            if not start.increase():
                break
            """
            special antisymmetry case: the first run of the scenario
            should always have A as the actor. This breaks the
            Bob-symmetry.
            """

        return res

def genall(rolecount,runcount,agentcount):
    """ Generate all scenarios that start with the prefix """

    global scenarioListCache

    key = (rolecount,runcount,agentcount)
    if not key in scenarioListCache.keys():
        scenarioListCache[key] = genall_fresh(rolecount,runcount,None,agentcount)
    return scenarioListCache[key]



class ScenarioSet(object):

    """
    The ScenarioSet object consists of a list (collection) of
    Scenarios
    """

    def __init__(self,rolecount,runcount,fill=True,agentcount=2):
        self.rolecount = rolecount
        self.runcount = runcount
        self.agentcount = agentcount
        if not fill:
            self.list = []
        else:
            self.fill()

    def fill(self):
        """
        Fill the scenario set with all posibilities within the
        rolecount,runcount etc
        """
        self.list = genall(self.rolecount,self.runcount,self.agentcount)
        self.filter()
        clen = countscenarios(self.rolecount,self.runcount,self.agentcount)
        if len(self.list) != clen:
            Error.showWarning("Generated list length %i does not match computed length %i for roles %i, runs %i, agents %i!" % (len(self.list),clen,self.rolecount,self.runcount,self.agentcount))

    def cover(self):
        """
        Create a scenario set with a single element, that is used for
        the coverage in RepScen
        """
        self.list = []
        scen = Scenario(self.rolecount,self.runcount)
        start = Run(self.rolecount,agentcount=self.agentcount)
        while True:
            scen.append(start.copy())
            if not start.increase():
                break
        self.runcount = len(scen)
        self.list.append(scen)
        #print scen
        #print "Created cover for %i roles, it has %i runs." % (self.rolecount, self.runcount)

    def filter(self):
        """ Filter out Alice/Bob symmetries. Idea: Alice should always
        occur at least as often as Bob in each Scenario."""
        newlist = []
        for scen in self.list:
            same = False
            scen.sort()
            sceninv = scen.inverted()
            for oldscen in newlist:
                if (scen == oldscen) or (sceninv == oldscen):
                    same = True
                    break
                
            if not same:
                newlist.append(scen)

        self.list = newlist
        self.list.sort()

    def len(self):
        return len(self.list)

    def __str__(self,mymax=80):
        s = "*%i*[\n" % self.len()
        n = len(self.list)
        if n > mymax:
            n = mymax
        for i in range(0,n):
            s += "  "
            s += str(self.list[i])
            s += "\n"
        if n == mymax:
            s += "  ...\n"
        s += "]"
        return s

    def ordered(self):
        for s in self.list:
            s.sort()
        self.list.sort()


def countscenarios(rolecount,runcount,agentcount=2):
    """
    Compute the number of scenarios

    First compute unique scenarios (where A and B are distinct).
    Second, add the scenarios which are equal to themselves under A<>B swap
    Third, divide by two to remove the duplicates (all A<>B swap variants are twice in there)
    """
    
    x = chooseback(agentcount*rolecount*power((agentcount+1),rolecount-1),runcount)
    if agentcount == 2:

        if runcount % 2 == 0:
            x = x + chooseback(rolecount*power(3,rolecount-1),runcount/2)
        return x/2
    else:
        # Not implemented yet
        return x


def find_sessions(scen,offset=0,skip=[],ress=[],reso=[]):
    """
    Extract some sessions from a scenario, list them, and show the
    remainder.
    """
    if offset in skip:
        return find_sessions(scen,offset+1,skip,ress,reso)

    if offset < scen.runcount:
        # There might still be a session left
        found = []
        agentlist = []
        refrun = scen[offset]
        todo = 0
        for r in range(0,scen.rolecount):
            if refrun.agent(r) == refrun.agentcount:
                # intruder, so session is easier, we don't need a
                # session for this role
                found.append(-2)
            else:
                # not found yet
                found.append(-1)
                todo = todo + 1
            agentlist.append(refrun.agent(r))

        found[refrun.role()] = offset
        todo = todo - 1

        for i in range(offset+1, scen.runcount):
            if i not in skip:
                run = scen[i]
                # Is this an option?
                role = run.role()
                if found[role] == -1:
                    # compare all parameters
                    match = True
                    for r in range(0,scen.rolecount):
                        if agentlist[r] != run.agent(r):
                            match = False
                            break
                    if match:
                        found[role] = i
                        todo = todo - 1
                        if todo <= 0:
                            break

        # Scanned, do we have a session?
        if todo == 0:
            # we should remove the found ones
            ses = Session()
            for x in found:
                if x != -2:
                    ses.append(scen[x])
            ses.sort()

            return find_sessions(scen,offset+1,skip + found,ress+[ses],reso)
        else:
            return find_sessions(scen,offset+1,skip,ress,reso+[refrun])

    else:
        for i in range(offset,scen.runcount):
            if i not in skip:
                reso.append(scen[i])
        return (ress,reso)



def test(a,b):
    """
    For a roles and b runs, both generate the lists and compute the direct result.
    Print both to compare.
    """
    y = countscenarios(a,b)
    ss = ScenarioSet(a,b)
    x = ss.len()
    print a,b,":",y,x
    

def testsome():
    """
    Test some small values
    """
    for a in range(2,4):
        for b in range(1,4):
            test(a,b)

def counttest(roles,agents,maxproc):
    print "roles %i, agents %i:" % (roles,agents)
    for proc in range(1,maxproc+1):
        x = ScenarioSet(roles,proc,agents)
        print len(x.list)
    print 

if __name__ == "__main__":

    ## Test some values

    #counttest(1,3,4)
    #counttest(2,3,4)
    #counttest(3,2,4)
    #counttest(2,2,5)

    print """
Listing the full scenario set for 2 roles, 2 honest agents, and 2
processes.

SES is a shorthand for a protocol session, which we get in two cases:
1) One of the agents is E : then, SES represents one process, of the
hones agent talking to E, as E's process is subsumed by the
intruder.
2) Both agents are honest : then, SES represents two processes, one
for each honest agent.
    """
    print "Theoretical computed count : ", countscenarios(2,2,agentcount=2)
    x = ScenarioSet(2,2,agentcount=2)
    seslist = []
    for s in x.list:
        (ress,reso) = find_sessions(s)
        x = []
        for i in ress:
            x.append(str(i))
        for i in reso:
            x.append(str(i))
        seslist.append(" ".join(x))
    seslist.sort()
    print "\n".join(seslist)

    #x = ScenarioSet(2,2,agentcount=2)
    #myscen = x.list[5]
    #print myscen

    #testsome()


