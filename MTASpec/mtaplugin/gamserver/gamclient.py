##
## gamclient.py
##
## GAMESS client, a multi threaded client for GAMESS server
## from a list of specified i/p files.
##
##
## 8th June 2004 (The day of venus transit, after 122 years)
##
## 9th Nov 2006: Completely rewritten using the new gamscheduler APIs
##
## @author: V.Ganesh
##

import sys
import string

import gamconstants
import gamscheduler

from gamconstants import *

#
# start the scheduler
def startGamScheduler(inputPrefix):
    gamScheduler = gamscheduler.PlugInGAUSSIANGamScheduler()
    gamScheduler.nodeAllocater = gamNodeAllocater
    gamScheduler.setInputPrefix(inputPrefix)

    # read frag.lst to check the number of jobs
    fragLst = open(inputPrefix+"frag.lst", "r")    
    gamScheduler.noOfJobs = string.atoi(string.split(fragLst.readline())[0])    
    fragLst.close()
    
    # check if we need to pump guess DM
    dmgus = open(inputPrefix+"senddm", "r")
    gamScheduler.sendDM = string.atoi(dmgus.readline())
    dmgus.close()

    # and then start of the scheduler
    gamScheduler.runScheduler()
    
# the main function
if __name__ == "__main__":
    if (len(sys.argv) != 2):
        print "usage: python gamclient.py <input-prefix>"
        sys.exit(10)
        
    startGamScheduler(sys.argv[1])


