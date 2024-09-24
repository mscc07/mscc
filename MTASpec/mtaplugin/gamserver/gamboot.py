##
## gamboot.py
##
## This is the new API for writing the boot process for new
## gamexecuter servers on a remote server.
##
## This code was written at CSIT, ANU, Canberra
##
## @author V.Ganesh
## @date 18 Oct 2006
## Last modified: 9th Nov 2006
##

import time
import socket

from time import *

import gamlogger
import gamconstants

from gamlogger import *
from gamconstants import *

#
# The null loader, doesn't do any thing
class GamBoot:        
    def __init__(self, nodeAllocater=None, inputPrefix=None, \
                 talkBackPort=None, talkBackHost=HOST_SUFIX):
        self.nodeAllocater = nodeAllocater
        self.inputPrefix   = inputPrefix
        self.outputPrefix  = OUTPUT_PREFIX
        self.talkBackPort  = talkBackPort
        self.talkBackHost  = talkBackHost

    def resetOutputPrefix(self):
        self.outputPrefix  = OUTPUT_PREFIX

        qualifyLogFileName(self.outputPrefix + "/" + self.inputPrefix)

    def getGenericCmd(self, node):
        if (node == None):
            return PYTHON_CMD + REMOTE_GAMSERVER_ROOT + "/gamserver.py " + \
                   self.talkBackHost + " " + repr(self.talkBackPort) + \
                   " localhost " + self.outputPrefix + " " + \
                   REMOTE_GAMSERVER_ROOT + " &"        
        else:
            return PYTHON_CMD + REMOTE_GAMSERVER_ROOT + "/gamserver.py " + \
                   self.talkBackHost + " " + repr(self.talkBackPort) + \
                   " " + node + " " + self.outputPrefix + " " + \
                   REMOTE_GAMSERVER_ROOT + " &"

    def getHostFileName(self, theHost):
        return (self.outputPrefix + "/" + self.inputPrefix + \
                theHost + "-" + theHost + "-host.gam")

    def checkSuccess(self, status, bootCmd):
        if (status == 0):
            appendLog("GamBoot: " + bootCmd + " was successful.")
        else:
            appendLog("GamBoot Warning: " + bootCmd + " faild to execute correctly.")
        
    def writeHostFile(self, theHost, noOfProcessor, pNames=None):
        hstFileName = self.getHostFileName(theHost)
        
        hstFile = open(hstFileName, "w")

        if (pNames == None):
          for i in range(0, noOfProcessor):
             hstFile.write(theHost + "\n")
        else:
          # ensure that the starting node is first in the list
          for i in range(0, noOfProcessor):
             if (theHost.find(pNames[i]) < 0): continue
             hstFile.write(theHost + "\n")

          for i in range(0, noOfProcessor):
             if (theHost.find(pNames[i]) >= 0): continue
             hstFile.write(pNames[i] + "\n")

        hstFile.close()

        if (noOfProcessor > 1):
            if (not NFS_AVAILABLE):
                cpCmd = REMOTE_COPY_CMD + hstFileName + " " + \
                        theHost + ":" + self.outputPrefix

                appendLog("Copying host file: " + cpCmd)
                status = os.system(cpCmd)
                self.checkSuccess(status, cpCmd)

    def syncSleep(self):
        sleep(REMOTE_SYNC_TIME)
        
    def bootIt(self):
        pass
# .............................................................


# .............................................................          
#
# Gam boot process that uses rsh (or ssh) to spawn
# the gamexecuter processes on remote hosts.
# Remember that for using rsh/ ssh for process spawning
# you should have a password less access among the nodes.
class RSHGamBoot(GamBoot):
    def __init__(self):
        GamBoot.__init__(self)
        
    def bootIt(self):
        ppNodes  = self.nodeAllocater.getProcessorPerNode()
        nodeList = ppNodes.keys()

        appendLog("*"*80)
        appendLog("RSHGamBoot: Initilising the boot process...")
        for node in nodeList:
            numaCmd = ""
            
            if (NUMA_ENABLED):
                numaCmd = DPLACE_CMD + NUMACTL_CMD
                
            bootCmd = REMOTE_CMD + node + " " + numaCmd + self.getGenericCmd(node)
            
            appendLog("On node " + node + ": " + bootCmd)
            
            status = os.system(bootCmd)
            self.checkSuccess(status, bootCmd)
           
            if (not self.nodeAllocater.getHasDifferentProcessorNames()): 
               self.writeHostFile(node, ppNodes[node])
            else:
               self.writeHostFile(node, ppNodes[node], self.nodeAllocater.getProcessorNamePerNode()[node])

        appendLog("RSHGamBoot: Boot process over.")
# .............................................................


# .............................................................          
# 
# Gam boot process that uses pbsdsh to spawn
# the gamexecuter processes on remote hosts.
class PBSDSHGamBoot(GamBoot):
    def __init__(self):
        GamBoot.__init__(self)
    
    def bootIt(self):
        ppNodes  = self.nodeAllocater.getProcessorPerNode()
        nodeList = ppNodes.keys()

        appendLog("*"*80)
        appendLog("PBSDSHGamBoot: Initilising the boot process...")

        numaCmd = ""
            
        if (NUMA_ENABLED):
            numaCmd = DPLACE_CMD + NUMACTL_CMD
                
        bootCmd = PBSDSH_CMD + numaCmd + self.getGenericCmd(None)
                  
        appendLog("Boot command is: " + bootCmd)
        
        status  = os.system(bootCmd)
        self.checkSuccess(status, bootCmd)

        if (not self.nodeAllocater.getHasDifferentProcessorNames()):
          for node in nodeList:
             self.writeHostFile(node, ppNodes[node])
        else:
          for node in nodeList:
             self.writeHostFile(node, ppNodes[node], self.nodeAllocater.getProcessorNamePerNode()[node])

        appendLog("PBSDSHGamBoot: Boot process over.")

        self.syncSleep()

