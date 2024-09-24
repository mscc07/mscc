##
## bootgamserver.py
##
## Automatcally start gamserver on remote nodes.
##
## 27th June 2005 
## 9th Nov 2006: Modified and rewritten using the new gamboot APIs.
##
## @author: V.Ganesh
##

import os
import sys
import string
import socket
import thread
import traceback
import threading

from time import *
from thread import *
from traceback import *
from threading import *

import gamconstants
import gamlogger

from gamlogger import *
from gamconstants import *

#
# Boot up the executers...
# The boot up process starts of by checking if
# PLUGIN_USE_STD_PORT is set to 1 (the default
# being 0).
# If set to 1, then the bootup starts
# up normally.
# If set to 0(default)/ 1, then a simple server
# is started up at an available free port.
# This port address is passed to the gamserver
# process, with then registers its port number back
# to the bootup process.
# In both the cases, a ports file is written that
# contain the executer host name and 2 ports, one
# for the compute thread and another for the
# "keep-alive" thread.
def bootUpExecuters(inputPrefix):
    # check to make sure that we need to auto boot
    if (not AUTO_BOOT_GAMSERVER): return

    # get the node list
    ppNodes = gamNodeAllocater.getProcessorPerNode()
    currentNodeList = ppNodes.keys()
    
    # first start up the port collector thread
    prtCol = PortCollector(len(ppNodes), inputPrefix)

    # for each node
    gamBoot.nodeAllocater = gamNodeAllocater
    gamBoot.inputPrefix   = inputPrefix
    gamBoot.talkBackHost  = HOST_SUFIX
    gamBoot.talkBackPort  = prtCol.obtainCollectorPort()

    gamBoot.resetOutputPrefix()
    
    # start collecting ports
    prtCol.start()
    
    # start bootup process
    gamBoot.bootIt()

    # let all port collection end here
    prtCol.join()

# the port collecter server thread
class PortCollector(Thread):
    def __init__(self, n, inputPrefix):
        Thread.__init__(self)
        
        self.theSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.theSocket.bind((PLUGIN_HOST, PLUGIN_ANY_PORT))

        self.numberOfNodes = n
        self.inputPrefix   = inputPrefix

        qualifyLogFileName(self.inputPrefix)

    # port file name!
    def getPortNumberFileName(self):
        return OUTPUT_PREFIX + "/" + self.inputPrefix + "ports"

    def obtainCollectorPort(self):
        return self.theSocket.getsockname()[1]

    def run(self):
        pf = open(self.getPortNumberFileName(), "w")
                  
        for i in range(0, self.numberOfNodes):
            try:
                self.theSocket.listen(1)
                conn, addr = self.theSocket.accept()

                fis = conn.makefile("r")

                port1 = string.strip(fis.readline())
                port2 = string.strip(fis.readline())
                peerName = string.strip(fis.readline())

                pf.write(peerName + " " + port1 + " " + port2 + "\n")
                
                fis.close()
                conn.close()
            except Exception, err:
                appendLog("Cannot read port number : " + err.__str__())          
                traceback.print_exc(file=sys.stderr)
            
        pf.close()
    
# the main function
if __name__ == "__main__":
    if (len(sys.argv) != 2):
        print "usage: python bootgamserver.py <inputPrefix>"
        sys.exit(10)
        
    bootUpExecuters(sys.argv[1])

