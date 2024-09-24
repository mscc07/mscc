##
## shutdowngamserver.py
##
## Automatcally shutdown gamserver on remote nodes.
## Caution: this is under assumption that there is only *this*
##          python script running!
##
## 27th June 2005 
##
## @author: V.Ganesh
##

import os
import sys
import string
import socket

from gamconstants import *

# start the servers
def shutServers(inputPrefix):
    print "Shutdown of gamexecuter in progress..."

    # first read in the port numbers
    pf = open(getPortNumberFileName(inputPrefix), "r")
    nodes = map(string.split, pf.readlines())
    pf.close()

    # for each node send a "nuke" packet on the
    # keep alive thread
    for node in nodes:
        try:
            print "Shutting down " + node[0] + ":" + node[1]
            
            s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            
            # open up the connection
            s.connect((node[0], string.atoi(node[2])))

            # open o/p & i/p stream                
            fin = s.makefile("r")
            fos = s.makefile("w")

            # pump in keep "nuke" message
            fos.write(PLUGIN_SERVER_KILL_MSG + '\n')
            fos.flush()
            
            # no waits...
            fos.close()
            fin.close()
            s.close()

            print "Shutting down command to " + node[0] + ":" + node[1] \
                  + " sent."
        except Exception, err:
            print "Improper shutdown for " + node[0] \
                  + ". Error is: " + err.__str__()
            
    print "Shutdown of gamexecuter completed."
    
def getPortNumberFileName(inputPrefix):
    return OUTPUT_PREFIX + "/" + inputPrefix + "ports"
    
# the main function
if __name__ == "__main__":
    if (len(sys.argv) != 2):
        print "usage: python shutdowngamserver.py <inputPrefix>"
        sys.exit(10)
        
    shutServers(sys.argv[1])
