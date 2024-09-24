##
## gamserver.py
##
## GAMESS server, a single threaded server for executing GAMESS
## from the specified i/p file.
##
## 8th June 2004 (The day of venus transit, after 122 years)
##
## 9th Nov 2006: Completely rewritten using the new gamexecuter APIs
##
## @author: V.Ganesh
##

import sys
import string
import socket
import traceback
import os

from getpass import *

from traceback import *

import gamlogger
import gamconstants

from gamlogger import *
from gamconstants import *


# start running the gam executer
def startGamExecuter(schedulerHost, talkbackPort, nodeName, \
                     _OUTPUT_PREFIX, _REMOTE_GAMSERVER_ROOT):
    
    # setup the log file
    gamExecuter.setLogFile(_OUTPUT_PREFIX)

    # and the server root
    gamExecuter.setGamServerRoot(_REMOTE_GAMSERVER_ROOT)
    
    # first init the executer
    gamExecuter.initExecuter()

    # then collect the port numbers
    compute, keepalive = gamExecuter.obtainPortNumbers()

    # send it across
    try:
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.connect((schedulerHost, talkbackPort))

        fos = s.makefile("w")
        fos.write(repr(compute) + '\n')
        fos.flush()

        fos.write(repr(keepalive) + '\n')
        fos.flush()

        fos.write(nodeName + '\n')
        fos.flush()
        
        fos.close()
        s.close()
    except Exception, err:
        appendLog("Unable to send the port information: " + err.__str__())  
        traceback.print_exc(file=sys.stderr)
    
    # and then really start running the executer
    gamExecuter.runExecuter()
    
# start running the gam executer ... cold start
def coldStartGamExecuter(port):
    # setup the log file
    gamExecuter.setLogFile(OUTPUT_PREFIX)

    # and the server root
    gamExecuter.setGamServerRoot(REMOTE_GAMSERVER_ROOT)

    # use the specified port to boot
    gamExecuter.setUseStandardPort(port)
    
    # first init the executer
    gamExecuter.initExecuter()
    
    # and then really start running the executer
    gamExecuter.runExecuter()
    
# the main function
if __name__ == "__main__":
    _OUTPUT_PREFIX         = OUTPUT_PREFIX
    _REMOTE_GAMSERVER_ROOT = REMOTE_GAMSERVER_ROOT
    
    if (len(sys.argv) < 2):
        print "usage: python gamserver.py [<gamscheduler-host> <talkback-port> [nodeName]" + \
              " [SCR] [GMSPATH]] [-force <port>] [-athome]"
        sys.exit(10)

    # foce a cold boot
    if (sys.argv[1].find("-force") >= 0):
        coldStartGamExecuter(string.atoi(sys.argv[2]))
    elif (sys.argv[1].find("-athome") >= 0):
        coldStartGamExecuter(AT_HOME_SERVER_PORT)
    else:
        # else a normal remote boot
        try:
            _OUTPUT_PREFIX         = sys.argv[4]
            _REMOTE_GAMSERVER_ROOT = sys.argv[5]
        except:
            _OUTPUT_PREFIX         = OUTPUT_PREFIX
            _REMOTE_GAMSERVER_ROOT = REMOTE_GAMSERVER_ROOT
            
        # ... start it!
        if (len(sys.argv) >= 4):
            if (sys.argv[3].find("localhost") >= 0):
                startGamExecuter(sys.argv[1], string.atoi(sys.argv[2]), \
                                 socket.gethostname(), \
                                 _OUTPUT_PREFIX, _REMOTE_GAMSERVER_ROOT)
            else:
                startGamExecuter(sys.argv[1], string.atoi(sys.argv[2]), sys.argv[3], \
                                 _OUTPUT_PREFIX, _REMOTE_GAMSERVER_ROOT)            
        else:
            startGamExecuter(sys.argv[1], string.atoi(sys.argv[2]), \
                             socket.gethostname(), \
                             _OUTPUT_PREFIX, _REMOTE_GAMSERVER_ROOT)

        
  
