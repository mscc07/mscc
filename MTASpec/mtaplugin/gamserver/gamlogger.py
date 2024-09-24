##
## gamlogger.py
##
## loggings utils for gamess client or server.... though
## more useful for the client!
##
## 29th June 2004 
##
## @author: V.Ganesh
##

import os
import sys
import string
import thread
import threading

from thread import *
from threading import *

from time import *

## for handling MT client
_log_lock = Lock()

## gamess log file name
_gamessLogFileName = "gamess_log.log"

##
## pre-qualify the gamess log file with a inputPrefix 
def qualifyLogFileName(inputPrefix):
    global _gamessLogFileName

    _gamessLogFileName = inputPrefix + "gamess_log.log"

##
## append a log entry with the time stamp
def appendLog(logEntry):
    global _gamessLogFileName

    _log_lock.acquire()
    
    log = open(_gamessLogFileName, "a+")
    log.write(asctime() + " :: " + logEntry + "\n")
    log.close()

    _log_lock.release()
    
    return

##
## clear all entries in the log
def clearLog():
    global _gamessLogFileName

    _log_lock.acquire()
    
    log = open(_gamessLogFileName, "w")
    log.write("")
    log.close()

    _log_lock.release()

    return
