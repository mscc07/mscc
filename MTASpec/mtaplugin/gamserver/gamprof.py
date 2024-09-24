##
## gamprof.py
##
## Simple profiler (and to be used as feedback for gamclient)
##
## 12th Oct 2004 
##
## @author: V.Ganesh
##

import os
import sys
import string
import traceback
import thread
import threading

from time import *
from thread import *
from traceback import *
from threading import *


## for handling MT client
_prof_lock = Lock()

## gamess log file name
_gamessProfFileName = "gamess_prof.log"

##
## pre-qualify the gamess profiling file with a inputPrefix 
def qualifyProfFileName(inputPrefix):
    global _gamessProfFileName

    _gamessProfFileName = inputPrefix + "gamess_prof.log"
    
##
## append a profiling entry with the time stamp
def appendProf(profEntry):
    global _gamessProfFileName
    
    _prof_lock.acquire()
    
    prof = open(_gamessProfFileName, "a+")
    prof.write(profEntry + "\n")
    prof.close()

    _prof_lock.release()
    
    return

##
## clear all entries in the profiler file
def clearProf():
    global _gamessProfFileName
    
    _prof_lock.acquire()
    
    prof = open(_gamessProfFileName, "w")
    prof.write("Node\tJob No.\tAtoms \tContractions \tTime(min) \tNp\n")
    prof.close()

    _prof_lock.release()
    
    return

