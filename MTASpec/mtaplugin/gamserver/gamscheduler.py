##
## gamschudeler.py
##
## This is the new API for writing schedulers for CG-MTA GAMESS.
## This has been written due to the need to have various schedulars
## when performing an CG-MTA run on various kind of clusters
## and parallel machines.
##
## This code was written at CSIT, ANU, Canberra
## (Modifications and bugfixes done from PU, Pune; BARC, Mumbai)
##
## @author V.Ganesh
## @date 18 Oct 2006
## Last modified: 26th Apr 2007
##
## Modifications by Anuja
## Added 2 new classes for MTA-PLUGIN
## Last modified 17 APRIL 2010

import os
import sys
import math
import string
import socket
import traceback
import thread
import threading

from time import *
from thread import *
from traceback import *
from threading import *

import gamio
import gamprof
import gamlogger
import gamcleanup
import gamconstants

from gamio import *
from gamprof import *
from gamlogger import *
from gamconstants import *
from gamcleanup import *

#
# The GamScheduler class the main top level "abstract" class
# that defines the interfaces required from any scheduling class.
class GamScheduler:
    def __init__(self):
        self.schedulerName    = ""        # the name of this scheduler
        self.nodeAllocater    = None      # the instance of GamNodeAllocater
        self.inputPrefix      = None      # name of the input file (prefix only)
        self.noOfJobs         = -1        # number of jobs to submit
        self.sendDM           = 0         # pump in the DM ? (default is NO)
        
        self.keepAlive        = USE_KEEPALIVE_THREAD  # start the keep alive threads? 

        # timings counters 
        self.totalComputeTime = 0
        self.totalNetworkTime = 0 

        self.keepAliveThreadList = []     # list of currently active keep alive threads 

        # call inidcaters ... for internal function operations
        self.__pickedDMCalled      = 0
        self.__pickedOverlapCalled = 0
        self.__pickedFockCalled    = 0
        self.__readGradientsCalled = 0
        self.__readEnergyCalled    = 0
        self.__readChargesCalled   = 0

        # Executer port map needs to be appropriately filled
        # for different kind of schedulers, as well as the
        # ones that use the KeepAlive threads
        # This map consists of two values per node, one specifying
        # the port of gamexecuter main thread and the other the port
        # for the keep alive thread manager.
        self.executerPortMap = {}

    def setInputPrefix(self, inputPrefix):
        self.inputPrefix = inputPrefix
        
        # prequalify gamess log and profiler file
        qualifyLogFileName(self.inputPrefix)
        qualifyProfFileName(self.inputPrefix)

        
    # override this method for the implementing the
    # scheduling algorithm
    def runScheduler(self):
        pass
    
    def startKeepAliveThreads(self):
        if (self.keepAlive != 1): return

        # we need to start the keep alive threads here

        if (self.nodeAllocater == None): return

        nodes = self.nodeAllocater.getProcessorPerNode().keys()
 
        for node in nodes:
            self.startAKeepAliveThread(node, self)

    def startAKeepAliveThread(self, node, callBackObject):
        if (self.keepAlive != 1): return
        
        keepAliveThread = KeepAliveThread(callBackObject, node, \
                                          self.executerPortMap[node][1])  
      
        keepAliveThread.start()     
        self.keepAliveThreadList.append(keepAliveThread)

    def stopKeepAliveThreads(self):
        for keepAliveThread in self.keepAliveThreadList:
            keepAliveThread.stopIt()

    def readInPortNumbers(self):
        portFileName = self.getPortNumberFileName()
        appendLog("Info: Reading port nubers form: " + portFileName)

        pFile = open(portFileName, "r")
        ports = pFile.readlines()
        pFile.close()

        for port in ports:
            w = port.split()
            self.executerPortMap[w[0]] = map(string.atoi, w[1:])
            
        appendLog(repr(self.executerPortMap))
            
    def getFragmentFileName(self, currentJobIndex):
        return (self.inputPrefix + "frag" \
                   + string.zfill(currentJobIndex, FRAG_NUMBERS) + ".inp")

    def getFragmentListFileName(self):
        return (self.inputPrefix + "frag.lst")

    def isCorrectVersion(self, fos, fin):
        # pump in the verion number
        fos.write(PLUGIN_VERSION + '\n')
        fos.flush()

        # and return the responce
        return (fin.readline().find(GAMESS_YES) >= 0)

    def sendFragmentInput(self, fragFile, fos):        
        # the first line is the input prefix
        fos.write(self.inputPrefix + '\n')
        fos.flush()

        # and then the fragment data        
        ff = open(fragFile, "r")
        data = ff.readlines()
        ff.close()

        # then write the data
        for d in data:
            fos.write(d)

        fos.write(DATA_OVER)
        fos.flush()

    def getFullDMFileName(self, currentJobIndex):
        return (self.inputPrefix + "frag" \
                + string.zfill(currentJobIndex, FRAG_NUMBERS) + ".dm")

    def readPointCharges(self, fin):
        if (fin.readline().find(GAMESS_YES) >= 0):
            self.readAFile(fin, self.getPointChargesFileName(), self.__readChargesCalled)
            self.__readChargesCalled = 1
    
    def sendDMData(self, currentJobIndex, fos):
        # send info on the DM guess
        fos.write(repr(self.sendDM) + "\n")
        fos.flush()

        # send the DM file
        if (self.sendDM == 1):
            writeFile(self.getFullDMFileName(currentJobIndex), fos)

    def readFullDM(self, fin, currentJobIndex):
        self.readAFile(fin, self.getFullDMFileName(currentJobIndex), 0)

    def getPortNumberFileName(self):
        return self.inputPrefix + "ports"

    def getPointChargesFileName(self):
        return self.inputPrefix + "pcharges"
    
    def getDMFileName(self):
        return self.inputPrefix + "pij"

    def getOverlapFileName(self):
        return self.inputPrefix + "sij"

    def getFockFileName(self):
        return self.inputPrefix + "fij"

    def getEnergyFileName(self):
        return self.inputPrefix + "energy.out"

    def getGradientFileName(self):
        return self.inputPrefix + "grdfil"

    def getAllGradientFileName(self, currentJobIndex):
        return (self.inputPrefix + "frag" \
                + string.zfill(currentJobIndex, FRAG_NUMBERS) + "-allgrd")
 
    def getFCMFileName(self, currentJobIndex):
        return (self.inputPrefix + "frag" \
                    + string.zfill(currentJobIndex, FRAG_NUMBERS) + "-fcmfil") 

    def getDATFileName(self, currentJobIndex):
        return (self.inputPrefix + "frag" \
                    + string.zfill(currentJobIndex, FRAG_NUMBERS) + ".dat") 

    def getDDMFileName(self, currentJobIndex):
        return (self.inputPrefix + "frag" \
                    + string.zfill(currentJobIndex, FRAG_NUMBERS) + "-ddmfil") 

    def getOUTFileName(self, currentJobIndex):
        return (self.inputPrefix + "frag" \
                    + string.zfill(currentJobIndex, FRAG_NUMBERS) + ".out") 

    def readAFile(self, fin, fileName, doAppend):
        if (doAppend):
           readFile(fileName, fin, "a+")
        else:
           readFile(fileName, fin, "w")

    def readPickedDM(self, fin):
        self.readAFile(fin, self.getDMFileName(), self.__pickedDMCalled)
        self.__pickedDMCalled = 1
       
    def readPickedOverlap(self, fin):
        self.readAFile(fin, self.getOverlapFileName(), self.__pickedOverlapCalled)
        self.__pickedOverlapCalled = 1

    def readPickedFock(self, fin):
        self.readAFile(fin, self.getFockFileName(), self.__pickedFockCalled)
        self.__pickedFockCalled = 1

    def readGradients(self, fin, currentJobIndex):
        if (fin.readline().find(GAMESS_YES) >= 0):
           self.readAFile(fin, self.getGradientFileName(), self.__readGradientsCalled)
           self.__readGradientsCalled = 1

           self.readAFile(fin, self.getAllGradientFileName(currentJobIndex), 0)

    def readFCM(self, fin, currentJobIndex):
        if (fin.readline().find(GAMESS_YES) >= 0):
            self.readAFile(fin, self.getFCMFileName(currentJobIndex), 0)

    def readDAT(self, fin, currentJobIndex):
        if (fin.readline().find(GAMESS_YES) >= 0):
            self.readAFile(fin, self.getDATFileName(currentJobIndex), 0)

    def readDDM(self, fin, currentJobIndex):
        if (fin.readline().find(GAMESS_YES) >= 0):
            self.readAFile(fin, self.getDDMFileName(currentJobIndex), 0)

    def readAtomsAndContractions(self, fin):
        return (string.atoi(fin.readline()), string.atoi(fin.readline())) 

    def readEnergy(self, fin, currentJobIndex):
        if (self.__readEnergyCalled):
           enefil = open(self.getEnergyFileName(), "a+")
        else:
           enefil = open(self.getEnergyFileName(), "w") 
           self.__readEnergyCalled = 1

        ene = fin.readline()
        enefil.write(repr(currentJobIndex) + " " + ene)
        enefil.close()

        return ene

    def readOUT(self, fin, currentJobIndex):
        if (fin.readline().find(GAMESS_YES) >= 0):
            self.readAFile(fin, self.getOUTFileName(currentJobIndex), 0)

    #
    # appropriately place the energy values
    def convertEnergyFile(self):
        uene = open(self.getEnergyFileName(), "r")
        lines = uene.readlines()
        uene.close()
        
        energies = {}
        for line in lines:
            words = string.split(line)
            energies[words[0]] = words[1]
            
        orene = open(self.getEnergyFileName(), "w")
        for i in range(1, len(energies)+1):
            orene.write(energies[repr(i)] + " ")
            
        orene.close()

        appendLog("Energies are: " + repr(energies))        

    def keepAliveCallback(self, keepAliveThread): 
        pass

# .............................................................


# .............................................................
#
# the keep alive thread is used to induce a periodic "heart beat" 
# sequence to make sure that the participating GamExecutes have 
# not unceremoniously died off during their 
class KeepAliveThread(Thread):

    # folling are codes (errorCode) to indicate the node status 
    NODE_DEAD  = 0 
    NODE_ALIVE = 1
 
    def __init__(self, callBackObject, theServer, port):
        Thread.__init__(self)

        self.callBackObject = callBackObject
        self.theServer      = theServer
        self.stopPing       = 0
        self.errorCode      = KeepAliveThread.NODE_ALIVE
        self.errorMessage   = ""
        self.theSocket      = None
        self.port           = port

    def run(self):
        self.errorOccured = 0

        while 1:
            try:
                if (self.stopPing): break
 
                try:
                   if (self.callBackObject.threadStopped): break
                except:
                   pass

                # connect to the first server in list
                self.theSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
                self.theSocket.settimeout(HEART_BEAT_SPAN*4)
                
                # open up the connection
                self.theSocket.connect((self.theServer, self.port))

                # open o/p & i/p stream                
                fin = self.theSocket.makefile("r")
                fos = self.theSocket.makefile("w")

                # pump in keep alive message
                fos.write(PLUGIN_SERVER_KEEPALIVE_MSG + '\n')
                fos.flush()

                # then read the echo
                line = fin.readline()
                if (not (string.strip(line) == PLUGIN_SERVER_KEEPALIVE_MSG)):
                    appendLog("KeepAliveThread: Unexpected response from >> " + \
                               repr(self.theServer)) 
                else:
                   if (self.errorOccured == 1): 
                      self.errorCode    = KeepAliveThread.NODE_ALIVE
                      self.errorMessage = "Again receiving heart beat!"
                      self.callBackObject.keepAliveCallback(self)
                   self.errorOccured = 0

                # close all stuff
                fin.close()
                self.theSocket.close()

                # sleep for a long ( ..... ) period!
                sleep(HEART_BEAT_SPAN)
            except Exception, err:
                self.errorCode    = KeepAliveThread.NODE_DEAD
                self.errorMessage = "Error in receiving heart beat!" 
                self.callBackObject.keepAliveCallback(self)
                self.errorOccured = 1

                # sleep for a longer ( ..... ) period! ... before retrying
                sleep(HEART_BEAT_SPAN*2)

    def stopIt(self):
        self.stopPing = 1

        try:
           self.theSocket.close()
        except:
           pass
 
# .............................................................


# .............................................................
#
# The default scheduling policy is dynamic and is described in:
# J. Chem. Phys. 125, 104109 (2006)
class DefaultGamScheduler(GamScheduler):
    def __init__(self):
        GamScheduler.__init__(self)

        self.schedulerName        = "DefaultGamScheduler" 
        self.executerThreadList   = [] 
        self.currentNodeList      = []
        self.defunctJobList       = []
        self.currentJobIndex      = 0 
        self.currentJobIndex_lock = Lock()
         
    def runScheduler(self):
        appendLog("="*80)
        appendLog(self.schedulerName + ": Scheduler Started")

        if (self.nodeAllocater == None):
           appendLog("Error: No GamNodeAllocater is defined!")
           return

        if (self.inputPrefix == None):
           appendLog("Error: No inputPrefix is defined!")
           return

        if (self.noOfJobs == -1):
           appendLog("Error: No jobs to be run!")
           return 

        # clean up the mess before proceeding
        cleanupScheduler()

        # read in the executer port numbers
        self.readInPortNumbers()

        # get the node list
        ppNodes = self.nodeAllocater.getProcessorPerNode()
        self.currentNodeList = ppNodes.keys()

        while(1):
           # first check if we have at least some one 
           # to run out jobs ... else back off!
           if (len(self.currentNodeList) == 0):
              appendLog("Error: No one available to run the jobs! Backing off ...")
              break  

           # start one thread per node
           for node in self.currentNodeList:
              executerThread = DefaultGamScheduler.ExecuterThread(self, node, \
                                              self.executerPortMap[node][0], \
                                              ppNodes[node])              
                                              
              executerThread.start()
              self.executerThreadList.append(executerThread)

              # start the keep alive threads if requested
              self.startAKeepAliveThread(node, executerThread)

           # now wait for each thread to join
           for executerThread in self.executerThreadList:
              executerThread.join()

           # exit if we do not have any "left over" jobs
           if (len(self.defunctJobList) == 0): break
           else:
              # refresh our node list, so that we can get all the
              # nodes that are having "life-after-death"
              self.currentNodeList = self.nodeAllocater.getProcessorPerNode().keys()  
   
        # stop the keep alive threads if running
        self.stopKeepAliveThreads()

        # convert the energy file
        self.convertEnergyFile()

        appendLog("Total CPU time: " + str(round(self.totalComputeTime/60.0, 1)) \
                   + " min. " \
                   + "Total Network time: " + str(round(self.totalNetworkTime, 1)) \
                   + " sec.")
        appendLog(self.schedulerName + ": Scheduler Stopped")

    #
    # save a list of defunct job list
    def defunctJob(self, currentJobIndex):
        if (self.defunctJobList.count(currentJobIndex) < 1):
            self.defunctJobList.append(currentJobIndex)

    #
    # the executer thread
    class ExecuterThread(Thread):
        def __init__(self, enclosingClass, theServer, port, noOfProcessors):
            Thread.__init__(self)
            
            self.enclosingClass = enclosingClass
            self.theServer      = theServer
            self.port           = port
            self.theSocket      = None
            self.theOutPut      = None
            self.theInput       = None
            self.noOfProcessors = noOfProcessors        

            self.totalComputeTime = 0
            self.totalNetworkTime = 0
            
            self.currentNodeStatus = KeepAliveThread.NODE_ALIVE
            
            self.threadStopped = 0

        def shutDownClientConnection(self):
            try:
              if (self.theSocket != None): self.theSocket.shutdown(socket.SHUT_RDWR)
              if (self.theOutput != None): self.theOutput.close()
              if (self.theInput  != None): self.theInput.close()
            except:
              pass

        def cancelJob(self, currentJobIndex):
            if (currentJobIndex > self.enclosingClass.noOfJobs):
               appendLog("Warning: Request for cancelling invalid Job %" \
                          + repr(currentJobIndex) + " ignored.")
               return

            appendLog("Error: Job #" + repr(currentJobIndex) + " on " + \
                              repr(self.theServer) + " was terminated abnormally!!")

            # follow an optimistic shutdown policy ... just kill the
            # offending node for the time being and assume that all is
            # well for this job on some other node
            appendLog("Error: shutting down thread for: " + repr(self.theServer))
            appendLog("Error: Job #" + repr(currentJobIndex) + " is cancelled" + \
                       "... Will try to resubmit to other nodes.")
            self.enclosingClass.defunctJob(currentJobIndex)
            self.shutDownClientConnection()
        
        # 
        # What to do if a node breaks down?
        def keepAliveCallback(self, keepAliveThread):
            if (self.currentNodeStatus != keepAliveThread.errorCode):
                appendLog("Info: " + keepAliveThread.theServer + "> " + \
                          keepAliveThread.errorMessage)
                self.currentNodeStatus = keepAliveThread.errorCode

                # if the node is dead, we kill any job that is currently running
                if (keepAliveThread.errorCode == KeepAliveThread.NODE_DEAD):
                    self.cancelJob(self.currentJobIndex)

        def run(self):
            appendLog("Started client thread for server : " + self.theServer)

            # run the stuff, till we do not have any thing!
            while(not self.threadStopped):
             try:
               # if this node has failed, keep trying till we get a
               # NODE_ALIVE message
               if (self.currentNodeStatus == KeepAliveThread.NODE_DEAD):
                   sleep(HEART_BEAT_SPAN*2)
                   continue
               
               self.totalComputeTime = 0
               self.totalNetworkTime = 0

               # acquire lock
               self.enclosingClass.currentJobIndex_lock.acquire() 

               self.enclosingClass.currentJobIndex += 1

               # make a local copy of jobIndex, the current job index
               self.currentJobIndex = self.enclosingClass.currentJobIndex

               # check if we are over, before proceeding?
               if (self.currentJobIndex > self.enclosingClass.noOfJobs):
                  # check if any defunct jobs are present?
                  # is so we take them
                  if (not len(self.enclosingClass.defunctJobList) == 0):
                     # if yes we take up this new defunct job
                     sz = len(self.enclosingClass.defunctJobList)
                     self.currentJobIndex = self.enclosingClass.defunctJobList[sz-1]
                     self.enclosingClass.defunctJobList.remove( \
                         self.enclosingClass.defunctJobList[sz-1])
                  else:
                     # release the lock
                     self.enclosingClass.currentJobIndex_lock.release()

                     # and stop execution of this thread
                     self.threadStopped = 1
                     break

               appendLog("Submitting job " + repr(self.currentJobIndex) \
                         + " to the server : " + self.theServer + ":" + repr(self.port))

               # release the lock
               self.enclosingClass.currentJobIndex_lock.release() 

               startTime = time()
               self.theSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
               self.theSocket.connect((self.theServer, self.port))

               appendLog("Connection establised with : " + self.theServer \
                         + ":" + repr(self.port))
               
               fragFile = self.enclosingClass.getFragmentFileName(self.currentJobIndex)

               # open o/p stream connection
               fos = self.theSocket.makefile("w")
               self.theOutput = fos

               # open i/p stream
               fin = self.theSocket.makefile("r")
               self.theInput = fin

               # make sure we have correct versions
               if (not self.enclosingClass.isCorrectVersion(fos, fin)):
                  appendLog("Error: Cannot continue any further on this thread, " + \
                            "version confusion with the gamexecuter!.")
                  appendLog("Please check that all the servers in your cluster " + \
                            "have version: " + PLUGIN_VERSION)
                  appendLog("Error: shutting down thread for: " + repr(self.theServer)) 
                  appendLog("Error: Job #" + repr(self.currentJobIndex) + " is cancelled" + \
                            "... Will try to resubmit to other nodes.")  
                  self.enclosingClass.defunctJob(self.currentJobIndex) 
                  self.shutDownClientConnection()
                  return    

               # send the fragment input data
               self.enclosingClass.sendFragmentInput(fragFile, fos)

               appendLog("Input file " + fragFile + " sent (Job #" + \
                         repr(self.currentJobIndex) + ")")

               # send the DM if required
               self.enclosingClass.sendDMData(self.currentJobIndex, fos)

               appendLog("DM file sent for Job #" + repr(self.currentJobIndex))
               
               self.totalNetworkTime += time() - startTime

               appendLog("Awaiting completion of Job #" + repr(self.currentJobIndex))
               
               # sent the DM file ...
               if (self.enclosingClass.sendDM == 1):
                   appendLog("DM for job " + repr(self.currentJobIndex) \
                             + " to the server : " + self.theServer + " sent")
   
               # now wait for completetion of job
               startTime = time()  
               if (not fin.readline() == GAMESS_JOB_OVER):
                   self.cancelJob(self.currentJobIndex) # some thing is wrong!
                   return    

               # before we go further, we ensure that we were orderd to be
               # killed! If so backoff...
               if (self.currentNodeStatus == KeepAliveThread.NODE_DEAD): continue
               
               self.totalComputeTime += time() - startTime

               # acquire lock
               self.enclosingClass.currentJobIndex_lock.acquire()

               appendLog("Job #" + repr(self.currentJobIndex) + " on " + \
                          repr(self.theServer) + " over. Now collecting output...")
 
               startTime = time()
               # next read in the o/p ... P, S, F, grad, energy etc... 
               self.enclosingClass.readPickedDM(fin)
               self.enclosingClass.readPickedOverlap(fin)
               self.enclosingClass.readPickedFock(fin)
               self.enclosingClass.readFullDM(fin, self.currentJobIndex) 
               self.enclosingClass.readGradients(fin, self.currentJobIndex)
               self.enclosingClass.readFCM(fin, self.currentJobIndex)
               self.enclosingClass.readPointCharges(fin)
               self.enclosingClass.readEnergy(fin, self.currentJobIndex) 

               atoms, contractions = self.enclosingClass.readAtomsAndContractions(fin)

               self.totalNetworkTime += time() - startTime
               
               # close the streams and connection
               self.shutDownClientConnection()

               appendLog("Collecting output for Job #" + repr(self.currentJobIndex) \
                         + " from server : " + self.theServer \
                         + " over. CPU time: " + str(round(self.totalComputeTime/60.0, 1)) \
                         + " min." \
                         + " Network time: " + str(round(self.totalNetworkTime, 1)) + " sec.")

               # write profiling data
               appendProf(self.theServer + "\t" + repr(self.currentJobIndex) + "\t" \
                          + repr(atoms) + "\t" \
                          + repr(contractions) + "\t" \
                          + str(repr(round(self.totalComputeTime/60.0, 1))) + "\t" \
                          + repr(self.noOfProcessors))

               # release lock
               self.enclosingClass.currentJobIndex_lock.release()

               # record global timings
               self.enclosingClass.totalComputeTime += self.totalComputeTime
               self.enclosingClass.totalNetworkTime += self.totalNetworkTime
             except Exception, err:
               self.cancelJob(self.currentJobIndex)

               appendLog("Error is: " + err.__str__())  
               traceback.print_exc(file=sys.stderr)

               return
# .............................................................

# .............................................................
#
# A dynamic scheduler based on adaptive rating of nodes and 
# minimizing the idle time on the nodes.
# Typically useful on a hetrogeneous setup for 
# CG-MTA optimization runs.
class RatingBasedGamScheduler(GamScheduler):
    def __init__(self):
        GamScheduler.__init__(self)

        self.schedulerName = "RatingBasedGamScheduler"

    def runScheduler(self):
        appendLog(self.schedulerName + ": Scheduler Started")

        if (self.nodeAllocater == None):
           appendLog("Error: No GamNodeAllocater is defined!")
           return

        if (self.inputPrefix == None):
           appendLog("Error: No inputPrefix is defined!")
           return

        if (self.noOfJobs == -1):
           appendLog("Error: No jobs to be run!")
           return

        appendLog("WARNING: Scheduler code not yet implemented.")
        appendLog(self.schedulerName + ": Scheduler Stopped")

    def keepAliveCallback(self, keepAliveThread): 
        pass
# .............................................................


# .............................................................
#
# Adaptive learning based scheduler, which tries to speculate
# timing for each fragment and then submit it to most 
# appropriate node to get best utilization of resources at 
# hand. 
class AdaptiveGamScheduler(GamScheduler):
    def __init__(self):
        GamScheduler.__init__(self)

        self.schedulerName = "AdaptiveGamScheduler"

    def runScheduler(self):
        appendLog(self.schedulerName + ": Scheduler Started")

        if (self.nodeAllocater == None):
           appendLog("Error: No GamNodeAllocater is defined!")
           return

        if (self.inputPrefix == None):
           appendLog("Error: No inputPrefix is defined!")
           return

        if (self.noOfJobs == -1):
           appendLog("Error: No jobs to be run!")
           return

        appendLog("WARNING: Scheduler code not yet implemented.")
        appendLog(self.schedulerName + ": Scheduler Stopped")

    def keepAliveCallback(self, keepAliveThread): 
        pass
# .............................................................

 
# .............................................................
#
# The scheduler for @Home scheme 
class AtHomeGamScheduler(GamScheduler):
    def __init__(self):
        GamScheduler.__init__(self)

        self.schedulerName = "AtHomeGamScheduler"
        self.keepAlive     = 0  # no keep alive threads here

    def runScheduler(self):
        # start logging
        appendLog("="*80)
        appendLog(self.schedulerName + ": Scheduler Started")

        if (self.inputPrefix == None):
           appendLog("Error: No inputPrefix is defined!")
           return

        if (self.noOfJobs == -1):
           appendLog("Error: No jobs to be run!")
           return

        # The @Home scheduler works in a reverse way as compared to
        # other schedulers written above.
        # This scheduler per-say doesnot schedule jobs but waits
        # for an @Home client to connect to this "scheduler".
        # The first job in the list is then sent to the client
        # and the scheduler just "forgets" about it. The other jobs
        # are submitted in a similar way.
        # The client on its part executes the job, and sends the
        # results back to the @Home scheduler....
        # Read on more below to see how the scheduler handles
        # "defunct" clients...

        # clean up the mess before proceeding
        cleanupScheduler()

        # next, the @Home uses standard port numbers as 
        # opposed to dynamic port numbers in other schedulers
        # ... hence no need to read any port numbers for file
                
        appendLog("Number of jobs to complete : " + repr(self.noOfJobs))

        # bind and listen to standard port
        self.atHomeServer = None

        for i in range(1, 5):
            try:
                self.atHomeServer = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
                self.atHomeServer.bind((PLUGIN_HOST, AT_HOME_SERVER_PORT))                
            except:
                appendLog("Failed opening @Home server at port: " + repr(AT_HOME_SERVER_PORT) + " " + repr(i) + "th time.")
                sleep(SERVER_SLEEP_SPAN)

                self.atHomeServer = None
                continue

        if (self.atHomeServer == None):
            appendLog("Unable to start @Home server at port: " + repr(AT_HOME_SERVER_PORT))
            sys.exit(10)

        # uptill now how many jobs are submitted?
        self.currentJobCount = 0

        # maintain a list of submitted jobs
        self.submittedJobs = {}
        
        # then wait for any incomming connection ... infinitely
        # ... till we get all the jobs done
        while(1):
            try:
                self.atHomeServer.listen(1)
                conn, addr = self.atHomeServer.accept()

                self.peerSock = conn                
                self.peerName = conn.getpeername()[0]
                 
                # open i/p stream connection
                fis = conn.makefile("r")
                # open o/p stream
                fos = conn.makefile("w")

                # make sure we have correct versions
                if (not self.isCorrectVersion(fos, fis)):
                    appendLog("Error: Cannot continue any further on this thread, " + \
                              "version confusion with the @Home gamexecuter!.")
                    appendLog("Please check that all the @Home clients in your cluster " + \
                              "have version: " + PLUGIN_VERSION)
                    continue
                
                # now read in to check what is client wants to do?
                # the client can either request for a job or
                # send in a job status report (whether it ran or
                # failed etc...)
                clientReq = fis.readline()

                appendLog("Received " + clientReq.strip() + " from client " + self.peerName)
                
                if (clientReq.find(QUERY_JOB) >= 0):
                    # search for a job that is not already submitted
                    if (self.currentJobCount != self.noOfJobs):
                        fos.write(GAMESS_YES + '\n')
                        fos.flush()
                    
                        self.currentJobCount += 1

                        jobid = repr(self.currentJobCount)+":"+repr(int(time()))

                        self.submitJob(jobid, fos, fis)
                    
                        # add an entry into the submittedJobs list
                        self.submittedJobs[self.peerName+":"+jobid] = (jobid, 1)
                        
                        # close connection and continue....
                        fos.close()
                        fis.close()
                        conn.close()
                        continue
                    else:
                        # else submit a job with maximum count
                        maxIndex = -1; maxCnt = -1
                        for job in self.submittedJobs:
                            if ((self.submittedJobs[job][1] > maxCnt) and ((self.submittedJobs[job][1] != 0))):
                                maxCnt = self.submittedJobs[job][1]
                                maxIndex = job

                        if (maxIndex != -1):
                            fos.write(GAMESS_YES + '\n')
                            fos.flush()
                        
                            appendLog("Trying to resubmit " + self.submittedJobs[maxIndex][0] + " on " + maxIndex \
                                      + " to " + self.peerName)
                            self.submitJob(self.submittedJobs[maxIndex][0], fos, fis)
                            self.submittedJobs[maxIndex] = (self.submittedJobs[maxIndex][0], \
                                                            self.submittedJobs[maxIndex][1] + 1)
                        
                            # close connection and continue....
                            fos.close()
                            fis.close()
                            conn.close()
                            continue
                        else:
                            fos.write(GAMESS_NO + '\n')
                            fos.flush()
                            fos.close()
                            fis.close()
                            conn.close()
                            break
                elif (clientReq.find(JOB_COMPLETE) >= 0):
                    # read in the JOB ID first
                    idstr = string.strip(fis.readline())
                    appendLog("Job ID received : " + idstr + " from " + self.peerName)
                    jobCnt, jbtim = map(string.atoi, string.split(idstr, ":"))

                    # first check if such a job has atall been submitted?
                    found = 0; jobIndex = 0
                    for job in self.submittedJobs:
                        if ((self.submittedJobs[job][0].find(idstr) >= 0) and ((self.submittedJobs[job][1] != 0))):
                            found = 1
                            jobIndex = job
                            break

                    if (found != 1):
                        # no!! some stray job, tell the client and ignore it!
                        appendLog("Cancelling stray job " + idstr + " from : " + self.peerName)
                        fos.write(GAMESS_NO + '\n')
                        fos.flush()                    
                        fos.close()
                        fis.close()
                        conn.close()
                        continue
                    else:
                        fos.write(GAMESS_YES + '\n')
                        fos.flush()                    

                    # next read in the o/p ... P, S, F, grad, energy etc... 
                    self.readPickedDM(fis)
                    self.readPickedOverlap(fis)
                    self.readPickedFock(fis)
                    self.readFullDM(fis, jobCnt) 
                    self.readGradients(fis, jobCnt)
                    self.readFCM(fis, jobCnt)
                    self.readPointCharges(fis)
                    self.readEnergy(fis, jobCnt)
                    
                    atoms, contractions = self.readAtomsAndContractions(fis)

                    # remove entry from the submittedJobs list
                    self.submittedJobs[jobIndex] = (idstr, 0)

                    appendLog("Job #" + repr(jobCnt) + " is completed by : " + self.peerName \
                              + " (" + repr(self.submittedJobs[jobIndex][1]) + ")")
                    # close the streams and connection
                    fis.close()
                    fos.close()
                    conn.close()
                    continue
                elif (clientReq.find(JOB_KILLED) >= 0):
                    appendLog("Job on " + self.peerName + " Killed! .. Will keep trying...")
                
                    # close the streams and connection
                    fis.close()
                    fos.close()
                    conn.close()
                    continue
                else:
                    appendLog("Unknown request sent : " + req)
                
                    # close the streams and connection
                    fis.close()
                    fos.close()
                    conn.close()
                    continue                
            except Exception, err:
                print asctime() + " : Cannot execute job : ", err.__str__()
                appendLog("Cannot execute job : " + err.__str__())
                
                traceback.print_exc(file=sys.stderr)

        self.atHomeServer.close()
        self.atHomeServer = None
        
        # convert the energy file
        self.convertEnergyFile()
        appendLog(self.schedulerName + ": Scheduler Stopped")

    # actually submit the job...
    def submitJob(self, jobid, fos, fis):
        jobCnt, jobtm = map(string.atoi, string.split(jobid, ":"))

        # send the fragment input data
        fragFile = self.getFragmentFileName(jobCnt)
        self.sendFragmentInput(fragFile, fos)
        
        # then send info on the DM guess
        fos.write(repr(self.sendDM) + "\n")
        fos.flush()
        
        # send the DM file
        if (self.sendDM == 1):
            appendLog("Sending DM for job " + repr(jobCnt))
        
            dmFileName = self.getFullDMFileName(jobCnt)            
            writeFile(dmFileName, fos)
            
            appendLog("DM for job " + repr(jobCnt) + " sent")

        # finally send the job ID
        fos.write(jobid + "\n")
        fos.flush()        

        try:
            appendLog("Job #" + repr(jobCnt) + " is accepted by : " + self.peerName  \
                      + " (" + repr(self.submittedJobs[self.peerName][1]) + ")")
        except:
            appendLog("Job #" + repr(jobCnt) + " is accepted by : " + self.peerName)        

# .............................................................

# .............................................................
#
# The PlugIn Gam Scheduler
# @ Anuja
# Used for MTA-PlugIn application with GAMESS at the backend
class PlugInGAMESSGamScheduler(GamScheduler):
    def __init__(self):
        GamScheduler.__init__(self)

        self.schedulerName        = "PlugInGAMESSGamScheduler" 
        self.executerThreadList   = [] 
        self.currentNodeList      = []
        self.defunctJobList       = []
        self.currentJobIndex      = 0 
        self.currentJobIndex_lock = Lock()
        
        self.__readGradientsCalled = 1

    def readGradients(self, fin, currentJobIndex):
        if (fin.readline().find(GAMESS_YES) >= 0):
           self.readAFile(fin, self.getAllGradientFileName(currentJobIndex), self.__readGradientsCalled)
           self.__readGradientsCalled = 1

           self.readAFile(fin, self.getAllGradientFileName(currentJobIndex), 0)

    def runScheduler(self):
        appendLog("="*80)
        appendLog(self.schedulerName + ": Scheduler Started ")
        appendLog(self.schedulerName + repr(self.currentJobIndex))

        if (self.nodeAllocater == None):
           appendLog("Error: No GamNodeAllocater is defined!")
           return

        if (self.inputPrefix == None):
           appendLog("Error: No inputPrefix is defined!")
           return

        if (self.noOfJobs == -1):
           appendLog("Error: No jobs to be run!")
           return 

        # clean up the mess before proceeding
        cleanupScheduler()

        # read in the executer port numbers
        self.readInPortNumbers()

        # get the node list
        ppNodes = self.nodeAllocater.getProcessorPerNode()
        self.currentNodeList = ppNodes.keys()

        while(1):
           # first check if we have at least some one 
           # to run out jobs ... else back off!
           if (len(self.currentNodeList) == 0):
              appendLog("Error: No one available to run the jobs! Backing off ...")
              break  

           # start one thread per node
           for node in self.currentNodeList:
              executerThread = PlugInGAMESSGamScheduler.ExecuterThread(self, node, \
                                              self.executerPortMap[node][0], \
                                              ppNodes[node])              
                                              
              executerThread.start()
              self.executerThreadList.append(executerThread)

              # start the keep alive threads if requested
              self.startAKeepAliveThread(node, executerThread)

           # now wait for each thread to join
           for executerThread in self.executerThreadList:
              executerThread.join()

           # exit if we do not have any "left over" jobs
           if (len(self.defunctJobList) == 0): break
           else:
              # refresh our node list, so that we can get all the
              # nodes that are having "life-after-death"
              self.currentNodeList = self.nodeAllocater.getProcessorPerNode().keys()  
   
        # stop the keep alive threads if running
        self.stopKeepAliveThreads()

        # convert the energy file
        self.convertEnergyFile()

        appendLog("Total CPU time: " + str(round(self.totalComputeTime/60.0, 1)) \
                   + " min. " \
                   + "Total Network time: " + str(round(self.totalNetworkTime, 1)) \
                   + " sec.")
        appendLog(self.schedulerName + ": Scheduler Stopped")

    #
    # save a list of defunct job list
    def defunctJob(self, currentJobIndex):
        if (self.defunctJobList.count(currentJobIndex) < 1):
            self.defunctJobList.append(currentJobIndex)

    #
    # the executer thread
    class ExecuterThread(Thread):
        def __init__(self, enclosingClass, theServer, port, noOfProcessors):
            Thread.__init__(self)
            
            self.enclosingClass = enclosingClass
            self.theServer      = theServer
            self.port           = port
            self.theSocket      = None
            self.theOutPut      = None
            self.theInput       = None
            self.noOfProcessors = noOfProcessors        

            self.totalComputeTime = 0
            self.totalNetworkTime = 0
            
            self.currentNodeStatus = KeepAliveThread.NODE_ALIVE
            
            self.threadStopped = 0

        def shutDownClientConnection(self):
            try:
              if (self.theSocket != None): self.theSocket.shutdown(socket.SHUT_RDWR)
              if (self.theOutput != None): self.theOutput.close()
              if (self.theInput  != None): self.theInput.close()
            except:
              pass

        def cancelJob(self, currentJobIndex):
            if (currentJobIndex > self.enclosingClass.noOfJobs):
               appendLog("Warning: Request for cancelling invalid Job %" \
                          + repr(currentJobIndex) + " ignored.")
               return

            appendLog("Error: Job #" + repr(currentJobIndex) + " on " + \
                              repr(self.theServer) + " was terminated abnormally!!")

            # follow an optimistic shutdown policy ... just kill the
            # offending node for the time being and assume that all is
            # well for this job on some other node
            appendLog("Error: shutting down thread for: " + repr(self.theServer))
            appendLog("Error: Job #" + repr(currentJobIndex) + " is cancelled" + \
                       "... Will try to resubmit to other nodes.")
            self.enclosingClass.defunctJob(currentJobIndex)
            self.shutDownClientConnection()
        
        # 
        # What to do if a node breaks down?
        def keepAliveCallback(self, keepAliveThread):
            if (self.currentNodeStatus != keepAliveThread.errorCode):
                appendLog("Info: " + keepAliveThread.theServer + "> " + \
                          keepAliveThread.errorMessage)
                self.currentNodeStatus = keepAliveThread.errorCode

                # if the node is dead, we kill any job that is currently running
                if (keepAliveThread.errorCode == KeepAliveThread.NODE_DEAD):
                    self.cancelJob(self.currentJobIndex)

        def run(self):
            appendLog("Started client thread for server : " + self.theServer)

            # run the stuff, till we do not have any thing!
            while(not self.threadStopped):
             try:
               # if this node has failed, keep trying till we get a
               # NODE_ALIVE message
               if (self.currentNodeStatus == KeepAliveThread.NODE_DEAD):
                   sleep(HEART_BEAT_SPAN*2)
                   continue
               
               self.totalComputeTime = 0
               self.totalNetworkTime = 0

               # acquire lock
               self.enclosingClass.currentJobIndex_lock.acquire() 

               self.enclosingClass.currentJobIndex += 1

               # make a local copy of jobIndex, the current job index
               self.currentJobIndex = self.enclosingClass.currentJobIndex

               # check if we are over, before proceeding?
               if (self.currentJobIndex > self.enclosingClass.noOfJobs):
                  # check if any defunct jobs are present?
                  # is so we take them
                  if (not len(self.enclosingClass.defunctJobList) == 0):
                     # if yes we take up this new defunct job
                     sz = len(self.enclosingClass.defunctJobList)
                     self.currentJobIndex = self.enclosingClass.defunctJobList[sz-1]
                     self.enclosingClass.defunctJobList.remove( \
                         self.enclosingClass.defunctJobList[sz-1])
                  else:
                     # release the lock
                     self.enclosingClass.currentJobIndex_lock.release()

                     # and stop execution of this thread
                     self.threadStopped = 1
                     break

               appendLog("Submitting job " + repr(self.currentJobIndex) \
                         + " to the server : " + self.theServer + ":" + repr(self.port))

               # release the lock
               self.enclosingClass.currentJobIndex_lock.release() 

               startTime = time()
               self.theSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
               self.theSocket.connect((self.theServer, self.port))

               appendLog("Connection establised with : " + self.theServer \
                         + ":" + repr(self.port))
               
               fragFile = self.enclosingClass.getFragmentFileName(self.currentJobIndex)

               # open o/p stream connection
               fos = self.theSocket.makefile("w")
               self.theOutput = fos

               # open i/p stream
               fin = self.theSocket.makefile("r")
               self.theInput = fin

               # make sure we have correct versions
               if (not self.enclosingClass.isCorrectVersion(fos, fin)):
                  appendLog("Error: Cannot continue any further on this thread, " + \
                            "version confusion with the gamexecuter!.")
                  appendLog("Please check that all the servers in your cluster " + \
                            "have version: " + PLUGIN_VERSION)
                  appendLog("Error: shutting down thread for: " + repr(self.theServer)) 
                  appendLog("Error: Job #" + repr(self.currentJobIndex) + " is cancelled" + \
                            "... Will try to resubmit to other nodes.")  
                  self.enclosingClass.defunctJob(self.currentJobIndex) 
                  self.shutDownClientConnection()
                  return    

               # send the fragment input data
               self.enclosingClass.sendFragmentInput(fragFile, fos)

               appendLog("Input file " + fragFile + " sent (Job #" + \
                         repr(self.currentJobIndex) + ")")

               # send the DM if required
#REMOVE               self.enclosingClass.sendDMData(self.currentJobIndex, fos)

#REMOVE               appendLog("DM file sent for Job #" + repr(self.currentJobIndex))
               
               self.totalNetworkTime += time() - startTime

               appendLog("Awaiting completion of Job #" + repr(self.currentJobIndex))
               
               # sent the DM file ...
#REMOVE               if (self.enclosingClass.sendDM == 1):
#REMOVE                   appendLog("DM for job " + repr(self.currentJobIndex) \
#REMOVE                             + " to the server : " + self.theServer + " sent")
   
               # now wait for completetion of job
               startTime = time()  
               if (not fin.readline() == GAMESS_JOB_OVER):
                   self.cancelJob(self.currentJobIndex) # some thing is wrong!
                   return    

               # before we go further, we ensure that we were orderd to be
               # killed! If so backoff...
               if (self.currentNodeStatus == KeepAliveThread.NODE_DEAD): continue
               
               self.totalComputeTime += time() - startTime

               # acquire lock
               self.enclosingClass.currentJobIndex_lock.acquire()

               appendLog("Job #" + repr(self.currentJobIndex) + " on " + \
                          repr(self.theServer) + " over. Now collecting output...")
 
               startTime = time()
               # next read in the o/p ... P, S, F, grad, energy etc... 
#REMOVE               self.enclosingClass.readPickedDM(fin)
#REMOVE               self.enclosingClass.readPickedOverlap(fin)
#REMOVE               self.enclosingClass.readPickedFock(fin)
#REMOVE               self.enclosingClass.readFullDM(fin, self.currentJobIndex) 
               self.enclosingClass.readGradients(fin, self.currentJobIndex)
               self.enclosingClass.readFCM(fin, self.currentJobIndex)
               self.enclosingClass.readDAT(fin, self.currentJobIndex)
#REMOVE               self.enclosingClass.readPointCharges(fin)
               ene = self.enclosingClass.readEnergy(fin, self.currentJobIndex) 
#REMOVE               appendLog("energy = " + ene)

               atoms, contractions = self.enclosingClass.readAtomsAndContractions(fin)
               self.enclosingClass.readOUT(fin, self.currentJobIndex)

               self.totalNetworkTime += time() - startTime
               
               # close the streams and connection
               self.shutDownClientConnection()

               appendLog("Collecting output for Job #" + repr(self.currentJobIndex) \
                         + " from server : " + self.theServer \
                         + " over. CPU time: " + str(round(self.totalComputeTime/60.0, 1)) \
                         + " min." \
                         + " Network time: " + str(round(self.totalNetworkTime, 1)) + " sec.")

               # write profiling data
               appendProf(self.theServer + "\t" + repr(self.currentJobIndex) + "\t" \
                          + repr(atoms) + "\t" \
                          + repr(contractions) + "\t" \
                          + str(repr(round(self.totalComputeTime/60.0, 1))) + "\t" \
                          + repr(self.noOfProcessors))

               # release lock
               self.enclosingClass.currentJobIndex_lock.release()

               # record global timings
               self.enclosingClass.totalComputeTime += self.totalComputeTime
               self.enclosingClass.totalNetworkTime += self.totalNetworkTime
#ANUJA
#               if ((gamconstants.OPTIMIZER == 'ConjugateGradient') and (gamconstants.OPT_FRAGENES.has_key(0))):
#                  if ((string.atof(ene) - gamconstants.OPT_FRAGENES[0][self.currentJobIndex]) > 0.0001):
#                     appendLog("THIS GEOMETRY IS NO GOOD")
#                     appendLog("BACKING OFF ONE STEP ..")
#                     self.threadStopped = 1
#                     os.system("touch " + OUTPUT_PREFIX + "dst-stop")
#                     break

             except Exception, err:
               self.cancelJob(self.currentJobIndex)

               appendLog("Error is: " + err.__str__())  
               traceback.print_exc(file=sys.stderr)

               return
# .............................................................

# .............................................................
#
# The PlugIn Gam Scheduler
# @ Anuja
# Used for MTA-PlugIn application with GAMESS at the backend
class PlugInGAUSSIANGamScheduler(GamScheduler):
    def __init__(self):
        GamScheduler.__init__(self)

        self.schedulerName        = "PlugInGAUSSIANGamScheduler" 
        self.executerThreadList   = [] 
        self.currentNodeList      = []
        self.defunctJobList       = []
        self.currentJobIndex      = 0 
        self.currentJobIndex_lock = Lock()
        
        self.__readGradientsCalled = 1

    def readGradients(self, fin, currentJobIndex):
        if (fin.readline().find(GAMESS_YES) >= 0):
           self.readAFile(fin, self.getAllGradientFileName(currentJobIndex), self.__readGradientsCalled)
           self.__readGradientsCalled = 1

           self.readAFile(fin, self.getAllGradientFileName(currentJobIndex), 0)

    def getFragmentFileName(self, currentJobIndex):
        return (self.inputPrefix + "frag" \
                   + string.zfill(currentJobIndex, FRAG_NUMBERS) + ".inp")

    def runScheduler(self):
        appendLog("="*80)
        appendLog(self.schedulerName + ": Scheduler Started 2")
        appendLog(self.schedulerName + repr(self.currentJobIndex))

        if (self.nodeAllocater == None):
           appendLog("Error: No GamNodeAllocater is defined!")
           return

        if (self.inputPrefix == None):
           appendLog("Error: No inputPrefix is defined!")
           return

        if (self.noOfJobs == -1):
           appendLog("Error: No jobs to be run!")
           return 

        # clean up the mess before proceeding
        cleanupScheduler()

        # read in the executer port numbers
        self.readInPortNumbers()

        # get the node list
        ppNodes = self.nodeAllocater.getProcessorPerNode()
        self.currentNodeList = ppNodes.keys()

        while(1):
           # first check if we have at least some one 
           # to run out jobs ... else back off!
           if (len(self.currentNodeList) == 0):
              appendLog("Error: No one available to run the jobs! Backing off ...")
              break  

           # start one thread per node
           for node in self.currentNodeList:
              executerThread = PlugInGAUSSIANGamScheduler.ExecuterThread(self, node, \
                                              self.executerPortMap[node][0], \
                                              ppNodes[node])              
                                              
              executerThread.start()
              self.executerThreadList.append(executerThread)

              # start the keep alive threads if requested
              self.startAKeepAliveThread(node, executerThread)

           # now wait for each thread to join
           for executerThread in self.executerThreadList:
              executerThread.join()

           # exit if we do not have any "left over" jobs
           if (len(self.defunctJobList) == 0): break
           else:
              # refresh our node list, so that we can get all the
              # nodes that are having "life-after-death"
              self.currentNodeList = self.nodeAllocater.getProcessorPerNode().keys()  
   
        # stop the keep alive threads if running
        self.stopKeepAliveThreads()

        # convert the energy file
        self.convertEnergyFile()

        appendLog("Total CPU time: " + str(round(self.totalComputeTime/60.0, 1)) \
                   + " min. " \
                   + "Total Network time: " + str(round(self.totalNetworkTime, 1)) \
                   + " sec.")
        appendLog(self.schedulerName + ": Scheduler Stopped")

    #
    # save a list of defunct job list
    def defunctJob(self, currentJobIndex):
        if (self.defunctJobList.count(currentJobIndex) < 1):
            self.defunctJobList.append(currentJobIndex)

    #
    # the executer thread
    class ExecuterThread(Thread):
        def __init__(self, enclosingClass, theServer, port, noOfProcessors):
            Thread.__init__(self)
            
            self.enclosingClass = enclosingClass
            self.theServer      = theServer
            self.port           = port
            self.theSocket      = None
            self.theOutPut      = None
            self.theInput       = None
            self.noOfProcessors = noOfProcessors        

            self.totalComputeTime = 0
            self.totalNetworkTime = 0
            
            self.currentNodeStatus = KeepAliveThread.NODE_ALIVE
            
            self.threadStopped = 0

        def shutDownClientConnection(self):
            try:
              if (self.theSocket != None): self.theSocket.shutdown(socket.SHUT_RDWR)
              if (self.theOutput != None): self.theOutput.close()
              if (self.theInput  != None): self.theInput.close()
            except:
              pass

        def cancelJob(self, currentJobIndex):
            if (currentJobIndex > self.enclosingClass.noOfJobs):
               appendLog("Warning: Request for cancelling invalid Job %" \
                          + repr(currentJobIndex) + " ignored.")
               return

            appendLog("Error: Job #" + repr(currentJobIndex) + " on " + \
                              repr(self.theServer) + " was terminated abnormally!!")

            # follow an optimistic shutdown policy ... just kill the
            # offending node for the time being and assume that all is
            # well for this job on some other node
            appendLog("Error: shutting down thread for: " + repr(self.theServer))
            appendLog("Error: Job #" + repr(currentJobIndex) + " is cancelled" + \
                       "... Will try to resubmit to other nodes.")
            self.enclosingClass.defunctJob(currentJobIndex)
            self.shutDownClientConnection()
        
        # 
        # What to do if a node breaks down?
        def keepAliveCallback(self, keepAliveThread):
            if (self.currentNodeStatus != keepAliveThread.errorCode):
                appendLog("Info: " + keepAliveThread.theServer + "> " + \
                          keepAliveThread.errorMessage)
                self.currentNodeStatus = keepAliveThread.errorCode

                # if the node is dead, we kill any job that is currently running
                if (keepAliveThread.errorCode == KeepAliveThread.NODE_DEAD):
                    self.cancelJob(self.currentJobIndex)

        def run(self):
            appendLog("Started client thread for server : " + self.theServer)

            # run the stuff, till we do not have any thing!
            while(not self.threadStopped):
             try:
               # if this node has failed, keep trying till we get a
               # NODE_ALIVE message
               if (self.currentNodeStatus == KeepAliveThread.NODE_DEAD):
                   sleep(HEART_BEAT_SPAN*2)
                   continue
               
               self.totalComputeTime = 0
               self.totalNetworkTime = 0

               # acquire lock
               self.enclosingClass.currentJobIndex_lock.acquire() 

               self.enclosingClass.currentJobIndex += 1

               # make a local copy of jobIndex, the current job index
               self.currentJobIndex = self.enclosingClass.currentJobIndex

               # check if we are over, before proceeding?
               if (self.currentJobIndex > self.enclosingClass.noOfJobs):
                  # check if any defunct jobs are present?
                  # is so we take them
                  if (not len(self.enclosingClass.defunctJobList) == 0):
                     # if yes we take up this new defunct job
                     sz = len(self.enclosingClass.defunctJobList)
                     self.currentJobIndex = self.enclosingClass.defunctJobList[sz-1]
                     self.enclosingClass.defunctJobList.remove( \
                         self.enclosingClass.defunctJobList[sz-1])
                  else:
                     # release the lock
                     self.enclosingClass.currentJobIndex_lock.release()

                     # and stop execution of this thread
                     self.threadStopped = 1
                     break

               appendLog("Submitting job " + repr(self.currentJobIndex) \
                         + " to the server : " + self.theServer + ":" + repr(self.port))

               # release the lock
               self.enclosingClass.currentJobIndex_lock.release() 

               startTime = time()
               self.theSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
               self.theSocket.connect((self.theServer, self.port))

               appendLog("Connection establised with : " + self.theServer \
                         + ":" + repr(self.port))
               
               fragFile = self.enclosingClass.getFragmentFileName(self.currentJobIndex)

               # open o/p stream connection
               fos = self.theSocket.makefile("w")
               self.theOutput = fos

               # open i/p stream
               fin = self.theSocket.makefile("r")
               self.theInput = fin

               # make sure we have correct versions
               if (not self.enclosingClass.isCorrectVersion(fos, fin)):
                  appendLog("Error: Cannot continue any further on this thread, " + \
                            "version confusion with the gamexecuter!.")
                  appendLog("Please check that all the servers in your cluster " + \
                            "have version: " + PLUGIN_VERSION)
                  appendLog("Error: shutting down thread for: " + repr(self.theServer)) 
                  appendLog("Error: Job #" + repr(self.currentJobIndex) + " is cancelled" + \
                            "... Will try to resubmit to other nodes.")  
                  self.enclosingClass.defunctJob(self.currentJobIndex) 
                  self.shutDownClientConnection()
                  return    

               # send the fragment input data
               self.enclosingClass.sendFragmentInput(fragFile, fos)

               appendLog("Input file " + fragFile + " sent (Job #" + \
                         repr(self.currentJobIndex) + ")")

               # send the DM if required
#REMOVE               self.enclosingClass.sendDMData(self.currentJobIndex, fos)

#REMOVE               appendLog("DM file sent for Job #" + repr(self.currentJobIndex))
               
               self.totalNetworkTime += time() - startTime

               appendLog("Awaiting completion of Job #" + repr(self.currentJobIndex))
               
               # sent the DM file ...
#REMOVE               if (self.enclosingClass.sendDM == 1):
#REMOVE                   appendLog("DM for job " + repr(self.currentJobIndex) \
#REMOVE                             + " to the server : " + self.theServer + " sent")
   
               # now wait for completetion of job
               startTime = time()  
               if (not fin.readline() == GAUSS_JOB_OVER):
                   self.cancelJob(self.currentJobIndex) # some thing is wrong!
                   return    

               # before we go further, we ensure that we were orderd to be
               # killed! If so backoff...
               if (self.currentNodeStatus == KeepAliveThread.NODE_DEAD): continue
               
               self.totalComputeTime += time() - startTime

               # acquire lock
               self.enclosingClass.currentJobIndex_lock.acquire()

               appendLog("Job #" + repr(self.currentJobIndex) + " on " + \
                          repr(self.theServer) + " over. Now collecting output...")
 
               startTime = time()
               # next read in the o/p ... P, S, F, grad, energy etc... 
#REMOVE               self.enclosingClass.readPickedDM(fin)
#REMOVE               self.enclosingClass.readPickedOverlap(fin)
#REMOVE               self.enclosingClass.readPickedFock(fin)
#REMOVE               self.enclosingClass.readFullDM(fin, self.currentJobIndex) 
               self.enclosingClass.readGradients(fin, self.currentJobIndex)
#REMOVE               self.enclosingClass.readFCM(fin, self.currentJobIndex)
#REMOVE               self.enclosingClass.readPointCharges(fin)
               self.enclosingClass.readEnergy(fin, self.currentJobIndex) 

               atoms, contractions = self.enclosingClass.readAtomsAndContractions(fin)
               self.enclosingClass.readOUT(fin, self.currentJobIndex)

               self.totalNetworkTime += time() - startTime
               
               # close the streams and connection
               self.shutDownClientConnection()

               appendLog("Collecting output for Job #" + repr(self.currentJobIndex) \
                         + " from server : " + self.theServer \
                         + " over. CPU time: " + str(round(self.totalComputeTime/60.0, 1)) \
                         + " min." \
                         + " Network time: " + str(round(self.totalNetworkTime, 1)) + " sec.")

               # write profiling data
               appendProf(self.theServer + "\t" + repr(self.currentJobIndex) + "\t" \
                          + repr(atoms) + "\t" \
                          + repr(contractions) + "\t" \
                          + str(repr(round(self.totalComputeTime/60.0, 1))) + "\t" \
                          + repr(self.noOfProcessors))

               # release lock
               self.enclosingClass.currentJobIndex_lock.release()

               # record global timings
               self.enclosingClass.totalComputeTime += self.totalComputeTime
               self.enclosingClass.totalNetworkTime += self.totalNetworkTime
             except Exception, err:
               self.cancelJob(self.currentJobIndex)

               appendLog("Error is: " + err.__str__())  
               traceback.print_exc(file=sys.stderr)

               return
# .............................................................
