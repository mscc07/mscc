##
## gamexecuter.py
##
## This defines the new APIs for defining GAMESS executers,
## or "compute servers".
##

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

import gamconstants
import gamprof
import gamlogger
import gamcleanup

from gamio import *
from gamprof import *
from gamconstants import *
from gamcleanup import *
from gamlogger import *

#
# The top level class defining the basic interfaces
# required of a compute server. 
class GamExecuter:
    def __init__(self, useStandardPort=0):
        self._useStandardPort = useStandardPort # use a standard GamExecuter port
        
        self.inputPrefix  = ""
        self.outputPrefix = OUTPUT_PREFIX
        
        self.doSendPointCharges = 0 # send point charges after calculation (NO)

        self.gamserverRoot = REMOTE_GAMSERVER_ROOT

    def setGamServerRoot(self, newRoot):
        self.gamserverRoot = newRoot
        
    def setLogFile(self, outputPrefix):
        self.outputPrefix = outputPrefix

        qualifyLogFileName(self.outputPrefix + "/gamexecuter-" \
                           + HOST_SUFIX + "-")
        
    def setInputPrefix(self, inputPrefix):
        self.inputPrefix = inputPrefix

        # prequalify gamess log file for the executer
        # make sure that we have independent log files
        # for each jobs, so that we don't mess up
        # in case we have a multi processor environment
        # and have two instances of executer running
        # in the same "box".
        qualifyLogFileName(self.outputPrefix + "/gamexecuter-" \
                           + self.inputPrefix + HOST_SUFIX + "-")

    def setUseStandardPort(self, useStandardPort):
        self._useStandardPort = useStandardPort
        
    def obtainPortNumbers(self): 
        pass
    
    def runExecuter(self):
        pass

    def killExecuter(self):
        try:
            appendLog("Attempting executer shutdown on: " + HOST_SUFIX)
            
            import os
            import signal

            # nuke all your child and yourself!
            os.killpg(os.getpgrp(), signal.SIGKILL)
        except Exception, err:
            appendLog("Unclean shutdown, Error is: " + err.__str__())
            
    def initExecuter(self):
        pass

    def getInputFileName(self):
        return (self.getInputFileNameSansExtension() + ".inp")

    def getInputFileNameSansExtension(self):
        return (self.outputPrefix + "/" + self.inputPrefix + HOST_SUFIX)

    def getInputName(self):
        inpName = self.inputPrefix + HOST_SUFIX
        
        return (inpName[0:len(inpName)])

    def getOutputFileName(self):
        return (self.getInputFileNameSansExtension() + ".out")
        
    def getGuessDMFileName(self):
        return (self.getInputFileNameSansExtension() + "-guess.dm")

    def getFinalDMFileName(self):
        return (self.getInputFileNameSansExtension() + "-final.dm")

    def getDMFileName(self):
        return (self.getInputFileNameSansExtension() + "-pij")

    def getOverlapFileName(self):
        return (self.getInputFileNameSansExtension() + "-sij")

    def getFockFileName(self):
        return (self.getInputFileNameSansExtension() + "-fij")

    def getGradientFileName(self):
        return (self.getInputFileNameSansExtension() + "-grdfil")

    def getAllGradientFileName(self):
        return (self.getInputFileNameSansExtension() + "-allgrd")

    def getFCMFileName(self):
        return (self.getInputFileNameSansExtension() + "-fcmfil")

    def getDATFileName(self):
        return (self.getInputFileNameSansExtension() + ".dat")

    def getDDMFileName(self):
        return (self.getInputFileNameSansExtension() + "-ddmfil")

    def getOUTFileName(self):
        return (self.getInputFileNameSansExtension() + ".out")

    def getPointChargesFileName(self):
        return (self.getInputFileNameSansExtension() + "-pcharges")

    def getEnergyFileName(self):
        return (self.getInputFileNameSansExtension() + "-energy.out")

    def getAtomsFileName(self):
        return (self.getInputFileNameSansExtension() + "-atoms.out")

    def getContractionsFileName(self):
        return (self.getInputFileNameSansExtension() + "-contractions.out")
    
    def isCorrectVersion(self, fin):
        # read in the verion number
        version = fin.readline()

        # and return the responce
        return (string.find(PLUGIN_VERSION, string.strip(version)) >= 0)
        
    def readFragmentInput(self, fin):
        # the first line is the input prefix        
        self.setInputPrefix(string.strip(fin.readline()))

        inpFile = self.getInputFileName()

        appendLog("Writing input file: " + inpFile)
        
        # and then the fragment data .. write it into
        # an appropriate file
        thejob = open(inpFile, "w")
        while 1:
            line = fin.readline()
           
            if not line: break

            if (line.find(DATA_OVER) >= 0): break
            
            thejob.write(line)

        thejob.close()

        appendLog("Writing input file over: " + inpFile)

    def readGuessDM(self, fin):
        # read the guess DM if available
        readDM = string.atoi(fin.readline())
        
        if (readDM == 1):
            readFile(self.getGuessDMFileName(), fin, "w")

    def executeJob(self, fin, fos, sendStatus=1):


        gamCmd = self.gamserverRoot + "/" + GAMESS_CMD + \
                  self.outputPrefix + " " + \
                  self.getInputName() + \
                  REDIRECTION_ERR_OPERATOR + \
                  self.getOutputFileName()


       
                 
        appendLog("Executing: " + gamCmd)
        status = os.system(gamCmd)
        appendLog("Execution over, status: " + repr(status))
                 
        # send a notification to the client that we are done
        if (sendStatus):
            if (status == 0):
                fos.write(GAMESS_JOB_OVER)
                fos.flush()
            else:
                fos.write(GAMESS_JOB_KILLED)
                fos.flush()
            
                # close the stream and connection
                fos.close()
                fin.close()

                appendLog("Job was killed abnormally! : " + gamCmd)
            
        return status

    def sendPickedDM(self, fos):
        writeFile(self.getDMFileName(), fos)
    
    def sendPickedOverlap(self, fos):
        writeFile(self.getOverlapFileName(), fos)
    
    def sendPickedFock(self, fos):
        writeFile(self.getFockFileName(), fos)
    
    def sendFullDM(self, fos):
        writeFile(self.getFinalDMFileName(), fos)

    def sendFileIfPresent(self, fos, fileName):
        try:
          open(fileName, "r").close()
         
          fos.write(GAMESS_YES + '\n')
          fos.flush() 
          writeFile(fileName, fos)

          return 1
        except:
          fos.write(GAMESS_NO + '\n')
          fos.flush()

          return 0
          
    def sendGradients(self, fos):
        if (self.sendFileIfPresent(fos, self.getGradientFileName())):
            writeFile(self.getAllGradientFileName(), fos)
    
    def sendFCM(self, fos):
        self.sendFileIfPresent(fos, self.getFCMFileName())
            
    def sendDAT(self, fos):
        self.sendFileIfPresent(fos, self.getDATFileName())
            
    def sendDDM(self, fos):
        self.sendFileIfPresent(fos, self.getDDMFileName())
            
    def sendPointCharges(self, fos):
        self.sendFileIfPresent(fos, self.getPointChargesFileName())

    def grepEnergy(self):
        ene = open(self.getEnergyFileName(), "r")
        energy = string.strip(ene.readline())
        ene.close()

        return energy

    def grepAtoms(self):
        grpAtmsCmd = GREP_ATOMS_CMD + self.getOutputFileName() \
                     + REDIRECTION_OPERATOR + self.getAtomsFileName()

        os.system(grpAtmsCmd)

        atms = open(self.getAtomsFileName(), "r")
        atoms = string.split(atms.readline())[5]
        atms.close()
    
        return atoms

    def grepContractions(self):
        grpContrCmd = GREP_CONTRACTIONS_CMD + self.getOutputFileName() \
                     + REDIRECTION_OPERATOR + self.getContractionsFileName()

        os.system(grpContrCmd)

        cntrs = open(self.getContractionsFileName(), "r")
        contractions = string.split(cntrs.readline())[2]
        cntrs.close()
    
        return contractions
    
    def sendEnergy(self, fos):
        fos.write(self.grepEnergy() + '\n')
        fos.flush()
    
    def sendAtomsAndContractions(self, fos):
        fos.write(self.grepAtoms() + '\n')
        fos.flush()
        fos.write(self.grepContractions() + '\n')
        fos.flush()
    
    def sendOUT(self, fos):
        self.sendFileIfPresent(fos, self.getOUTFileName())
    
# .............................................................


# .............................................................            
#
# The thread class for "keep alive" messages
# Warning: We can run only one keep alive thread pre node,
# consecutively it also means that only one instance of GamExecuter 
# can be started on one node. This may be problematic if you intend
# to use it in a queuing system and the system allocates processor 
# blocks on same node. However, the "0" socket work-around may work,
# if it doesn't you will have to change the port numbers assigned
# in gamconstants.py 
class KeepAliveThread(Thread):
    def __init__(self, executer, useStandardPort):
        Thread.__init__(self)

        self.__executer = executer
        self.__useStandardPort = useStandardPort

        # start the service
        self.theSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

        if (self.__useStandardPort > RESERVED_PORTS):
            self.theSocket.bind((PLUGIN_HOST, self.__useStandardPort))
        elif (self.__useStandardPort):
            self.theSocket.bind((PLUGIN_HOST, PLUGIN_KEEPALIVE_PORT))
        else:               
            self.theSocket.bind((PLUGIN_HOST, PLUGIN_ANY_PORT))

        self.__isServiceOn = 1

    def obtainPortNumber(self):
        return self.theSocket.getsockname()[1]
    
    def run(self):
        self.startKeepAliveServer()

    def startKeepAliveServer(self):
        appendLog("Starting keep alive server on: " + HOST_SUFIX)
        
        # start an endless loop of "beat-request" and "beat-reply"
        while self.__isServiceOn:
          try:
            # listen till we get some client
            self.theSocket.listen(1)
            conn, addr = self.theSocket.accept()

            # open o/p & i/p stream
            fos = conn.makefile("w")
            fis = conn.makefile("r")

            msg = string.strip(fis.readline())

            if (msg.find(PLUGIN_SERVER_KILL_MSG) >= 0):
                appendLog("Received kill message on: " + HOST_SUFIX)
                
                # we have received a message (order ?) to nuke ourselves
                fos.close()
                fis.close()
                conn.close()

                # nuke the executer
                self.__executer.killExecuter()

                # should never come here!
                return
            
            # send the keep alive message
            fos.write(PLUGIN_SERVER_KEEPALIVE_MSG + '\n')
            fos.flush()

            # close the stream and connection
            fos.close()

            conn.close()
          except Exception, err:
            traceback.print_exc(file=sys.stderr)
            appendLog("Cannot send heart beat! : " + err.__str__())          
            traceback.print_exc(file=sys.stderr)

        self.theSocket.close()

        appendLog("Keep alive server on: " + HOST_SUFIX + " stopped.")

    def stopIt(self): 
        self.__isServiceOn = 0
 
        try:
           self.theSocket.close()
        except:
           pass

        appendLog("stopIt(): Keep alive server on: " \
                  + HOST_SUFIX + " stopped.")
# .............................................................


# .............................................................
#
# The default Gam executer works as a "push" receiving 
# agent. Means it waits for the gamscheduler to push 
# jobs into it, executes it and then sends back the 
# result. It is implemented as a single threaded 
# compute server ... for obvious reasons ;) 	
class DefaultGamExecuter(GamExecuter):
    def __init__(self, useStandardPort=0):
        GamExecuter.__init__(self, useStandardPort)

    def initExecuter(self):
        # start the service
        self.theSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM) 

        if (self._useStandardPort > RESERVED_PORTS):
            self.theSocket.bind((PLUGIN_HOST, self._useStandardPort))
        elif (self._useStandardPort):
            self.theSocket.bind((PLUGIN_HOST, PLUGIN_SERVER_PORT))
        else:               
            self.theSocket.bind((PLUGIN_HOST, PLUGIN_ANY_PORT))

        self.theKeepAliveThread = KeepAliveThread(self, self._useStandardPort)
        self.__isServiceOn = 1

        # socket i/o streams
        self.theOutPut = None
        self.theInput  = None

        # the hostname to which we are currently connected
        self.peerName = None

        # and the socket
        self.peerSock = None 
        
    def obtainPortNumbers(self):
        return (self.theSocket.getsockname()[1], \
                self.theKeepAliveThread.obtainPortNumber())

    def cancelJob(self):
        try:
            self.theSocket.close()
            self.theInput.close()
            self.theOutPut.close()
        except:
            pass
        
    def runExecuter(self):
         self.theKeepAliveThread.start()
         
         # start an endless loop of "wait-recieve-execute-send" cycle
         while (self.__isServiceOn):
             try:
                 # listen till we get some client
                 self.theSocket.listen(1)
                 conn, addr = self.theSocket.accept()

                 self.peerSock = conn
                 
                 self.peerName = conn.getpeername()[0]

                 # open i/p stream connection
                 fin = conn.makefile("r")
                 # open o/p stream
                 fos = conn.makefile("w")

                 self.theInput  = fin
                 self.theOutPut = fos
        
                 # clean up the mess before you start
                 cleanupExecuter()

                 # make sure that we are all same version
                 if (not self.isCorrectVersion(fin)):
                     # problem...
                     fos.write(GAMESS_NO + '\n')
                     fos.flush()

                     self.cancelJob()

                     # and log it
                     appendLog("Cannot proceed with job request from: " \
                                + self.peerName + ". Reason: incompatible version!")
                 else:
                     fos.write(GAMESS_YES + '\n')
                     fos.flush()
                 
                 # else... we can do something about it!
                 # first read in the input name
                 self.readFragmentInput(fin)
                     
                 # then obtain all auxilary information
                 # needed to run the job further

                 # i.e. read the guess DM file if available
                 self.readGuessDM(fin)

                 appendLog("DM Reading over: " + self.inputPrefix)
                 
                 # and clean it up in the end too
                 cleanupExecuter()

                 appendLog("Starting execution: " + self.inputPrefix)
                           
                 # then execute the job
                 status = self.executeJob(fin, fos)

                 # if there was something wrong, we abort
                 # and proceed to the next request
                 if (status != 0):
                     conn.close()
                     continue

                 appendLog("Execution over for: " + self.inputPrefix)
                 
                 appendLog("Sending output to: " + self.peerName)
                 
                 # if ok, then we send the required o/p
                 self.sendPickedDM(fos)
                 self.sendPickedOverlap(fos)
                 self.sendPickedFock(fos)
                 self.sendFullDM(fos)
                 self.sendGradients(fos)
                 self.sendFCM(fos)
                 self.sendPointCharges(fos)
                 self.sendEnergy(fos)
                 self.sendAtomsAndContractions(fos)

                 # then shutdown this connection
                 self.peerSock.close()
                 self.theInput.close()
                 self.theOutPut.close()

                 appendLog("Job " + self.inputPrefix + " for " \
                           + self.peerName + " over.")
             except Exception, err:
                 traceback.print_exc(file=sys.stderr)
                 
                 appendLog("Cannot execute job : " + err.__str__())
                 
                 try:
                     if (self.theInput  != None): self.theInput.close()
                     if (self.theOutPut != None): self.theOutPut.close()
                     if (self.peerSock  != None): self.peerSock.close()
                 except:
                     pass                 
                 
# .............................................................


# .............................................................
#
# This @Home model is similar to the way SETI@Home works.
# The implementation here is that of a "pull" model, where
# the compute server pulls jobs from the gamsechedulers. 
class AtHomeGamExecuter(GamExecuter):
    def __init__(self, useStandardPort=1):
        GamExecuter.__init__(self, useStandardPort)

    def initExecuter(self):
        # start the service
        self.theSocket = None

        # socket i/o streams
        self.theOutPut = None
        self.theInput  = None

        # the hostname to which we are currently connected
        self.peerName = None

        # and the socket
        self.peerSock = None         
    
    def obtainPortNumbers(self):
        pass

    def cancelJob(self):
        try:
            self.theSocket.close()
            self.theInput.close()
            self.theOutPut.close()
        except:
            pass
        
    def runExecuter(self):
        while(1):
            try:                
                self.theSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM) 
                self.theSocket.connect((AT_HOME_SERVER_ADDRESS, AT_HOME_SERVER_PORT))
                
                # open i/p stream connection
                fin = self.theSocket.makefile("r")
                # open o/p stream
                fos = self.theSocket.makefile("w")

                self.theInput  = fin
                self.theOutPut = fos

                # make sure that we are all same version
                if (not self.isCorrectVersion(fin)):
                    # problem...
                    fos.write(GAMESS_NO + '\n')
                    fos.flush()

                    self.cancelJob()

                    # and log it
                    appendLog("Cannot proceed with job request from: " \
                                + self.peerName + ". Reason: incompatible version!")
                else:
                    fos.write(GAMESS_YES + '\n')
                    fos.flush()
                     
                # clean up the mess before you start
                cleanupExecuter()

                # send in the request
                fos.write(QUERY_JOB)
                fos.flush()

                responce = fin.readline()
                
                if (string.find(GAMESS_YES, string.strip(responce)) < 0):
                    fos.close()
                    fin.close()
                    self.theSocket.close()

                    sleep(SERVER_SLEEP_SPAN)
                    continue
      
                # first read in the input name
                self.readFragmentInput(fin)
                     
                # then obtain all auxilary information
                # needed to run the job further
                
                # i.e. read the guess DM file if available
                self.readGuessDM(fin)
                
                appendLog("DM Reading over: " + self.inputPrefix)

                # finally read in the job ID
                jobid = fin.readline()
                appendLog("The Job ID is " + jobid)
        
                appendLog("Closing connection with server.")
                # then shutdown this connection
                self.theSocket.close()
                fos.close()
                fin.close()
                
                # and clean it up in the end too
                cleanupExecuter()

                appendLog("Starting execution: " + self.inputPrefix)
                           
                # then execute the job, but do not send status result
                status = self.executeJob(fin, fos, 0)

                appendLog("Execution over. Re-establishing connection with server.")
                
                # then reopen the connection
                self.theSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM) 
                self.theSocket.connect((AT_HOME_SERVER_ADDRESS, AT_HOME_SERVER_PORT))
                
                # open i/p stream connection
                fin = self.theSocket.makefile("r")
                # open o/p stream
                fos = self.theSocket.makefile("w")

                self.theInput  = fin
                self.theOutPut = fos

                # make sure that we are all same version
                if (not self.isCorrectVersion(fin)):
                    # problem...
                    fos.write(GAMESS_NO + '\n')
                    fos.flush()

                    self.cancelJob()

                    # and log it
                    appendLog("Cannot proceed with job request from: " \
                                + self.peerName + ". Reason: incompatible version!")
                else:
                    fos.write(GAMESS_YES + '\n')
                    fos.flush()
                                     
                # if there was something wrong, we abort
                # and proceed to the next request
                # send a notification to the client that we are done
                if (status == 0):
                    fos.write(JOB_COMPLETE)
                    fos.flush()
                else:
                    appendLog("Job " + self.inputPrefix + " was killed!")
                    fos.write(JOB_KILLED)
                    fos.flush()

                    # close the stream and connection
                    self.cancelJob()
                    continue                

                appendLog("Execution over for: " + self.inputPrefix)                
                appendLog("Sending output for Job ID: " + jobid)
                
                # write out the job id!
                fos.write(string.strip(jobid) + "\n")
                fos.flush()

                responce = fin.readline()

                appendLog("Is the Job valid? " + string.strip(responce)  + '\n')

                if (string.find(GAMESS_YES, string.strip(responce)) < 0):
                    appendLog("Job is invalid!! : " + jobid + '\n')
                    self.cancelJob()
                    continue                
                 
                # if ok, then we send the required o/p
                self.sendPickedDM(fos)
                self.sendPickedOverlap(fos)
                self.sendPickedFock(fos)
                self.sendFullDM(fos)
                self.sendGradients(fos)
                self.sendFCM(fos)
                self.sendPointCharges(fos)
                self.sendEnergy(fos)
                self.sendAtomsAndContractions(fos)

                # then shutdown this connection
                self.theSocket.close()
                fos.close()
                fin.close()

                appendLog("Job " + self.inputPrefix + " over.")                
            except Exception, err:
                sleep(SERVER_SLEEP_SPAN)
                
                if (err.__str__().find("Connection refused") >= 0):
                    continue
        
                appendLog("Cannot execute job : " + err.__str__())
                

 
# .............................................................
#
# The PlugIn Gam Executer 
# @ Anuja
# Used for MTA-PlguIn application with GAMESS at the back end
#
class PlugInGAMESSGamExecuter(GamExecuter):
    def __init__(self, useStandardPort=0):
        GamExecuter.__init__(self, useStandardPort)

    def initExecuter(self):
        # start the service
        self.theSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM) 

        if (self._useStandardPort > RESERVED_PORTS):
            self.theSocket.bind((PLUGIN_HOST, self._useStandardPort))
        elif (self._useStandardPort):
            self.theSocket.bind((PLUGIN_HOST, PLUGIN_SERVER_PORT))
        else:               
            self.theSocket.bind((PLUGIN_HOST, PLUGIN_ANY_PORT))

        self.theKeepAliveThread = KeepAliveThread(self, self._useStandardPort)
        self.__isServiceOn = 1

        # socket i/o streams
        self.theOutPut = None
        self.theInput  = None

        # the hostname to which we are currently connected
        self.peerName = None

        # and the socket
        self.peerSock = None 
        
    def obtainPortNumbers(self):
        return (self.theSocket.getsockname()[1], \
                self.theKeepAliveThread.obtainPortNumber())

    def readFragmentInput(self, fin):
        # the first line is the input prefix        
        self.setInputPrefix(string.strip(fin.readline()))

        inpFile = self.getInputFileName()

        appendLog("Writing input file: " + inpFile)

        # and then the fragment data .. write it into
        # an appropriate file
        thejob = open(inpFile, "w")
        while 1:
            line = fin.readline()

            if not line: break

            if (line.find(DATA_OVER) >= 0): break

            thejob.write(line)

        thejob.close()

        appendLog("Writing input file over: " + inpFile)



    def executeJob(self, fin, fos, sendStatus=1):
#        gamCmd =  GAMESS_CMD + \
#                  self.outputPrefix + " " + \
#                  self.getInputName() +  \
#                  REDIRECTION_ERR_OPERATOR + \
#                  self.getOutputFileName()
        gamCmd =  GAMESS_CMD + \
                  self.outputPrefix + " " + \
                  self.getInputName() +  " 00 4 " +\
                  REDIRECTION_ERR_OPERATOR + \
                  self.getOutputFileName()
                 
        appendLog("Executing: " + gamCmd)
        status = os.system(gamCmd)
        appendLog("Execution over, status: " + repr(status))
                 
        # send a notification to the client that we are done
        if (sendStatus):
            if (status == 0):
                fos.write(GAMESS_JOB_OVER)
                fos.flush()
            else:
                fos.write(GAMESS_JOB_KILLED)
                fos.flush()
            
                # close the stream and connection
                fos.close()
                fin.close()

                appendLog("Job was killed abnormally! : " + gamCmd)
            
        return status


    def getEnergyFileName(self):
        return (self.getInputFileNameSansExtension() + ".out")

    def getDatFileName(self):
        return (self.getInputFileNameSansExtension() + ".dat")


    def grepEnergy(self):
        ene = open(self.getEnergyFileName(), "r")
        olines = ene.readlines()
        ene.close()

        for eline in olines:
          if (eline.find("FINAL RHF ENERGY") >= 0):
            energy = eline.split()[4]
          elif (eline.find("E(MP2") >= 0 ):
            energy = eline.split()[1]
          elif ( (eline.find("FINAL") >= 0 ) and (eline.find("ENERGY") >= 0) ):
            energy = eline.split()[4]

        return energy

    def grepGradients(self):
        datfl = open(self.getDatFileName(),"r")
        datlines = datfl.readlines()
        datfl.close()

        gradlines = []
        for i in range (0,len(datlines)):
          if (datlines[i].find("$GRAD") >= 0 ): break;
        for j in range (i+2,len(datlines)):
          if (datlines[j].find("$END") >= 0): break;
          gradlines.append(datlines[j])

        gradfl = open(self.getAllGradientFileName(), "w")
        for grad in  gradlines:
          gradfl.write(grad)
        gradfl.close()

    def sendGradients(self, fos):
        if (self.sendFileIfPresent(fos, self.getAllGradientFileName())):
            writeFile(self.getAllGradientFileName(), fos)

    def grepHessian(self):
        datfl = open(self.getDatFileName(),"r")
        datlines = datfl.readlines()
        datfl.close()

        hesslines = []
        for i in range (0,len(datlines)):
          if (datlines[i].find("$HESS") >= 0 ): break;
        for j in range (i+2,len(datlines)):
          if (datlines[j].find("$END") >= 0): break;
          hesslines.append(datlines[j])

        hessfl = open(self.getFCMFileName(), "w")
        for hess in  hesslines:
          hessfl.write(hess)
        hessfl.close()
    

    def cancelJob(self):
        try:
            self.theSocket.close()
            self.theInput.close()
            self.theOutPut.close()
        except:
            pass
        
    def runExecuter(self):
         self.theKeepAliveThread.start()
         
         # start an endless loop of "wait-recieve-execute-send" cycle
         while (self.__isServiceOn):
             try:
                 # listen till we get some client
                 self.theSocket.listen(1)
                 conn, addr = self.theSocket.accept()

                 self.peerSock = conn
                 
                 self.peerName = conn.getpeername()[0]

                 # open i/p stream connection
                 fin = conn.makefile("r")
                 # open o/p stream
                 fos = conn.makefile("w")

                 self.theInput  = fin
                 self.theOutPut = fos
        
                 # clean up the mess before you start
                 cleanupExecuter()

                 # make sure that we are all same version
                 if (not self.isCorrectVersion(fin)):
                     # problem...
                     fos.write(GAMESS_NO + '\n')
                     fos.flush()

                     self.cancelJob()

                     # and log it
                     appendLog("Cannot proceed with job request from: " \
                                + self.peerName + ". Reason: incompatible version!")
                 else:
                     fos.write(GAMESS_YES + '\n')
                     fos.flush()
                 
                 # else... we can do something about it!
                 # first read in the input name
                 self.readFragmentInput(fin)

                     
                 # then obtain all auxilary information
                 # needed to run the job further

                 # i.e. read the guess DM file if available
#REMOVE                 self.readGuessDM(fin)

#REMOVE                 appendLog("DM Reading over: " + self.inputPrefix)
                 
                 # and clean it up in the end too
                 cleanupExecuter()

                 appendLog("Starting execution: " + self.inputPrefix)
                           
         
                 # then execute the job
                 status = self.executeJob(fin, fos)

                 # if there was something wrong, we abort
                 # and proceed to the next request
                 if (status != 0):
                     conn.close()
                     continue

                 appendLog("Execution over for: " + self.inputPrefix)
                 
                 # grep gradients and prepare -allgrd
                 self.grepGradients()

                 # grep Hessian and prepare -fcmfil
                 self.grepHessian()

# TEMPORARY FOR F, G AND H
#                 os.system(" cat ~/jobs/final.dm ~/jobs/final.h1 ~/jobs/final.g ~/jobs/final.fck  >>  " + self.getDATFileName())
                 os.system(" cat " + self.getDMFileName() + "  >>  " + self.getDATFileName())
                 os.system(" cat " + self.getFockFileName() + "  >>  " + self.getDATFileName())
                 appendLog("Sending output to: " + self.peerName)
                 
                 # if ok, then we send the required o/p
#REMOVE                 self.sendPickedDM(fos)
#REMOVE                 self.sendPickedOverlap(fos)
#REMOVE                 self.sendPickedFock(fos)
#REMOVE                 self.sendFullDM(fos)
                 self.sendGradients(fos)
                 self.sendFCM(fos)
                 self.sendDAT(fos)
#REMOVE                 self.sendPointCharges(fos)
                 self.sendEnergy(fos)
                 self.sendAtomsAndContractions(fos)
                 self.sendOUT(fos)

                 # then shutdown this connection
                 self.peerSock.close()
                 self.theInput.close()
                 self.theOutPut.close()

                 appendLog("Job " + self.inputPrefix + " for " \
                           + self.peerName + " over.")
             except Exception, err:
                 traceback.print_exc(file=sys.stderr)
                 
                 appendLog("Cannot execute job : " + err.__str__())
                 
                 try:
                     if (self.theInput  != None): self.theInput.close()
                     if (self.theOutPut != None): self.theOutPut.close()
                     if (self.peerSock  != None): self.peerSock.close()
                 except:
                     pass                 
                 
# .............................................................
# .............................................................
#
# The PlugIn Gam Executer 
# @ Anuja
# Used for MTA-PlguIn application with GAUSSIAN at the back end
#
class PlugInGAUSSIANGamExecuter(GamExecuter):
    def __init__(self, useStandardPort=0):
        GamExecuter.__init__(self, useStandardPort)

    def initExecuter(self):
        # start the service
        self.theSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM) 

        if (self._useStandardPort > RESERVED_PORTS):
            self.theSocket.bind((PLUGIN_HOST, self._useStandardPort))
        elif (self._useStandardPort):
            self.theSocket.bind((PLUGIN_HOST, PLUGIN_SERVER_PORT))
        else:               
            self.theSocket.bind((PLUGIN_HOST, PLUGIN_ANY_PORT))

        self.theKeepAliveThread = KeepAliveThread(self, self._useStandardPort)
        self.__isServiceOn = 1

        # socket i/o streams
        self.theOutPut = None
        self.theInput  = None

        # the hostname to which we are currently connected
        self.peerName = None

        # and the socket
        self.peerSock = None 
        
    def obtainPortNumbers(self):
        return (self.theSocket.getsockname()[1], \
                self.theKeepAliveThread.obtainPortNumber())

    def getInputFileName(self):
        return (self.getInputFileNameSansExtension() + ".inp")

    def readFragmentInput(self, fin):
        # the first line is the input prefix        
        self.setInputPrefix(string.strip(fin.readline()))

        inpFile = self.getInputFileName()

        appendLog("Writing input file: " + inpFile)

        # and then the fragment data .. write it into
        # an appropriate file
        thejob = open(inpFile, "w")
        while 1:
            line = fin.readline()

            if not line: break

            if (line.find(DATA_OVER) >= 0): break

#            appendLog(line)
            thejob.write(line)

        thejob.close()

        appendLog("Writing input file over: " + inpFile)


    def getInputName(self):
        inpName = self.inputPrefix + HOST_SUFIX + ".inp"
        
        return (inpName[0:len(inpName)])

    def getEnergyFileName(self):
        return (self.getInputFileNameSansExtension() + ".out")

#    def getOutputFileName(self):
#        return (self.getInputFileNameSansExtension() + ".out")
        

    def executeJob(self, fin, fos, sendStatus=1):
        gaussCmd = GAUSS_CMD + \
                  self.outputPrefix + self.getInputName() + " " + \
                  self.getOutputFileName()  
                 
        appendLog("Executing: " + gaussCmd)
        status = os.system(gaussCmd)
        appendLog("Execution over, status: " + repr(status))

        # A Gaussian single-point energy-grdient step does not terminate normally
        chk = open (self.getOutputFileName(),"r")
        chklines = chk.readlines()
        chk.close()
        
        for chkline in chklines:
          if (chkline.find("Forces (Hartrees/Bohr)")):
             status = 0
             break;
                 
        # send a notification to the client that we are done
        if (sendStatus):
            if (status == 0):
                fos.write(GAUSS_JOB_OVER)
                fos.flush()
            else:
                fos.write(GAUSS_JOB_KILLED)
                fos.flush()
            
                # close the stream and connection
                fos.close()
                fin.close()

                appendLog("Job was killed abnormally! : " + gaussCmd)
            
        return status


    def grepEnergy(self):
#        ene = open(self.getEnergyFileName(), "r")
        ene = open(self.getOutputFileName(), "r")
        olines = ene.readlines()
        ene.close()

        isCounterpoise = 0
        for o in olines:
           if (o.upper().find("COUNTERPOISE") >= 0): isCounterpoise = 1

        for eline in olines:
          if (not isCounterpoise):
            if (eline.find("E(RHF)") >= 0):
              energy = eline.split()[4]
            # For any functional in DFT:
            elif (eline.find("SCF Done") >= 0):
              energy = eline.split()[4]
            elif (eline.find("EUMP2") >= 0 ):
              eneword = eline.split("=")[2].split("D")
              energy = repr(string.atof(eneword[0])*(10**string.atoi(eneword[1])))
             # for B2PLYPD FUNCTIONAL 
            elif (eline.find("E(B2PLYPD)") >= 0 ):
              eneword = eline.split("=")[2].split("D")
              energy = repr(string.atof(eneword[0])*(10**string.atoi(eneword[1])))
            elif (eline.find("CCSD(T)= ") >= 0 ):
              eneword = eline.split("=")[1].split("D")
              energy = repr(string.atof(eneword[0])*(10**string.atoi(eneword[1])))
          elif (isCounterpoise):
            if (eline.find("Counterpoise: corrected energy") >= 0):
              energy = eline.split()[4]

        return energy


    def grepGradients(self):

        datfl = open(self.getOutputFileName(),"r")
        datlines = datfl.readlines()
        datfl.close()

        gradlines = []
        for i in range (0,len(datlines)):
          if (datlines[i].find("Forces (Hartrees/Bohr)") >= 0 ): break;
        for j in range (i+3,len(datlines)):
          if (datlines[j].find("-----") >= 0): break;
          words = datlines[j].split()
          gradl = AtmSymbol[string.atoi(words[1])] + "          "  + words[1] + "   " + words[2] + "   " + words[3] + "   " + words[4] + "\n"
          gradlines.append(gradl)

        gradfl = open(self.getAllGradientFileName(), "w")
        for grad in  gradlines:
          gradfl.write(grad)
        gradfl.close()

    def sendGradients(self, fos):
        if (self.sendFileIfPresent(fos, self.getAllGradientFileName())):
            writeFile(self.getAllGradientFileName(), fos)
    
    def cancelJob(self):
        try:
            self.theSocket.close()
            self.theInput.close()
            self.theOutPut.close()
        except:
            pass
        
    def grepContractions(self):
        grpContrCmd = GAUSS_GREP_CONTRACTIONS_CMD + self.getOutputFileName() \
                     + REDIRECTION_OPERATOR + self.getContractionsFileName()

        os.system(grpContrCmd)

        cntrs = open(self.getContractionsFileName(), "r")
        contractions = string.split(cntrs.readline())[1]
        cntrs.close()
    
        return contractions
    
    def grepAtoms(self):
        grpAtmsCmd = GAUSS_GREP_ATOMS_CMD + self.getOutputFileName() \
                     + REDIRECTION_OPERATOR + self.getAtomsFileName()

        os.system(grpAtmsCmd)

        atms = open(self.getAtomsFileName(), "r")
        atoms = string.split(atms.readline())[1]
        atms.close()
    
        return atoms

    def runExecuter(self):
         self.theKeepAliveThread.start()
         
         # start an endless loop of "wait-recieve-execute-send" cycle
         while (self.__isServiceOn):
             try:
                 # listen till we get some client
                 self.theSocket.listen(1)
                 conn, addr = self.theSocket.accept()

                 self.peerSock = conn
                 
                 self.peerName = conn.getpeername()[0]

                 # open i/p stream connection
                 fin = conn.makefile("r")
                 # open o/p stream
                 fos = conn.makefile("w")

                 self.theInput  = fin
                 self.theOutPut = fos
        
                 # clean up the mess before you start
                 cleanupExecuter()

                 # make sure that we are all same version
                 if (not self.isCorrectVersion(fin)):
                     # problem...
                     fos.write(GAUSS_NO + '\n')
                     fos.flush()

                     self.cancelJob()

                     # and log it
                     appendLog("Cannot proceed with job request from: " \
                                + self.peerName + ". Reason: incompatible version!")
                 else:
                     fos.write(GAUSS_YES + '\n')
                     fos.flush()
                 
                 # else... we can do something about it!
                 # first read in the input name
                 self.readFragmentInput(fin)

                     
                 # then obtain all auxilary information
                 # needed to run the job further

                 # i.e. read the guess DM file if available
#REMOVE                 self.readGuessDM(fin)

#REMOVE                 appendLog("DM Reading over: " + self.inputPrefix)
                 
                 # and clean it up in the end too
                 cleanupExecuter()

                 appendLog("Starting execution: " + self.inputPrefix)
                           
         
                 # then execute the job
                 status = self.executeJob(fin, fos)

                 # if there was something wrong, we abort
                 # and proceed to the next request
                 if (status != 0):
                     conn.close()
                     continue

                 appendLog("Execution over for: " + self.inputPrefix)
                 
                 # grep gradients and prepare -allgrd
                 self.grepGradients()

                 appendLog("Sending output to: " + self.peerName)
                 
                 # if ok, then we send the required o/p
#REMOVE                 self.sendPickedDM(fos)
#REMOVE                 self.sendPickedOverlap(fos)
#REMOVE                 self.sendPickedFock(fos)
#REMOVE                 self.sendFullDM(fos)
                 self.sendGradients(fos)
#REMOVW                 self.sendFCM(fos)
#REMOVE                 self.sendPointCharges(fos)
                 self.sendEnergy(fos)
                 self.sendAtomsAndContractions(fos)
                 self.sendOUT(fos)

                 # then shutdown this connection
                 self.peerSock.close()
                 self.theInput.close()
                 self.theOutPut.close()

                 appendLog("Job " + self.inputPrefix + " for " \
                           + self.peerName + " over.")
             except Exception, err:
                 traceback.print_exc(file=sys.stderr)
                 
                 appendLog("Cannot execute job : " + err.__str__())
                 
                 try:
                     if (self.theInput  != None): self.theInput.close()
                     if (self.theOutPut != None): self.theOutPut.close()
                     if (self.peerSock  != None): self.peerSock.close()
                 except:
                     pass                 
                 
# .............................................................
