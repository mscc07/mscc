##
## gamnodeallocater.py
##
## This is the new API for writing schedulers for CG-MTA GAMESS.
## This has been written due to the need to have various schedulars
## when performing an CG-MTA run on various kind of clusters
## and parallel machines.
## The node allocater API is specifically written to handle various
## types of queuing mechinishms on a variety of HPC cluster setup.
##
## This code was written at CSIT, ANU, Canberra
##
## @author V.Ganesh
## @date 18 Oct 2006
##

import sys
import socket

#
# The class GamNodeList offers a means to obtain Nodes
# (as IP address or names) that may participate in 
# a compute activity, and that may have an instance of
# gamexecuter running on them. This is the top level
# class that defines the interfaces required from its
# implementors.  
class GamNodeList:
    def __init__(self):
        self.nodeList = [] 

    def getNodeList(self):
        return self.nodeList

#
# This class defines the interfaces needed to classify 
# as to how the nodes ("processors that represent a node")
# into a physical "node" entity that is usually a shared
# memory system of some nodes that are interfaced using 
# tight and fast network.  
class GamNodeAllocater:
    def __init__(self, gnl):
        self.gamNodeList = gnl 
        self.processorPerNode = {}
        self.processorNamePerNode = {}
        self.hasDifferentProcessorNames = 0

    def getHasDifferentProcessorNames(self):
        return self.hasDifferentProcessorNames
 
    def getProcessorNamePerNode(self):
        return self.processorNamePerNode

    def getProcessorPerNode(self):
        return self.processorPerNode 

#
# The manual list, where the nodes are statically allocated
# and no messing around the queing system... you are the king. 
class ManualGamNodeList(GamNodeList):
    def __init__(self, nl):
        GamNodeList.__init__(self)
        self.nodeList = nl 

#
# The manual list, where the nodes are statically allocated
# and no messing around the queing system... you are the king. 
# This wrapper is useful when you have a shared memory system.
class SingleGamNodeList(GamNodeList):
    def __init__(self, nodeName, n):
        GamNodeList.__init__(self)
        self.nodeList = []

        for i in range(0, n): self.nodeList.append(nodeName)

import os
import string

#
# The manual list, where the nodes are statically allocated
# and no messing around the queing system... you are the king.
# The node names are specified in a file name (hopefull a
# fully qualified name)
class ManualFileGamNodeList(GamNodeList):
    def __init__(self, fl):
        GamNodeList.__init__(self)
        try:
            sf = open(fl, "r")
            lst = sf.readlines()
            sf.close()

            for i in range(len(lst)-1, -1, -1): 
                self.nodeList.append(string.strip(lst[i]))
        except:
            self.nodeList = [socket.gethostname()]
        
#
# The PBS batch scheduler generally writes the node names in 
# a file represented by environment variable PBS_NODEFILE.
class PBSGamNodeList(GamNodeList):
    def __init__(self):
        GamNodeList.__init__(self)

        try:
            sf = open(os.environ['PBS_NODEFILE'], "r")
            lst = sf.readlines()
            sf.close()

            for i in range(len(lst)-1, -1, -1): 
                self.nodeList.append(string.strip(lst[i]))
        except:
            self.nodeList = [socket.gethostname()]

#
# The LFS queue manager generally provides the list of allocated
# nodes in environment variable LFS_HOSTS
class LFSGamNodeList(GamNodeList):
    def __init__(self):
        GamNodeList.__init__(self)

        try:
            self.nodeList = string.split(os.environ['LSF_HOSTS'])
        except:
            self.nodeList = [socket.gethostname()]

#
# The load leveller (LL) stores the allocated processor list in
# environment variable LOADL_PROCESSOR_LIST
class LLGamNodeList(GamNodeList):
    def __init__(self):
        GamNodeList.__init__(self)

        try:
            self.nodeList = string.split(os.environ['LOADL_PROCESSOR_LIST'])
        except:
            self.nodeList = [socket.gethostname()]

#
# The default just considers every node to have only one processor
class DefaultGamNodeAllocater(GamNodeAllocater):
    def __init__(self, gnl):
        GamNodeAllocater.__init__(self, gnl)
         
        for node in self.gamNodeList.getNodeList():
            self.processorPerNode[node] = 1

#
# Cluster up nodes which are non-unique into one.
# The set of these nodes (unique) mostly represent a shared memory
# system from where a GAMESS parallel run can be made.
class ClusteredNodeGamNodeAllocater(GamNodeAllocater):
    def __init__(self, gnl):
        GamNodeAllocater.__init__(self, gnl)

        nl = self.gamNodeList.getNodeList()
        uniqueNl = []
        
        for n in nl:
           if (uniqueNl.count(n) == 0):
              uniqueNl.append(n)

        for n in uniqueNl:
            self.processorPerNode[n] = nl.count(n)             

#
# Consider a group of n node from the node list to be a virtual 
# cluster. The set of these nodes mostly represent nodes with 
# different CPU boxes networked by a good network connection.
# Format: gnl: node list
#         sl : size list for clustering nodes
class GroupClusteredNodeGamNodeAllocater(GamNodeAllocater):
    def __init__(self, gnl, sl):
        GamNodeAllocater.__init__(self, gnl)

        self.hasDifferentProcessorNames = 1

        nl = self.gamNodeList.getNodeList()
        siz = len(nl)
        uniqueNl = []

        if (sum(sl) < siz):
           print "WARNING: Not all nodes will be used!\n"
        
        if (sum(sl) > siz):
           print "ERROR: You requested more nodes than available!\n"
           sys.exit(10)
    
        ii = 0; k = 0 
        for ii in range(0, len(sl)): 
          n = sl[ii]
          uniqueNl.append(nl[k])

          self.processorNamePerNode[nl[k]] = []
          for j in range(k, k+n):
             self.processorNamePerNode[nl[k]].append(nl[j])

          k += n

        ii = 0
        for i in uniqueNl:
            self.processorPerNode[i] = sl[ii]
            ii += 1

