##
## gamclient.py
##
## GAMESS network I/O utils
##
## 18th June 2004 (10 days after venus transit ;))
##
## @author: V.Ganesh
##

import string

from gamconstants import *

#
# readFile() - reads a files over a network strem
#
# @param fileName : the file name on the local machine to which
#                   file read over the network is to be written
# @param fin : the stream connected to the server
# @param mode: the mode string "a+", "w" etc...
def readFile(fileName, fin, mode):
    fil = open(fileName, mode)
    noOfLine = string.atoi(fin.readline())
    for i in range(0, noOfLine):
        fil.write(fin.readline())
    fil.close()    
    return

#
# writeFile() - write a files over a network strem
#
# @param fileName : the file name on the local machine which
#                   is to be written over to the network
# @param fos : the stream connected to the client, that needs to
#              be pumped with this file data
def writeFile(fileName, fos):
    fil = open(fileName, "r")
    lines = fil.readlines()
    fil.close()
        
    fos.write(repr(len(lines)) + "\n")
    fos.flush()
    for line in lines:
        fos.write(line)
        fos.flush()
    fos.flush()
    return

#
# readBinFile() - reads a binary files over a network strem
#
# @param fileName : the file name on the local machine to which
#                   file read over the network is to be written
# @param fin : the stream connected to the server
# @param mode: the mode string "ab+", "wb" etc...
def readBinFile(fileName, fin, mode):
    fil = open(fileName, mode)

    while 1:
        data = fin.read(UPDATE_BUFFER_SIZE)

        if (not data): break
        
        fil.write(data)
        
    fil.close()    
    return

#
# writeBinFile() - write a binary files over a network strem
#
# @param fileName : the file name on the local machine which
#                   is to be written over to the network
# @param fos : the stream connected to the client, that needs to
#              be pumped with this file data
def writeBinFile(fileName, fos):
    fil = open(fileName, "rb")    
    
    while 1:
        data = fil.read(UPDATE_BUFFER_SIZE)

        if (not data):
            fos.write(data)
            break
        
        fos.write(data)
        fos.flush()
        
    fos.flush()

    fil.close()
    
    return

