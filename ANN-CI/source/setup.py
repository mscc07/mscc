import time
import os
import sys
from shutil import copyfile
import csv
import math
from math import factorial as fact
from bitstring import BitArray 

inputfile = sys.argv[1]

def readInput():

    
    outputfile = str(inputfile) + '.out'
    
    copyfile(inputfile, outputfile)

    fin = open(inputfile,"r")
    lines = fin.readlines()
    length = len(lines)
    restart = False
    saveBasis = 'nothing.dat'
    for i in range(length):
        toks = lines[i].split(",")
        if len(toks) >= 2:

            if toks[0] == 'model':
                model = toks[1].strip()

            if toks[0] == 'nSite':
                nSite = int(toks[1])
            
            if toks[0] == 'subSpace':
                subSpace = int(toks[1])

            if toks[0] == 'nStates':
                nStates = int(toks[1])

            if toks[0] == 's2Target':
                s2Target = float(toks[1])

            if toks[0] == 'maxItr':
                maxItr = int(toks[1])

            if toks[0] == 'startSpinTargetItr':
                startSpinTargetItr = int(toks[1])

            if toks[0] == 'energyTola':
                energyTola = float(toks[1])

            if toks[0] == 'spinTola':
                spinTola = float(toks[1])

            if toks[0] == 'beta':
                beta = float(toks[1])
            
            if toks[0] == 'bondOrder':
                bondOrder = str(toks[1]).strip()

            if toks[0] == 'jValue':
                jVal = -float(toks[1])

            if toks[0] == 'restart':
                if toks[1] == 'True':
                    restart = True
                    saveBasis = str(toks[2]).strip()

            
            if toks[0] == 'Ms':
                noOfMs = int(toks[1])
                det = []
                posibleDet = []
                Ms = []
                for j in range(noOfMs):
                    ms = int( toks[2 + j ])
                    up = int((nSite/2)  + ms)
                    down = nSite - up
                    det0 = BitArray(down)
                    num = fact(nSite)/(fact(nSite - up) * fact(up) )
                    for k in range(up):
                        det0 = det0 + '0b1'                        
                    det.append(det0)
                    Ms.append(ms)
                    posibleDet.append(num)
                        
    return model, nSite, subSpace, nStates, s2Target, maxItr, startSpinTargetItr, energyTola, spinTola, beta, jVal, det, Ms,  posibleDet, bondOrder, outputfile, restart, saveBasis 
