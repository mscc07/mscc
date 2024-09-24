#!/usr/bin/env python3

import time
import os
import sys
from shutil import copyfile
from setup import readInput
from MCCI import performMCCI



start = time.time()

model, nSite, subSpace, nStates, s2Target, maxItr, startSpinTargetItr, energyTola, spinTola, beta, jVal, det, Ms,  posibleDet, bondOrder, outputfile, restart, saveBasis  = readInput()

newline = ("\nTotal Posible Determinats are %d .\nBreakup are [Ms, No of Determinants] - ")% (sum(posibleDet))
with open (outputfile, "a") as fout:
    fout.write(newline)

for i in range(len(Ms)):
    newline = ("\t[%d, %d]")%(Ms[i], posibleDet[i]) 
    with open(outputfile, "a") as fout:
        fout.write(newline)
        if (i+1 == len(Ms)):
            fout.write("\n\n")

if ( subSpace > (sum(posibleDet) *0.8)):
    sys.exit("Sub-Space size is more than 80 % of total determinants space. Make Sub-Space size smaller and run it again.\n ")

performMCCI()

newline = ("Total Time Taken in MCCI Calculation is %f sec.")%( time.time() - start )
with open(outputfile, "a") as fout:
    fout.write(newline)
