#                                        MTA-PLUGIN                                         #
#                                       MAIN PROGRAM                                        #
#                                UPDATED:   20 MAY 2019                                     #
#                                AUTHORS: GANESH, ANUJA, SUBODH			            #

import os
import sys
import math
import time
import string
import fpformat
import multiprocessing

from math import *
from datetime import datetime

from time import *

import gamserver.gamconstants
from gamserver.gamconstants import *
import gamserver.gamclient
from gamserver.gamclient import *
import gamserver.bootgamserver
from gamserver.bootgamserver import *
import gamserver.shutdowngamserver
from gamserver.shutdowngamserver import *
import gamserver.gamscheduler
from gamserver.gamscheduler import *

import mtaplugin
import mtainp
import patch
import misc
import socket


def CheckMassFile( inpPrefix):
	filecheck= inpPrefix +"-mass"

	if os.path.isfile(filecheck) :
        	fp = open ( filecheck,"r")
	        data = fp.readlines()
        	fp.close()
	        for line in data:
        	        if len(line.split())==0: continue
                	if len(line.split())!=2:
                        	print "Please provide atom numner and mass seperated by space"
	                        sys.exit()
		return True	
	else:
        	return False


def ReadInputFromUser():


        print " \n\nPL ENTER THE FOLLOWING DETAILS FOR LOWER LEVEL COMPUTATION \n\n"
        Level_Lower = raw_input(" Enter Level of Theory  (HF/DFT/MP2) ==> ")
        Basis_Lower = raw_input(" Enter Basis Set(6-31G/3-21G/6-31+G etc.) ==> ")
        print " \n\nPL ENTER FOLLOWING DETAILS FOR HIGHER LEVEL COMPUTATION \n\n"
        Basis_higher = raw_input(" Enter Basis Set(6-31G/3-21G/6-31+G etc.) ==> ")
	
	return Level_Lower.strip(), Basis_Lower.strip(), Basis_higher.strip()


def mta(inpFileName, Run, outFileName):

   inpPrefix = inpFileName.split(".")[0]

   # Removing all the fragment input files of previous run
   for i in range(1,10000):
      file = inpPrefix + "-frag" + repr(i).zfill(4) + ".inp"
      flag = os.path.isfile(file)
      if flag : os.system (FILE_DEL_CMD + "  " + file )


   os.system("export OUTPNM=" + OUTPUT_PREFIX + outFileName)
   os.putenv("OUTNM", OUTPUT_PREFIX + outFileName)
   os.system("export INPNM=" + OUTPUT_PREFIX + inpPrefix)
   os.putenv("INPNM", OUTPUT_PREFIX + inpPrefix)

#  CHECKING THE KEYWORDS AMONG IRGRAFT, RAMANGRAFT, FRGONL
   misc.checkRun(Run, outFileName)

#  CHECKING FOR PROPER XYZ FILE
   misc.checkXYZInputFile(inpFileName)

   curr_time = asctime(localtime(time()))
   misc.writeLines(outFileName, "Job Submission Time :: " + curr_time   + "\n" )
   misc.writeLines(outFileName, "Inputfile name      :: " + inpFileName + "\n" )
   misc.writeLines(outFileName, "Outputfile name     :: " + outFileName + "\n" )
   misc.writeLines(outFileName, "Keyword             :: " + Run         + "\n" )

#  CHECKING FOR FragParam File 
   if not os.path.isfile(OUTPUT_PREFIX + 'FragParam') : 
	print "FragParam file is missing in the folder " + OUTPUT_PREFIX
	sys.exit()

# CHECKING FOR Atomic mass file
   if ( CheckMassFile( inpPrefix) ):
   	misc.writeLines(outFileName, "Atomic mass will read from the file  " +   inpPrefix      + "-mass\n" )
   
   lines = misc.readLines("FragParam")
   fpfragparam = open("FragParam","w")
   for li in lines:
       if len(li.split())==0:continue
       fpfragparam.write(li)
   fpfragparam.close() 
   lines = misc.readLines("FragParam")

   keyValues = {}
   for i in range(0, len(lines)):
           line = lines[i].strip()
           w = line.split("=")
           keyValues[w[0].upper()] = w[1].upper()

   if len(keyValues) != 5 : print "some keywords Entries are missing in FragParam file"
   if not ("MAXSZ"  in keyValues):  print "MAXSZ parameter is missing from FragParam file "
   if not ("MINSZ"  in keyValues):  print "MINSZ parameter is missing from FragParam file "
   if not ("RADCUT" in keyValues):  print "RADCUT parameter is missing from FragParam file "
   if not ("RDEXT"  in keyValues):  print "RDEXT parameter is missing from FragParam file "
   if not ("ADDUM"  in keyValues):  print "ADDUM parameter is missing from FragParam file "

   misc.writeLines(outFileName, "\n\n-------FragParam-----\n" )
   for line in lines:
         misc.writeLines(outFileName, line )
   misc.writeLines(outFileName, "-----------------\n\n" )

   StartTime = time()

   GaussianFlag = misc.checkGaussian()
   if  not GaussianFlag:   
       print GAUSSIANCOMAND + "\tis not installed"
       sys.exit()

   if (Run.upper() == 'FRGONL'):
	Level_Lower,Basis_Lower,Basis_higher = ReadInputFromUser()
	INPFileName = misc.CreateInputFile(inpFileName,Level_Lower,Basis_Lower)
      	fragment(INPFileName, outFileName, CODE)
      	misc.writeLines(OUTPUT_PREFIX + outFileName, "EXITING AFTER FRAGMENTATION")
#-----------------------------

   elif (Run.upper() == "MTAGRAFT"):

	print "MTA GRAFTING RUN"
	Level_Lower,Basis_Lower,Basis_higher = ReadInputFromUser()
        Level_higher = Level_Lower

	#Running Actual Job at Lower Level
	curr_time = asctime(localtime(time()))
	misc.writeLines(outFileName, "Full Calculation (Lower) job Start Time :: " + curr_time + "\n" )
	status,HFEne,E2Ene  = CalculateEnergyAtLower(inpFileName, Level_Lower, Basis_Lower) 
	ENERGY_ACT = HFEne + E2Ene
        misc.writeLines(OUTPUT_PREFIX + outFileName, "---------------------------------------\n")
	misc.writeLines(OUTPUT_PREFIX + outFileName, "  Level of theory == " + Level_Lower +"/"+Basis_Lower + "\n")
	misc.writeLines(OUTPUT_PREFIX + outFileName, "  HF   ENERGY == "     + repr(round(HFEne,6)) + "\n")
	misc.writeLines(OUTPUT_PREFIX + outFileName, "  E(2) ENERGY == "     + repr(round(E2Ene,6)) + "\n")
	misc.writeLines(OUTPUT_PREFIX + outFileName, "  ENERGY == "          + repr(round(ENERGY_ACT,6)) + "\n")
        misc.writeLines(OUTPUT_PREFIX + outFileName, "---------------------------------------\n")

	curr_time = asctime(localtime(time()))
	misc.writeLines(outFileName, "Full Calculation (Lower) job End Time :: " + curr_time + "\n" )

	#Running MTA Job at  lower Level

	curr_time = asctime(localtime(time()))
	misc.writeLines(outFileName, "MTA Calculation (Lower) start Time :: " + curr_time + "\n" )
    	Geometry = misc.readGeometryfromXYZ(inpFileName)
	InputFileName = misc.CreateInputFile( inpFileName,Level_Lower,Basis_Lower )

	fragment(InputFileName, outFileName, CODE)
	run(inpPrefix, outFileName)

       	MTAEnergy = patch.patchEnergy(inpPrefix, CODE, OUTPUT_PREFIX + outFileName)

	ENERGY_MTA_Lower = MTAEnergy

	curr_time = asctime(localtime(time()))
	misc.writeLines(outFileName, "MTA Lower end Time :: " + curr_time + "\n" )

	#Running MTA Job at Higher
	InputFileName = misc.CreateInputFile( inpFileName,Level_higher,Basis_higher )
	
        if ( status == 0 ):
		curr_time = asctime(localtime(time()))
		misc.writeLines(outFileName, "MTA Calculation (Higher) start Time :: " + curr_time + "\n" )

		fragment(InputFileName, outFileName, CODE)
		run(inpPrefix, outFileName)

		curr_time = asctime(localtime(time()))
		misc.writeLines(outFileName, "Fragment Run Over :: " + curr_time + "\n" )
         	MTAEnergy = patch.patchEnergy(inpPrefix, CODE, OUTPUT_PREFIX + outFileName)
		ENERGY_MTA_Heigher = MTAEnergy
		ENERGY_FINAL =  string.atof(ENERGY_ACT) - string.atof(ENERGY_MTA_Lower) + string.atof(ENERGY_MTA_Heigher)

        	misc.writeLines(OUTPUT_PREFIX + outFileName, "---------------------------------------\n")
		misc.writeLines(OUTPUT_PREFIX + outFileName, " MTA GRAFTED ENERGY == "+ repr(round(ENERGY_FINAL,6))+ "\n")
        	misc.writeLines(OUTPUT_PREFIX + outFileName, "---------------------------------------\n")
        	misc.writeLines(OUTPUT_PREFIX + outFileName, "\n\n\n")
        	misc.writeLines(OUTPUT_PREFIX + outFileName, "---------------------------------------\n")
		misc.writeLines(OUTPUT_PREFIX + outFileName, "             SUMMARY             "  + "\n")
		nproc = multiprocessing.cpu_count()
		misc.writeLines(OUTPUT_PREFIX + outFileName, "Machine Name     ==> "+ socket.gethostname()  + "\n")
		misc.writeLines(OUTPUT_PREFIX + outFileName, "Shared processors ==> "+ repr(nproc) + "\n")
		misc.writeLines(OUTPUT_PREFIX + outFileName, "FC ["+ Level_Lower +"/"+Basis_Lower+ "] ==> "+fpformat.fix(ENERGY_ACT,5)+ "\n")
		misc.writeLines(OUTPUT_PREFIX + outFileName, "MTA["+ Level_Lower +"/"+Basis_Lower+ "] ==> "+fpformat.fix(ENERGY_MTA_Lower,5)+ "\n")
		mtacorrection = ENERGY_ACT -  ENERGY_MTA_Lower
		misc.writeLines(OUTPUT_PREFIX + outFileName, "CORRECTION["+ Level_Lower +"/"+Basis_Lower+ "] ==> "+fpformat.fix(mtacorrection,5)+ "\n")
		misc.writeLines(OUTPUT_PREFIX + outFileName, "MTA["+ Level_higher +"/"+Basis_higher+ "] ==> "+fpformat.fix(ENERGY_MTA_Heigher,5)+ "\n")
		misc.writeLines(OUTPUT_PREFIX + outFileName, "GMTA["+ Level_higher +"/"+Basis_higher+ "] ==> "+fpformat.fix(ENERGY_FINAL,5)+ "(Overall grafting)\n")
        	misc.writeLines(OUTPUT_PREFIX + outFileName, "---------------------------------------\n")

#----------------

   elif (Run.upper() == "RAMANGRAFT"):

        print "\nMTA RAMAN-GRAFTING RUN\n"

	Level_Lower,Basis_Lower,Basis_higher = ReadInputFromUser()
        Level_higher = Level_Lower



        #Running Actual Job at Lower Level

        curr_time = asctime(localtime(time()))
	misc.writeLines(outFileName, "FULL COMPUTATION (LOWER LEVEL) START TIME :: " + curr_time + "\n" )
        status,ENERGY_ACT = CalculateRamanAtLower(inpFileName, Level_Lower, Basis_Lower)
        ENERGY_ACT = float (ENERGY_ACT)
	patch.grepActHessian (inpFileName)
	patch.grepActDipoleDerivative(inpFileName)
	patch.grepActPolDer(inpFileName)
        misc.writeLines(OUTPUT_PREFIX + outFileName, "---------------------------------------\n")
        misc.writeLines(OUTPUT_PREFIX + outFileName, " ACTUAL ENERGY == " + repr(round(ENERGY_ACT,6)) + "\n")
        misc.writeLines(OUTPUT_PREFIX + outFileName, "---------------------------------------\n")
        curr_time = asctime(localtime(time()))
	misc.writeLines(outFileName, "FULL COMPUTATION (LOWER LEVEL) END TIME :: " + curr_time + "\n" )


        if ( status == 0 ):
        #Running MTA Job at Lower Level
	        curr_time = asctime(localtime(time()))
	        misc.writeLines(outFileName, "MTA-RAMAN COMPUTATION (LOWER LEVEL) START TIME :: " + curr_time + "\n" )
		InputFileName = misc.CreateRAMANInputFile ( inpFileName,Level_Lower,Basis_Lower )
		fragment(InputFileName, outFileName, CODE)
	        run(inpPrefix, outFileName)
		DoMtaRAMANRun(inpPrefix, CODE, OUTPUT_PREFIX , outFileName)
		os.system("cat " + OUTPUT_PREFIX +  inpPrefix + "-final.hess > " + OUTPUT_PREFIX + inpPrefix + "-final-lowerMTA.hess")
		os.system("cat " + OUTPUT_PREFIX +  inpPrefix + "-final.dipdr > " + OUTPUT_PREFIX + inpPrefix + "-final-lowerMTA.dipdr")
		os.system("cat " + OUTPUT_PREFIX +  inpPrefix + "-final.poldr > " + OUTPUT_PREFIX + inpPrefix + "-final-lowerMTA.poldr")
                MTAEnergy = patch.patchEnergy(inpPrefix, CODE, OUTPUT_PREFIX + outFileName)
                ENERGY_MTA_Lower = MTAEnergy
	        curr_time = asctime(localtime(time()))
	        misc.writeLines(outFileName, "MTA-RAMAN COMPUTATION (LOWER LEVEL) END TIME :: " + curr_time + "\n" )

        #Running MTA Job at Heigher
                curr_time = asctime(localtime(time()))
	        misc.writeLines(outFileName, "MTA-RAMAN COMPUTATION (HIGHER LEVEL) START TIME :: " + curr_time + "\n" )
	        InputFileName = misc.CreateRAMANInputFile( inpFileName,Level_higher,Basis_higher )
                fragment(InputFileName, outFileName, CODE)
                run(inpPrefix, outFileName)
                DoMtaRAMANRun(inpPrefix, CODE, OUTPUT_PREFIX , outFileName)
                curr_time = asctime(localtime(time()))
	        misc.writeLines(outFileName, "MTA-RAMAN COMPUTATION (HIGHER LEVEL) END TIME :: " + curr_time + "\n" )
                os.system("cat " + OUTPUT_PREFIX +  inpPrefix + "-final.hess > " + OUTPUT_PREFIX + inpPrefix + "-final-higherMTA.hess")
                os.system("cat " + OUTPUT_PREFIX +  inpPrefix + "-final.dipdr > " + OUTPUT_PREFIX + inpPrefix + "-final-higherMTA.dipdr")
                os.system("cat " + OUTPUT_PREFIX +  inpPrefix + "-final.poldr > " + OUTPUT_PREFIX + inpPrefix + "-final-higherMTA.poldr")
                MTAEnergy = patch.patchEnergy(inpPrefix, CODE, OUTPUT_PREFIX + outFileName)
                ENERGY_MTA_Heigher = MTAEnergy
                ENERGY_FINAL =  string.atof(ENERGY_ACT) - string.atof(ENERGY_MTA_Lower) + string.atof(ENERGY_MTA_Heigher)

		patch.graftHessian ( inpPrefix )
		patch.graftDipDr( inpPrefix )
		patch.graftPolDr( inpPrefix )
	
                misc.writeLines(OUTPUT_PREFIX + outFileName, "---------------------------------------\n")
                misc.writeLines(OUTPUT_PREFIX + outFileName, " MTA GRAFTED ENERGY == " + repr(round(ENERGY_FINAL,6)) + "\n")
                misc.writeLines(OUTPUT_PREFIX + outFileName, "---------------------------------------\n")


                Status = DoGauVisRun ( inpFileName, Level_higher, Basis_higher )
                if (Status == 0):
                        patch.calculateGraftedRAMANGAUFormat( OUTPUT_PREFIX + inpPrefix )

	
                VISFileName  = inpFileName.split(".")[0] + "-VIS.log"
		print VISFileName


#***************************************************
                misc.writeLines(OUTPUT_PREFIX + outFileName, "\n\n\n")
                misc.writeLines(OUTPUT_PREFIX + outFileName, "---------------------------------------\n")
                misc.writeLines(OUTPUT_PREFIX + outFileName, "             SUMMARY             "  + "\n")
                nproc = multiprocessing.cpu_count()
                misc.writeLines(OUTPUT_PREFIX + outFileName, "Machine Name     ==> "+ socket.gethostname()  + "\n")
                misc.writeLines(OUTPUT_PREFIX + outFileName, "Shared processors ==> "+ repr(nproc) + "\n")
                misc.writeLines(OUTPUT_PREFIX + outFileName, "FC ["+ Level_Lower +"/"+Basis_Lower+ "] ==> "+fpformat.fix(ENERGY_ACT,5)+ "\n")
                misc.writeLines(OUTPUT_PREFIX + outFileName, "MTA["+ Level_Lower +"/"+Basis_Lower+ "] ==> "+fpformat.fix(ENERGY_MTA_Lower,5)+ "\n")
                mtacorrection = ENERGY_ACT -  ENERGY_MTA_Lower
                misc.writeLines(OUTPUT_PREFIX + outFileName, "CORRECTION["+ Level_Lower +"/"+Basis_Lower+ "] ==> "+fpformat.fix(mtacorrection,5)+ "\n")
                misc.writeLines(OUTPUT_PREFIX + outFileName, "MTA["+ Level_higher +"/"+Basis_higher+ "] ==> "+fpformat.fix(ENERGY_MTA_Heigher,5)+ "\n")
                misc.writeLines(OUTPUT_PREFIX + outFileName, "GMTA["+ Level_higher +"/"+Basis_higher+ "] ==> "+fpformat.fix(ENERGY_FINAL,5)+ "(Overall grafting)\n")
                misc.writeLines(OUTPUT_PREFIX + outFileName, "---------------------------------------\n")

#***************************************************



   elif (Run.upper() == "IRGRAFT"):


        print "\nMTA IR-GRAFTING RUN\n"

	Level_Lower,Basis_Lower,Basis_higher = ReadInputFromUser()
        Level_higher = Level_Lower

        #Running Actual Job at Lower Level

        curr_time = asctime(localtime(time()))
	misc.writeLines(outFileName, "FULL COMPUTATION (LOWER LEVEL) START TIME :: " + curr_time + "\n" )
        status,ENERGY_ACT = CalculateFrequencyAtLower(inpFileName, Level_Lower, Basis_Lower)
	ENERGY_ACT = float (ENERGY_ACT)
	patch.grepActHessian (inpFileName)
	patch.grepActDipoleDerivative(inpFileName)
        misc.writeLines(OUTPUT_PREFIX + outFileName, "---------------------------------------\n")
        misc.writeLines(OUTPUT_PREFIX + outFileName, " ACTUAL ENERGY == " + repr(round(ENERGY_ACT,5)) + "\n")
        misc.writeLines(OUTPUT_PREFIX + outFileName, "---------------------------------------\n")
        curr_time = asctime(localtime(time()))
	misc.writeLines(outFileName, "FULL COMPUTATION (LOWER LEVEL) END TIME :: " + curr_time + "\n" )

        if ( status == 0 ):
        #Running MTA Job at Lower Level
		InputFileName = misc.CreateFREQInputFile ( inpFileName,Level_Lower,Basis_Lower )
	        curr_time = asctime(localtime(time()))
	        misc.writeLines(outFileName, "MTA-IR COMPUTATION (LOWER LEVEL) START TIME :: " + curr_time + "\n" )
		fragment(InputFileName, outFileName, CODE)
	        run(inpPrefix, outFileName)
		DoMtaFrequencyRun(inpPrefix, CODE, OUTPUT_PREFIX , outFileName)
		os.system("cat " + OUTPUT_PREFIX +  inpPrefix + "-final.hess > " + OUTPUT_PREFIX + inpPrefix + "-final-lowerMTA.hess")
		os.system("cat " + OUTPUT_PREFIX +  inpPrefix + "-final.dipdr > " + OUTPUT_PREFIX + inpPrefix + "-final-lowerMTA.dipdr")
                MTAEnergy = patch.patchEnergy(inpPrefix, CODE, OUTPUT_PREFIX + outFileName)
                ENERGY_MTA_Lower = MTAEnergy
	        curr_time = asctime(localtime(time()))
	        misc.writeLines(outFileName, "MTA-IR COMPUTATION (LOWER LEVEL) END TIME :: " + curr_time + "\n" )

        #Running MTA Job at Heigher
	        InputFileName = misc.CreateFREQInputFile( inpFileName,Level_higher,Basis_higher )
                curr_time = asctime(localtime(time()))
	        misc.writeLines(outFileName, "MTA-IR COMPUTATION (HIGHER LEVEL) START TIME :: " + curr_time + "\n" )
                fragment(InputFileName, outFileName, CODE)
                run(inpPrefix,outFileName)
                DoMtaFrequencyRun(inpPrefix, CODE, OUTPUT_PREFIX , outFileName)
                os.system("cat " + OUTPUT_PREFIX +  inpPrefix + "-final.hess > " + OUTPUT_PREFIX + inpPrefix + "-final-higherMTA.hess")
                os.system("cat " + OUTPUT_PREFIX +  inpPrefix + "-final.dipdr > " + OUTPUT_PREFIX + inpPrefix + "-final-higherMTA.dipdr")
                curr_time = asctime(localtime(time()))
	        misc.writeLines(outFileName, "MTA-IR COMPUTATION (HIGHER LEVEL) END TIME :: " + curr_time + "\n" )


                MTAEnergy = patch.patchEnergy(inpPrefix, CODE, OUTPUT_PREFIX + outFileName)
                ENERGY_MTA_Heigher = MTAEnergy
                ENERGY_FINAL =  string.atof(ENERGY_ACT) - string.atof(ENERGY_MTA_Lower) + string.atof(ENERGY_MTA_Heigher)

                misc.writeLines(OUTPUT_PREFIX + outFileName, "---------------------------------------\n")
                misc.writeLines(OUTPUT_PREFIX + outFileName, " MTA GRAFTED ENERGY == "+repr(round(ENERGY_FINAL,6))+"\n")
                misc.writeLines(OUTPUT_PREFIX + outFileName, "---------------------------------------\n")
		patch.graftHessian ( inpPrefix )
		patch.graftDipDr( inpPrefix )
	
	        Status = DoGauVisRun ( inpFileName, Level_higher, Basis_higher )
		if (Status == 0):
			patch.calculateGraftedFrquencyGAUFormat( OUTPUT_PREFIX + inpPrefix )

		VISFileName  = inpFileName.split(".")[0] + "-VIS.log"
#***************************************************
                misc.writeLines(OUTPUT_PREFIX + outFileName, "\n\n\n")
                misc.writeLines(OUTPUT_PREFIX + outFileName, "---------------------------------------\n")
                misc.writeLines(OUTPUT_PREFIX + outFileName, "             SUMMARY             "  + "\n")
                nproc = multiprocessing.cpu_count()
                misc.writeLines(OUTPUT_PREFIX + outFileName, "Machine Name     ==> "+ socket.gethostname()  + "\n")
                misc.writeLines(OUTPUT_PREFIX + outFileName, "Shared processors ==> "+ repr(nproc) + "\n")
                misc.writeLines(OUTPUT_PREFIX + outFileName, "FC ["+ Level_Lower +"/"+Basis_Lower+ "] ==> "+fpformat.fix(ENERGY_ACT,5)+ "\n")
                misc.writeLines(OUTPUT_PREFIX + outFileName, "MTA["+ Level_Lower +"/"+Basis_Lower+ "] ==> "+fpformat.fix(ENERGY_MTA_Lower,5)+ "\n")
                mtacorrection = ENERGY_ACT -  ENERGY_MTA_Lower
                misc.writeLines(OUTPUT_PREFIX + outFileName, "CORRECTION["+ Level_Lower +"/"+Basis_Lower+ "] ==> "+fpformat.fix(mtacorrection,5)+ "\n")
                misc.writeLines(OUTPUT_PREFIX + outFileName, "MTA["+ Level_higher +"/"+Basis_higher+ "] ==> "+fpformat.fix(ENERGY_MTA_Heigher,5)+ "\n")
                misc.writeLines(OUTPUT_PREFIX + outFileName, "GMTA["+ Level_higher +"/"+Basis_higher+ "] ==> "+fpformat.fix(ENERGY_FINAL,5)+ "(Overall grafting)\n")
                misc.writeLines(OUTPUT_PREFIX + outFileName, "File for Gauss View  ==> "+ VISFileName + "\n")
                misc.writeLines(OUTPUT_PREFIX + outFileName, "---------------------------------------\n")

#***************************************************


	else:
        	print "Error in running the Job. Aborting "
	        sys.exit()

   curr_time = asctime(localtime(time()))
   misc.writeLines(outFileName, "Job Completion Time :: " + curr_time + "\n" )
   EndTime = time()
   ElapsedTime = (EndTime - StartTime)/60.0
   misc.writeLines(outFileName, "Total Elapsed Time :: " + repr(round(ElapsedTime,2)) + " Minutes\n" )

def DoGauVisRun(inpFileName, Level_higher, Basis_higher ):

	InputFileName = misc.CreateGauVisFile( inpFileName,Level_higher,Basis_higher )
	status = os.system ( GAUSS_CMD + " " + InputFileName)
	return status


def DoMtaRAMANRun ( inpPrefix, CODE, OUTPUT_PREFIX , outFileName):
         MTAEnergy = patch.patchEnergy(inpPrefix, CODE, OUTPUT_PREFIX + outFileName)
         Grads, GMax, GRMS = patch.patchGradients(inpPrefix, CODE, OUTPUT_PREFIX + outFileName)
         Hess, massHess = patch.patchHessian(inpPrefix, CODE, OUTPUT_PREFIX + outFileName)
         DipDer, DipDr = patch.patchTensor(inpPrefix, CODE, OUTPUT_PREFIX + outFileName, 'Dipole')
         PolarDer, PolarDr = patch.patchTensor(inpPrefix, CODE, OUTPUT_PREFIX + outFileName, 'Polarizability')

def DoMtaFrequencyRun (inpPrefix, CODE, OUTPUT_PREFIX , outFileName): 


         MTAEnergy = patch.patchEnergy(inpPrefix, CODE, OUTPUT_PREFIX + outFileName)
         Grads, GMax, GRMS = patch.patchGradients(inpPrefix, CODE, OUTPUT_PREFIX + outFileName)
         Hess, massHess = patch.patchHessian(inpPrefix, CODE, OUTPUT_PREFIX + outFileName)
         DipDer, DipDr = patch.patchTensor(inpPrefix, CODE, OUTPUT_PREFIX + outFileName, 'Dipole')


def CalculateEnergyAtLower(inpFileName, Level_Lower, Basis_Lower):

    Geometry = misc.readGeometryfromXYZ(inpFileName)
    ActFileName = inpFileName.split(".")[0] + "-act.inp"
    ChkFileName = inpFileName.split(".")[0] + "-act.chk"

#   Calculating the number of processor and Memory
    nproc = multiprocessing.cpu_count()
    mem_bytes = os.sysconf('SC_PAGE_SIZE') * os.sysconf('SC_PHYS_PAGES')
    mem_gib = int( mem_bytes/(1024.**3) )
    if mem_gib > 100:    mem_gib = 50

    fp = open ( ActFileName, "w" )
    fp.write("%chk="+ChkFileName+"\n%nproc="+ repr(nproc) +"\n%mem="+repr(mem_gib)+"GB\n#P " + Level_Lower + "/" + Basis_Lower + " SP NOSYMM\n\nLower Level Actual job\n\n0 1\n")
    for line in Geometry:
	fp.write(line.strip()+"\n")
    fp.write("\n")
    fp.close()

    status = 0
    status = os.system ( GAUSS_CMD + " " + ActFileName)
    if (status == 0):
	HFEne = 0.0 ; E2Ene=0.0
	lines = misc.readLines(ActFileName.split(".")[0] + ".log")
	#if (Level_Lower.strip().upper() == "MP2"):
	if ( Level_Lower.strip().upper().find("MP2") >=0 ):
		for line in lines:
			 if ( line.find("Done")  >= 0) : 
				HFEne = line.split()[4]
				HFEne = string.atof(HFEne)
			 if ( line.find("EUMP2") >= 0) : 
				E2Ene = line.split()[2].replace ("D","E")
				E2Ene =string.atof(E2Ene)
	else:
		for line in lines:
			 if ( line.find("Done")  >= 0) : 
				HFEne = line.split()[4]
				HFEne = string.atof(HFEne)

    
    os.system("formchk"+ " " + ChkFileName )
    return status, HFEne, E2Ene

def CalculateRamanAtLower(inpFileName, Level_Lower, Basis_Lower):
    Geometry = misc.readGeometryfromXYZ(inpFileName)
    ActFileName = inpFileName.split(".")[0] + "-act.inp"
    ChkFileName = inpFileName.split(".")[0] + "-act.chk"
    fp = open ( ActFileName, "w" )
    nproc= multiprocessing.cpu_count()
    mem_bytes = os.sysconf('SC_PAGE_SIZE') * os.sysconf('SC_PHYS_PAGES')
    mem_gib = int( mem_bytes/(1024.**3) )
    if mem_gib>100:    mem_gib = 50
    fp.write("%nproc="+ repr(nproc)+"\n%chk=" + ChkFileName +"\n%mem="+repr(mem_gib)+"GB\n#P " + Level_Lower + "/" + Basis_Lower + " FREQ(RAMAN) NOSYMM\n\nActual job\n\n0 1\n")
    for line in Geometry:
        fp.write(line.strip()+"\n")
    fp.write("\n")
    fp.close()
    status = 0
    status = os.system ( GAUSS_CMD + " " + ActFileName)
    if (status == 0):
        lines = misc.readLines(ActFileName.split(".")[0] + ".log")
        #if (Level_Lower.strip().upper() == "MP2"):
	if ( Level_Lower.strip().upper().find("MP2") >=0 ):
                EnergyKeyWord = "EUMP2"
                EnergyKeyIdx  = 5
        else:
                EnergyKeyWord = "Done"
                EnergyKeyIdx  = 4
        for line in lines:
                if ( line.find(EnergyKeyWord) >= 0) : break
        Ene = line.split()[EnergyKeyIdx]
        Ene = Ene.replace ("D","E")

    print "formchk"+ " " + inpFileName.split(".")[0] + "-act.chk"
    os.system("formchk"+ " " + inpFileName.split(".")[0] + "-act.chk" )
    return status, Ene


def CalculateFrequencyAtLower(inpFileName, Level_Lower, Basis_Lower):
    Geometry = misc.readGeometryfromXYZ(inpFileName)
    ActFileName = inpFileName.split(".")[0] + "-act.inp"
    ChkFileName = inpFileName.split(".")[0] + "-act.chk"
    fp = open ( ActFileName, "w" )
    nproc= multiprocessing.cpu_count()
    mem_bytes = os.sysconf('SC_PAGE_SIZE') * os.sysconf('SC_PHYS_PAGES')
    mem_gib = int( mem_bytes/(1024.**3) )
    if mem_gib > 100:    mem_gib = 50
    fp.write("%nproc="+ repr(nproc)+"\n%chk=" + ChkFileName +"\n%mem="+repr(mem_gib)+"GB\n#P " + Level_Lower + "/" + Basis_Lower + " FREQ NOSYMM\n\nActual job\n\n0 1\n")
    for line in Geometry:
        fp.write(line.strip()+"\n")
    fp.write("\n")
    fp.close()
    status = 0
    status = os.system ( GAUSS_CMD + " " + ActFileName)
    if (status == 0):
        lines = misc.readLines(ActFileName.split(".")[0] + ".log")
        #if (Level_Lower.strip().upper() == "MP2"):
	if ( Level_Lower.strip().upper().find("MP2") >=0 ):
                EnergyKeyWord = "EUMP2"
                EnergyKeyIdx  = 5
        else:
                EnergyKeyWord = "Done"
                EnergyKeyIdx  = 4
        for line in lines:
                if ( line.find(EnergyKeyWord) >= 0) : break
        Ene = line.split()[EnergyKeyIdx]
        Ene = Ene.replace ("D","E")
    
    print "formchk"+ " " + inpFileName.split(".")[0] + "-act.chk" 
    os.system("formchk"+ " " + inpFileName.split(".")[0] + "-act.chk" )
    return status, Ene

	
def fragment(inpFileName, outFileName, CODE, isOpt = 0):

    try:
      inpPrefix = inpFileName[:len(inpFileName)-4]

      # call read input for given program
      inp = mtainp.readInp(OUTPUT_PREFIX + inpFileName, CODE)

      # dump the geometry in the output file
      if isOpt:
        misc.writeLines(outFileName, "\n\t1NSERCH = " + (misc.readLines('opt-step'))[0].strip() + "\n")
        curr_opt_step = string.atoi(misc.readLines('opt-step')[0])
        if (curr_opt_step > 0):
          paramlines = misc.readLines("FragParam")
          fp = open("FragParam","w")
          #for p in range (0, 4):
          for p in range (0, len(paramlines)):
             if (paramlines[p].upper().find("RDEXT") >= 0) :
                fp.write("RDEXT=1\n")
             else:
                fp.write(paramlines[p])
          fp.close()

      else:
        misc.writeLines(outFileName, '\tInput Geometry\n')
      misc.writeLines(outFileName, "--------------------------------------------------------------------------------\n")
      misc.writeLines(outFileName, " COORDINATES OF ALL ATOMS ARE (ANGS)\n")
      misc.writeLines(outFileName, "    ATOM   CHARGE       X              Y              Z\n")
      misc.writeLines(outFileName, "--------------------------------------------------------------------------------\n")
      #atoms = misc.readGeometryfromXYZ(inpFileName)
      atoms = misc.getGeometryInfo(inpFileName, CODE)
      geom_string = ""


      for i in range (1, len(atoms)+1):
	  geom_string += ('{0:>6s} {1:6d} {2:15.7f} {3:15.7f} {4:15.7f}'.format( atoms[i][0],atoms[i][1],atoms[i][2],atoms[i][3],atoms[i][4] ))+ "\n"

      misc.writeLines(outFileName, geom_string)
      misc.writeLines(outFileName, "**\n-----------------------------------------------------------\n")

      # call the fragmentation
      
      mtaplugin.fragment(inp.isor, inp.ishes, inp.nserch, inp.useDM, inp.dirFrg, inp.useMT,
               inp.rdExt,inp.www, inp.radCut, inp.mplevl, inp.frgetl, inp.frgitl, inp.frgdtl,
               inp.memfrg, inp.addum, inp.useddi, inp.mddifr, inp.frzatm, inp.icntmap,
               inp.frfuz, inp.isdft, inp.selgrd, inp.forcdm, inp.armygd, inp.verbse, inp.helcor,
               inp.setene, inp.setgrd, inp.hydorg, inp.dodiis, inp.pchrg, inp.setdm, inp.setcrg,
               inp.radthr, inp.ctring, inp.strscf, inp.maxvt, inp.isatmgrd, inp.isgrdcl)

      misc.storeGlobIdx(OUTPUT_PREFIX + inpPrefix, CODE)
      generateInpFile(OUTPUT_PREFIX + inpFileName, CODE)

      return 

    except Exception, err:
      misc.writeLines(OUTPUT_PREFIX + outFileName, 'ERROR WHILE FRAGMENTING:\n' + err.__str__()) 
      sys.exit(10)


def run(inpPrefix, outFileName):

    try :
      # start the boot process
#      bootgamserver.bootUpExecuters(inpPrefix + "-")
      bootUpExecuters(inpPrefix + "-")

      # Start gamclient.py
      # for that prepare a dummy senddm file
      f = open(OUTPUT_PREFIX + inpPrefix +"-senddm","w")
      f.write("0")
      f.close()

      misc.writeLines(OUTPUT_PREFIX + outFileName, "\nSTARTING DISTRIBUTED MODE CODE....")
      print "STARTING DISTRIBUTED MODE CODE...."
      # start the client
#      gamclient.startGamScheduler(inpPrefix + "-") 
      startGamScheduler(inpPrefix + "-") 

      misc.writeLines(OUTPUT_PREFIX + outFileName, "DISTRIBUTED MODE OF CODE STOPPED....\n")
      print "DISTRIBUTED MODE OF CODE STOPPED....\n"
#      shutdowngamserver.shutServers(inpPrefix + "-")
      shutServers(inpPrefix + "-")


    except Exception, err:
      misc.writeLines(OUTPUT_PREFIX + outFileName, 'ERROR WHILE RUNNING:\n' + err.__str__()) 


def mtaenergy(coord, inpFileName, outFileName, CODE, opt_step):
   inpPrefix = inpFileName[:len(inpFileName)-4]

   curr_opt_step = string.atoi((misc.readLines('opt-step'))[0].strip())

   # update the geometry
   update(inpPrefix, outFileName, CODE, coord, curr_opt_step)

   fragment(inpFileName, outFileName, CODE, 1)
   run(inpPrefix, outFileName)
   MTAEnergy = patch.patchEnergy(inpPrefix, CODE, OUTPUT_PREFIX + outFileName)

   f = open ('opt-step','w')
   f.write(repr(curr_opt_step + 1))
   f.close()

   return MTAEnergy

def mtagrads(coord, inpFileName, outFileName, CODE, opt_step):
   inpPrefix = inpFileName[:len(inpFileName)-4]

   Grads, GMax, GRMS = patch.patchGradients(inpPrefix, CODE, OUTPUT_PREFIX + outFileName)

   grads = []
   for i in range (0,len(Grads)):
      grads.append(Grads[i+1][2])
      grads.append(Grads[i+1][3])
      grads.append(Grads[i+1][4])

   grads = numpy.array(grads)
   return grads

def update(inpPrefix, outFileName, CODE, coord, opt_step):
   inpFileName = OUTPUT_PREFIX + inpPrefix + ".inp"

#   IMPORTANT
#   if (opt_step < 1): os.system("cp " + OUTPUT_PREFIX +  inpPrefix + ".inp " + OUTPUT_PREFIX + inpPrefix + ".inp.org")
#   if (opt_step > 0): modifyFragRdExt()

   os.system("cat " + OUTPUT_PREFIX +  inpPrefix + ".inp >> " + OUTPUT_PREFIX + inpPrefix + ".inp.org")

   atoms = misc.getGeometryInfo(inpFileName, CODE)
   newGeom = ""
   nat = len(coord)/3
   for i in range (1, nat+1):
      if (CODE == 'GAMESS'):
        newGeom += atoms[i][0] + "   " + repr(atoms[i][1]) + "   " + repr(coord[(i-1)*3]) + "   " + repr(coord[(i-1)*3+1]) + "   " + repr(coord[(i-1)*3+2]) + "\n"
      if (CODE == 'GAUSSIAN'):
        newGeom += repr(atoms[i][1]) + "   " + repr( round(coord[(i-1)*3],6) ) + "   " + repr( round(coord[(i-1)*3+1],6) ) + "   " + repr( round(coord[(i-1)*3+2],6) ) + "\n"

   # grep the template
   template = misc.getTemplate(inpFileName, CODE)
   curr_opt_step = misc.readLines("opt-step")
#   commentLine = "STEP " + repr(opt_step + 1) + " :  " + misc.getCommentLine(inpFileName, CODE)
   commentLine = "STEP " + repr(opt_step) + " :  " + misc.getCommentLine(inpFileName, CODE)

   # write the template and this new geometry in the input file
   inpFile = open (inpFileName, "w")
   if (CODE == 'GAMESS'):
     inpFile.write(template + " $DATA \n" + commentLine + newGeom + " $END")
   elif (CODE == 'GAUSSIAN'):
     charge_mult = misc.getChargeMult(inpFileName)
     inpFile.write(template + "\n" + commentLine + "\n" + charge_mult + "\n" + newGeom + "\n")
   inpFile.close()

   print "UPDATED GEOMETRY"



def generateInpFile(inpFileName, CODE):

   if (CODE == "GAMESS"):

     lines = misc.readLines(inpFileName)
     template = ""

     for i in range (0,len(lines)):
        if (lines[i].upper().find("$DATA") >= 0): break;
        
     for j in range (0,i):
        template += lines[j]

     # If somebody gives keywords after coordinates, which is possible in GAMESS,
     # they should be added in the template ..
     for k in range (i,len(lines)):
        if (lines[k].upper().find("$END") >= 0): break;

     for j in range (k+1, len(lines)):
        template += lines[j]

     inpPrefix = inpFileName[0:len(inpFileName)-4]

     numberOfFrag = misc.getNumberOfFragments(inpPrefix, CODE)

     template1 = template.strip()

     for i in range (1,numberOfFrag+1):
        newFragFile = open('new.inp','w')
        newFragFile.write(template1)
	template ="\n"

        fragFileName = inpPrefix + '-frag' + repr(i).zfill(4) + ".inp"
        fragLines = misc.readLines(fragFileName)
        for i in range (0,len(fragLines)):
           if (fragLines[i].upper().find("DATA") >= 0): break;

        for k in range (i,len(fragLines)):
           if (fragLines[k].upper().find("END") >= 0): break;
           template += fragLines[k]

        template += " $end\n"
       
        FLAG = 0; 
        for fragi in range (0,len(fragLines)):
           #if (fragLines[fragi].upper().find("$DATA") >= 0): break;
           if (fragLines[fragi].upper().find("$data") >= 0): break;
        newFragFile.write(template)
	newFragFile.close()

        os.system("mv new.inp " + fragFileName)
     

   elif (CODE == "GAUSSIAN"):
     lines = misc.readLines(inpFileName)
     inpPrefix = inpFileName[0:len(inpFileName)-4]
     nat = misc.getNumberOfAtoms(inpPrefix+".inp", CODE)


     template = ""
     charge_mult = ""
     isCounterpoise = 0
     isBackgroundCharge = 0
     for i in range (0,len(lines)):
        if (lines[i].find("%chk") < 0):
          if (lines[i][0] == '#'):
            if (lines[i].upper().find("opt") >= 0):
              if (lines[i].upper().find("MAXCYCLE=1") < 0):
                lines[i]= lines[i].strip() + "(MaxCycle=1)\n"

            if (lines[i].upper().find("CHARGE") >= 0): isBackgroundCharge = 1

            if (lines[i].upper().find("COUNTERPOISE") >= 0): 
                isCounterpoise = 1
                tempwords = lines[i].split()
                line = ""
                for t in range (0,len(tempwords)):
                  if (tempwords[t].upper().find("COUNTERPOISE") < 0):
                    line += tempwords[t] + " "
                lines[i] = line + "\n"
            
          template += lines[i]
          if (lines[i][0] == '#'):
             # double check for correct #-line 
             if (len(lines[i+4].split()) == 2): charge_mult = lines[i+4]; break;

     if (isBackgroundCharge):

        atoms = misc.getGeometryInfo(inpPrefix+".inp", CODE)
        fragGlobIdx = misc.readGlobIdx()

        try:
           bgcharges = misc.readLines(inpPrefix + "-bgcharges")
           BgChrg = {}
           for c in range (0,len(bgcharges)):
              cwords = bgcharges[c].split()
              BgChrg[string.atoi(cwords[0])] = cwords[2]
        except:
           print "Mullikan charges not provided in -bgcharges file .. Aborting .."

     numberOfFrag = misc.getNumberOfFragments(inpPrefix, CODE)

     ffp = open ( inpPrefix +".xyz","r")
     natm=ffp.readline()
     titlefrag = ffp.readline()
     OriGeom  = ffp.readlines()
     ffp.close()
     filecheck= inpPrefix +"-mass"
     isotopmass =[] 
     if os.path.isfile(filecheck) :
     	ffp = open ( filecheck,"r")
	isotopmass = ffp.readlines()
	ffp.close()


     for i in range (1,numberOfFrag+1):
        newFragFile = open('new.inp','w')

        fragFileName = inpPrefix + '-frag' + repr(i).zfill(4) + ".inp"
        #print "Frag-", i, " reading", fragFileName
        fragLines = misc.readLines(fragFileName)
       
        for fragi in range (0,len(fragLines)):
           if (fragLines[fragi].upper().find("$DATA") >= 0): break;

        if (isCounterpoise):
           counter = 0
           for fragj in range (fragi+3,len(fragLines)):
              if (fragLines[fragj].upper().find("$END") >= 0): break;
              counter += 1

           fragMonoFile = inpPrefix + '-frag' + repr(i).zfill(4) + ".mon"
           fcp = open(fragMonoFile,"w")
           fcp.write(repr(counter) + "\n")
           for fragj in range (fragi+3,len(fragLines)):
              if (fragLines[fragj].upper().find("$END") >= 0): break;
              words = fragLines[fragj].split()
              fcp.write(words[0] + "   " + words[2] + "   " + words[3] + "   " + words[4] + "\n")
           fcp.close()
           noOfMonomers = misc.callSeparator(fragMonoFile)

        # checkpoint file for fragment
        newFragFile.write('%chk=' + fragFileName[0:len(fragFileName)-4] + ".chk\n")
        # the #-line
        newFragFile.write(template)
        if ((isCounterpoise) and (string.atoi(noOfMonomers) != 1)): newFragFile.write("# Counterpoise=" + noOfMonomers + "\n")
        newFragFile.write("\n")
        # comment line in the fragm input file (fragment number)
        newFragFile.write(fragFileName[0:len(fragFileName)-4] + "\n\n")
        # it is assumed that the calculation is closed Shell with no charge
#        newFragFile.write("0 1\n")
        newFragFile.write(charge_mult)
        

        if (not isCounterpoise):
           for fragj in range (fragi+3,len(fragLines)):
	      chkflag = True
              if (fragLines[fragj].upper().find("$END") >= 0): break;
              
              words = fragLines[fragj].split()
              #newFragFile.write(words[0] + "   " + words[2] + "   " + words[3] + "   " + words[4] + "\n")
	      #by sssk	
	      xoord = float(words[2]); yoord = float(words[3]); zoord = float(words[4])
	      for i in range(len(OriGeom)):
		synori, xori, yori, zori = OriGeom[i].split()
		dx = xoord-float(xori); dy=yoord-float(yori); dz= zoord-float(zori)
		if ( abs(dx)<0.001 and abs(dy) <0.001 and abs(dz)<0.001):
			globalpos = i
			break
			
	      for massess in isotopmass:
		if len(massess.split())<=0:continue
		num,ma = massess.split()
		if int(num) == globalpos+1:
              		newFragFile.write(words[0] + "(ISO="+ ma +")\t" + words[2] + "   " + words[3] + "   " + words[4] + "\n")
			chkflag = False
			break

	      if chkflag: newFragFile.write(words[0] + "   " + words[2] + "   " + words[3] + "   " + words[4] + "\n")
				

        elif (isCounterpoise):
           fcplines = misc.readLines(OUTPUT_PREFIX + "input")
           monolines = misc.readLines(fragMonoFile)
           for m in range (1,len(monolines)):
             monowords = monolines[m].split()
             for f in range (1,len(fcplines)):
                fcpwords = fcplines[f].split()
                if (monowords[0] == fcpwords[0]):
                   if (monowords[1] == fcpwords[1]):
                      if ( (monowords[2] == fcpwords[2]) and (monowords[3] == fcpwords[3]) ):
                         newFragFile.write(fcplines[f])

        newFragFile.write("\n")

        if (isBackgroundCharge):
           chargelines = ""
           for c in range (1, nat+1):
              if (fragGlobIdx[i].count(repr(c)) == 0): 
                  chargelines += repr(atoms[c][2]) + "\t" + repr(atoms[c][3]) + "\t" + repr(atoms[c][4]) + "\t" + BgChrg[c] + "\n"
           newFragFile.write(chargelines)

        newFragFile.close()
       
        os.system("mv new.inp " + fragFileName)



def getSandCNT(inpFileName, outFileName, CODE):
   f = open (inpFileName, "a")
   f.write(" $contrl nprint=5 exetyp=check $end\n")
   f.close()

   inpPrefix = inpFileName[:len(inpFileName)-4]

   hostfl = open(OUTPUT_PREFIX + inpPrefix + "-" + socket.gethostname() + "-host.gam","w")
   hostfl.write(socket.gethostname() + "\n" + socket.gethostname() + "\n" + socket.gethostname() + "\n" + socket.gethostname())
   hostfl.close()

   bshfl = open(OUTPUT_PREFIX + inpPrefix + "-run.sh","w")


   gamCmd =  GAMESS_CMD + \
                  OUTPUT_PREFIX + " " + \
                  inpPrefix +  " 00 4 " +\
                  " >& " + \
                  inpPrefix + "-exetyp" 

   bshfl.write(gamCmd)
   bshfl.close()

   # Run gamess to generate S matrix and contraction map
   print "\n\t\t\tGENERATING OVERLAP MATRIX (S) ...\n"
   status = os.system("sh " + OUTPUT_PREFIX + inpPrefix + "-run.sh")

   if (status != 0):
      misc.writeLines(outFileName, "Something went wrong in generating overlap matrix S and contraction map.\nCheck " + inpPrefix + "-exetyp for more details\n")
      sys.exit(10)

   slines = misc.readLines(OUTPUT_PREFIX + inpPrefix + ".dat")
   for sl in range (0, len(slines)):
      if (slines[sl].find("WRITING OVERLAP MATRIX") >= 0): break;

   ncnt = string.atoi(slines[sl+1].strip())
   slen = ncnt*(ncnt+1)/2
   smat_lowtr = [] 
   for s in range(sl+2, sl+2+slen):
     smat_lowtr.append(string.atof(slines[s].strip()))

   # Initializing S-matrix
   smat = []
   for row in range (0, ncnt):
     srow = []
     for column in range (0, ncnt):
       srow.append(0.0)
     smat.append(srow)

   idx = 0
   for i in range (0,ncnt):
     for j in range (0,i+1):
        smat[i][j] = smat_lowtr[idx]
        idx += 1

   for i in range (0,ncnt):
      for j in range (i,ncnt):
         smat[i][j] = smat[j][i]

   S = matrix(smat)

   for sl in range (0, len(slines)):
      if (slines[sl].find("CONTRACTION MAP") >= 0): break;

   nat = misc.getNumberOfAtoms(inpFileName, CODE)
   icntmap = {}
   for i in range (0,nat):
      icntmap[i+1] = string.atoi(slines[sl+1+i].split()[2])

   return smat, S, icntmap

def getCNTH(inpFileName, CODE, icntmap):
   atoms =  misc.getGeometryInfo(inpFileName, CODE)
   for i in range (1, len(atoms)+1):
      if (atoms[i][0] == 'H'): break;

   return icntmap[i]
   

def getPositionOfCnt(atom_idx, a_cnt, icntmap):
   p_cnt = 0
   for i in range (1, atom_idx):
      p_cnt += icntmap[i]

   return p_cnt+a_cnt
       


mta(sys.argv[1],sys.argv[2],sys.argv[3])


