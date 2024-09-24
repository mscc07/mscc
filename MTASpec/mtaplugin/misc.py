#                          MISCALLANEAOUS UTITLIES FOR PLUGIN                                #


import os
import sys
import string
import multiprocessing
import subprocess

import gamserver.gamconstants
from gamserver.gamconstants import *

def checkGaussian():
    temp = subprocess.Popen(['which',GAUSSIANCOMAND], stdout = subprocess.PIPE)
    t = temp.communicate()
    if len(t[0]) == 0 : return False
    else:return True

def CreateGauVisFile(fileName, Theory, Basis ):
   Geometry = readGeometryfromXYZ(fileName)
   Name = fileName.split(".")[0] + "-VIS.inp"
   fp = open ( Name, "w")
   nproc = 1
   mem_gib = 5

   fp.write("%nproc="+ repr(nproc)+"\n%mem="+ repr(mem_gib)+"GB\n%KJob L401\n#P " + Theory + "/" + Basis + " SP NOSYMM\n\nMTA job\n\n0 1\n")
   for line in Geometry:
        sym,x,y,z = line.split()
        for i in range (len(AtmNumber)):
                if (AtmNumber[i] == sym):
                        break
        fp.write(repr(i+1) + " " + x+" " + y + " "+ z + " \n")
   fp.write("\n")
   fp.close()
   return Name


def CreateInputFile( fileName, Theory, Basis ):
   Geometry = readGeometryfromXYZ(fileName)
   Name = fileName.split(".")[0] + ".inp"
   fp = open ( Name, "w")
   nproc = multiprocessing.cpu_count()
   mem_bytes = os.sysconf('SC_PAGE_SIZE') * os.sysconf('SC_PHYS_PAGES')
   mem_gib = int( mem_bytes/(1024.**3) )
   if mem_gib > 100:    mem_gib = 50
   else: mem_gib/2

   fp.write("%nproc="+ repr(nproc)+"\n%mem="+ repr(mem_gib)+"GB\n#P " + Theory + "/" + Basis + " SP NOSYMM\n\nMTA job\n\n0 1\n")
   for line in Geometry:
        sym,x,y,z = line.split()
        for i in range (len(AtmNumber)):
                if (AtmNumber[i] == sym):
                        break
        fp.write(repr(i+1) + " " + x+" " + y + " "+ z + " \n")
   fp.write("\n")
   fp.close()
   return Name


def CreateFREQInputFile( fileName, Theory, Basis ):
   Geometry = readGeometryfromXYZ(fileName)
   Name = fileName.split(".")[0] + ".inp"
   fp = open ( Name, "w")
   nproc = multiprocessing.cpu_count()
   mem_bytes = os.sysconf('SC_PAGE_SIZE') * os.sysconf('SC_PHYS_PAGES')
   mem_gib = int( mem_bytes/(1024.**3) )
   if mem_gib > 100:    mem_gib = 50
   else: mem_gib/2
   mem_gib = 10
   fp.write("%nproc="+ repr(nproc)+"\n%mem="+repr(mem_gib)+"GB\n#P " + Theory + "/" + Basis + " FREQ NOSYMM\n\nMTA job\n\n0 1\n")
   for line in Geometry:
        sym,x,y,z = line.split()
        for i in range (len(AtmNumber)):
                if (AtmNumber[i] == sym):
                        break
        fp.write(repr(i+1) + " " + x + " " + y + " " + z + "\n")
   fp.write("\n")
   fp.close()
   return Name


def CreateRAMANInputFile ( fileName, Theory, Basis ):
   Geometry = readGeometryfromXYZ(fileName)
   Name = fileName.split(".")[0] + ".inp"
   fp = open ( Name, "w")
   nproc = multiprocessing.cpu_count()
   mem_bytes = os.sysconf('SC_PAGE_SIZE') * os.sysconf('SC_PHYS_PAGES')
   mem_gib = int( mem_bytes/(1024.**3) )
   if mem_gib > 100:    mem_gib = 50
   else: mem_gib/2
   fp.write("%nproc="+ repr(nproc)+"\n%mem="+repr(mem_gib)+"GB\n#P " + Theory + "/" + Basis + " FREQ(RAMAN) NOSYMM\n\nMTA job\n\n0 1\n")
   for line in Geometry:
        sym,x,y,z = line.split()
        for i in range (len(AtmNumber)):
                if (AtmNumber[i] == sym):
                        break
        fp.write(repr(i+1) + " " + x + " " + y + " " + z + "\n")
   fp.write("\n")
   fp.close()
   return Name

   

def readGeometryfromXYZ(fileName):
   fl = open (fileName,"r")
   natm = string.atoi( fl.readline() )
   title = fl.readline()
   lines = []
   for i in range (natm):
       lines.append(fl.readline())
   fl.close()

   return lines
   

def readLines(fileName):
   fl = open (fileName,"r")
   lines = fl.readlines()
   fl.close()

   return lines

def writeLines(filename, lines):
   fl = open (filename,"a")
   fl.write(lines)
   fl.close()
    

def checkXYZInputFile(inpFileName):
        Flag = True
        try:
                fp = open (inpFileName.strip(), "r")
                natm = int( fp.readline() )
                title = fp.readline()
                for i in range(natm):
                        line = fp.readline()
                        if len( line.split()) != 4 :
                                print "Error in the XYZ input file"
                                Flag = False
                                break
                        ch = line.split()[0]
                        if not ch.isalpha():
                                print "Error in the XYZ input file"
                                Flag = False
                                break
        except:
                print "Error in input file "
                Flag = False

        if not Flag:sys.exit()

def checkRun(Run, outFileName):

  outFile = open(outFileName,"w")

  if (Run.upper() == "IRGRAFT"):
     outFile.write("\n\n\t\tTHIS IS A MTA-BASED IR GRAFTING RUN\n\n")
     outFile.write("\t\tFRAGMENTATION PARAMETERS WILL BE READ FROM FILE FragParam\n\n")
     outFile.write("--------------------------------------------------------------------------------\n\n")
     outFile.close()
  elif (Run.upper() == 'MTAGRAFT'):
     outFile.write("\n\t\t\tTHIS IS A MTA-BASED ENERGY RUN\n\n")
     outFile.write("\t\tFRAGMENTATION PARAMETERS WILL BE READ FROM FILE FragParam\n\n")
     outFile.close()
  elif (Run.upper() == 'FRGONL'):
     outFile.write("\n\t\t\tTHIS IS A MTA-BASED FRAGMENT ONLY RUN\n\n")
     outFile.write("\t\tFRAGMENTATION PARAMETERS WILL BE READ FROM FILE FragParam\n\n")
     outFile.close()
  elif (Run.upper() == "RAMANGRAFT"):
     outFile.write("\n\n\t\tTHIS IS A MTA-BASED RAMAN GRAFTING RUN\n\n")
     outFile.write("\t\tFRAGMENTATION PARAMETERS WILL BE READ FROM FILE FragParam\n\n")
     outFile.write("--------------------------------------------------------------------------------\n\n")
     outFile.close()
  elif (Run.upper() == "SCANFRG"):
     outFile.write("\n\n\t\tTHIS IS A MTA-BASED FRAGMENTATION SCANNING RUN\n\n")
     outFile.write("\t\tFRAGMENTATION PARAMETERS WILL BE READ FROM FILE FragParam\n\n")
     outFile.write("--------------------------------------------------------------------------------\n\n")
     outFile.close()
  else:
    print "INVALID RUN .. ABORTING .."
    print "PLEASE CHOOSE ONE OF THE VALID OPTIONS: "
    print "IRGRAFT: MTA IR ASSISTED BY GRAFTING CORRECTION"
    print "RAMANGRAFT: MTA RAMAN ASSISTED BY GRAFTING CORRECTION"
    print "SCANFRG: MTA FRAGMENTATION SCHEME SCANNING RUN"
    print "MTAGRAFT: MTA ENERGY CORRECTED BY GRAFTING PROCEDURE"
    sys.exit(10)


def getNumberOfAtoms_GAMESS(inpFileName):

   lines = readLines(inpFileName)

   for i in range (0,len(lines)):
      if (lines[i].upper().find("$DATA") >= 0): break;
   n = i+3

   for i in range (n,len(lines)):
      if (lines[i].upper().find("$END") >= 0): break;

   nat = (i-n)
   return nat

def getNumberOfAtoms_GAUSSIAN(inpFileName):

   lines = readLines(inpFileName)

   for i in range (0,len(lines)):
      if (lines[i][0] == "#"):
        if (len(lines[i+4].split()) == 2): break;
   nat = (len(lines)-1) - (i+5)
   return nat

def getNumberOfAtoms(inpFileName, CODE):
   if (CODE == 'GAMESS'):
     nat = getNumberOfAtoms_GAMESS(inpFileName)
   elif (CODE == 'GAUSSIAN'):
     nat = getNumberOfAtoms_GAUSSIAN(inpFileName)

   return nat

def getGeometryInfo(inpFileName, CODE, isCounterpoise = 0):

   lines = readLines(inpFileName)

   atoms = {}

   if (CODE == 'GAMESS'):
     for i in range (0,len(lines)):
        if (lines[i].upper().find("$DATA") >= 0): break;
     n = i+3

     j = 1
     for i in range (n,len(lines)):
        if (lines[i].upper().find("$END") >= 0): break;

        atom = []
        l = lines[i].split()
        atom.append(l[0])
        atom.append(int(string.atof(l[1])))
        atom.append(string.atof(l[2]))
        atom.append(string.atof(l[3]))
        atom.append(string.atof(l[4]))

        atoms[j] = atom
        j += 1

   elif (CODE == 'GAUSSIAN'):
     for i in range (0,len(lines)):
        if (lines[i][0] == "#"):
          if (len(lines[i+4].split()) == 2): break;
     n = i + 5

     j = 1
     for i in range (n,len(lines)-1):
        atom = []
        l = lines[i].split()
        atom.append(AtmSymbol[int(string.atof(l[0]))])
        atom.append(int(string.atof(l[0])))
        atom.append(string.atof(l[1]))
        atom.append(string.atof(l[2]))
        atom.append(string.atof(l[3]))

        atoms[j] = atom
        j += 1

   return atoms


def getNumberOfFragments(inpPrefix, CODE):
   flag = 'True'

   counter = 0  
   # One can not have fragments more than 10000 in number
   for i in range(1,10000):
      file = inpPrefix + "-frag" + repr(i).zfill(4) + ".inp"
      flag = os.path.isfile(file)
      if (flag): counter += 1
      else: break;

   return counter


def getCarSigns(inpPrefix, CODE):
   carsign = {}
   carline = readLines(inpPrefix + "-carsign")

   nFrag = getNumberOfFragments(inpPrefix, CODE)
   nOverlap = string.atoi(carline[0])

   for i in range (1,nFrag-nOverlap+1):
     carsign[i] = 1

   for j in range (1,len(carline)):
     carsign[i+j] = string.atoi(carline[j].strip())

   return carsign

def getGlobalIndices(inpPrefix, CODE):

   nFrag = getNumberOfFragments(inpPrefix, CODE)

   fragGlobIdx = {}
   for i in range (1,nFrag+1):
      idxes = []

      fraglines = readLines(inpPrefix + "-frag" + repr(i).zfill(4) + ".inp")

      for j in range (0,len(fraglines)):
        if (fraglines[j].lower().find("$mat") >= 0): break;

      for k in range (j+2, len(fraglines)):
        if ((fraglines[k][0] == " ") and (fraglines[k].lower().find("$end") < 0)):
          idxes.append(fraglines[k].split()[0].strip())

      fragGlobIdx[i] = idxes

   return fragGlobIdx

def storeGlobIdx(inpPrefix, CODE):

   fragGlobIdx = getGlobalIndices(inpPrefix, CODE)

   fl = open ("GlobIdx","w")
   for i in range (1,len(fragGlobIdx)+1):
     fl.write("Frag #"+repr(i) + "\n")
     for j in range (0,len(fragGlobIdx[i])):
        fl.write(fragGlobIdx[i][j] + "\n")
   fl.close()

def readGlobIdx():

   fragGlobIdx = {}
   idxlines = readLines('GlobIdx')

   for i in range (0, len(idxlines)):
       if (idxlines[i].find('Frag #') >= 0):
           frag_num = string.atoi(idxlines[i].split('#')[1])
           idxes = []
       else: 
           idxes.append(idxlines[i].strip())
           fragGlobIdx[frag_num] = idxes

   return fragGlobIdx

def getTemplate(inpFileName, CODE):

   lines = readLines(inpFileName)
   template = ""

   if (CODE == "GAMESS"):
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

   else: 
     for i in range (0,len(lines)):
        if (lines[i][0] == "#"):
          if (len(lines[i+4].split()) == 2): break;

     for j in range (0, i+1):
        template +=lines[j]

   return template


def getCommentLine(inpFileName, CODE):
   lines = readLines(inpFileName + ".org")

   if (CODE == 'GAMESS'):
     for i in range (0,len(lines)):
        if (lines[i].upper().find("$DATA") >= 0): break;
   
     commentLine = lines[i+1] + lines[i+2]

   elif (CODE == 'GAUSSIAN'):
     for i in range (0,len(lines)):
        if (lines[i][0] == "#"):
          if (len(lines[i+4].split()) == 2): break;

     commentLine = lines[i+2]

   return commentLine

# In case of Gaussian back-end, get charge and multiplicity
def getChargeMult(inpFileName):
   lines = readLines(inpFileName + ".org")

   for i in range (0,len(lines)):
      if (lines[i][0] == "#"):
        if (len(lines[i+4].split()) == 2): break;

   charge_mult = lines[i+4].strip()

   return charge_mult 


def callSeparator(fileName):

   # this is a modified version of separator from MBAC
   # this needs a -frag.inp and a path for "input" file which is the output of this program ;)

   os.system(PLUGIN_ROOT + "monomer_separator.exe " + fileName + " " + OUTPUT_PREFIX )

   # this one is for getting the number of monomers present in this file
   mlines = readLines(OUTPUT_PREFIX + "input") 
   return mlines[0].split()[0]

def getPickInfo():

  p = open("PickInfo", "r")
  plines = p.readlines()
  p.close()

  atompairpick = []
  for i in range (0, len(plines)): 
     atompairpick.append(map(string.atoi, plines[i].split()))

  return atompairpick
     
