#                              INPUT READER AND PARSER                                       #

import string

import misc

import gamserver.gamconstants
from gamserver.gamconstants import *

# MTAINP class for encapsulating MTA-GAMESS input options
class MTAINP:
    def __init__(self, nat):

       self.isor = self.nserch = self.ishes = self.useDM = self.dirFrg = self.useMT = 0
       self.rdExt = self.www = 0
       self.radCut = 3.0
       self.Maxsz = 40
       self.mplevl = 0
       self.frgetl = 1e-5
       self.frgitl = 1e-8
       self.frgdtl = 1e-5
       self.memfrg = 120000000
       self.addum = 1
       self.useddi = 0
       self.mddifr = 120
       self.frzatm = 0
       self.icntmap = []
       for i in range (0, nat):
         self.icntmap.append(0)
       self.frfuz = 1
       self.isdft = self.selgrd = self.forcdm = self.armygd = self.verbse = self.helcor = 0
       self.hydorg = self.dodiis = self.pchrg = self.setdm = 0
       self.setene = self.setgrd = 1
       self.setcrg = 0
       self.radthr = 1.0
       self.ctring = 0
       self.strscf = 'RHF'
       self.maxvt = 40 
       self.isatmgrd = 0 
       self.isgrdcl = []
       for i in range (0, nat):
         self.isgrdcl.append(0)

    def convertToKeyValue(self):
       lines = misc.readLines("FragParam")

       keyValues = {}

       for i in range(0, len(lines)):
           line = lines[i].strip()
           w = line.split("=")
           keyValues[w[0].upper()] = w[1].upper()

       return keyValues


    def getValue(self, keyValues, key):
       return keyValues[key.upper()]


##  END OF MAIN CLASS  ##

############################################################################################################################

##  MTA_GAMESS CLASS  ##
class MTAINP_GAMESS(MTAINP):
    def __init__(self, nat):
       MTAINP.__init__(self, nat)


    def getMaximumSize(self):
       keyValues = self.convertToKeyValue()
       return self.getValue(keyValues, 'MAXSZ')


    def FragInp(self, lines, inpFileName):
       inpPrefix = inpFileName[:len(inpFileName)-4]

       inpFil = open(inpPrefix + "-frag.inp", "w")

       Maxsz = self.getMaximumSize()

       inpFil.write(" 1 " + Maxsz + " 1 1 1 1 1 \n")  # except for Maxsz others are not relevent

       for i in range (0,len(lines)):
          if ((lines[i].upper().find("$BASIS") >= 0) and (lines[i][0] != '!')):
             inpFil.write(" " + lines[i].strip() + '\n') 
             break;

#       for i in range (0,len(lines)):
#          if ((lines[i].upper().find("$DFT") >= 0) and (lines[i][0] != '!')):
#             inpFil.write(" " + lines[i].strip() + '\n')
#             break;

       for i in range (0,len(lines)):
          if (lines[i].upper().find("$DATA") >= 0): break;
       n = i+3

       for i in range (n,len(lines)):
          if (lines[i].upper().find("$END") >= 0): break;

       nat = (i-n)
       inpFil.write(" " + repr(nat) + '\nGAMESS-MTA\n')
       for i in range (n,len(lines)):
          if (lines[i].upper().find("$END") >= 0): break;
          else: inpFil.write(" " + lines[i].split()[0] + '\t' + lines[i].split()[2] + '\t' + lines[i].split()[3] + '\t' + lines[i].split()[4] + '\n')
 
       inpFil.close()

##  END OF MTA_GAMESS CLASS ##

############################################################################################################################

##  MTA_GAUSSIAN CLASS  ##
class MTAINP_GAUSSIAN(MTAINP):
    def __init__(self,nat):
      MTAINP.__init__(self,nat)


    def getMaximumSize(self):
       keyValues = self.convertToKeyValue()
       return self.getValue(keyValues, 'MAXSZ')
    def getMinimumSize(self):
       keyValues = self.convertToKeyValue()
       return self.getValue(keyValues, 'MINSZ')

    def FragInp(self, lines, inpFileName):
       inpPrefix = inpFileName[:len(inpFileName)-4]

       inpFil = open(inpPrefix + "-frag.inp", "w")

       Maxsz = self.getMaximumSize()
       Minsz = self.getMinimumSize()

       inpFil.write(Minsz + " "+ Maxsz + " 1 1 1 1 1 \n")  # except for Maxsz others are not relevent

# a dummy basis set will do for fragmentation
       inpFil.write(' $BASIS GBASIS=n21 NGAUSS=3 $END' + '\n') 

       for i in range (0,len(lines)):
          if (lines[i][0] == "#"):
            if (len(lines[i+4].split()) == 2): break;
       nat = (len(lines)-1) - (i+5)
       inpFil.write(" " + repr(nat) + '\nGAUSSIAN-MTA\n')
       for i in range (i+5 ,i+5+nat):
          words = lines[i].split()
          AtmNo = int(string.atof(words[0]))
          inpFil.write(" " + AtmSymbol[AtmNo] + '\t' + words[1] + '\t' + words[2] + '\t' + words[3] + '\n')
#          inpFil.write(" " + lines[i].split()[0] + '\t' + lines[i].split()[1] + '\t' + lines[i].split()[2] + '\t' + lines[i].split()[3] + '\n')
 
       inpFil.close()

##  END OF MTA_GAUSSIAN CLASS ##

############################################################################################################################


# Function for reading a standard GAMESS/GAUSSIAN input
# In: the GAMESS input file name
# Out: return the formed object of MTAINP

def readInp(inpFileName, CODE):

    lines = misc.readLines(inpFileName)
    nat = misc.getNumberOfAtoms(inpFileName, CODE)

    if (CODE == 'GAMESS'):
       inp = MTAINP_GAMESS(nat)
    if (CODE == 'GAUSSIAN'):
       inp = MTAINP_GAUSSIAN(nat)

    keyValues = inp.convertToKeyValue()

    try:
       inp.rdExt = (inp.getValue(keyValues, 'rdext') == '1')
    except:
       pass
    try:
       inp.addum = (inp.getValue(keyValues, 'addum') == '1' )
    except:
       pass
    try:
      inp.radCut = string.atof(inp.getValue(keyValues, 'radCut'))
    except:
      pass
    try:
       inp.pchrg = (inp.getValue(keyValues, 'pchrg') == '1')
    except:
       pass
#       for i in range(0,len(lines)):
#         if ((lines[i].upper().find("$DFT") >= 0) and (lines[i][0] != '!')):
#            inp.isdft = 1 
#            break;

    inp.FragInp(lines, inpFileName)

    return inp
