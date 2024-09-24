#                      PATCHER FOR ENERGY, GRADIENTS, HESSIANS, S/F/DM ETC.                  #


import os
import sys
import math
import time
import string
import fpformat
import gamserver.gamconstants
from gamserver.gamconstants import *
import mtainp
import misc


def grepActPolDer(inpFileName):
        filename = inpFileName.split(".")[0] + "-act.fchk"
        fp = open (filename.strip(),"r")
        data = fp.readlines()
        fp.close()
        hesslines = []
        for i in range (0,len(data)):
         if (data[i].find("Number of atoms")>=0): natm = string.atoi( data[i].split()[4] )
         if (data[i].find("Polarizability Derivatives") >= 0 ): break;
        for j in range (i+1,len(data)):
         if (data[j].find("HyperPolarizability") >= 0): break;
         hesslines.append(data[j])

        filename = inpFileName.split(".")[0] + "-final-act.poldr"
        fp = open (filename,"w")
        fp.write(repr(18*natm)+"\n")
	
	for line in hesslines: 
		for value in line.split():
                        fp.write( "%15.10f \n"%(string.atof(value)) )
	fp.close()



def grepActHessian(inpFileName):

        filename = inpFileName.split(".")[0] + "-act.fchk"
        fp = open (filename.strip(),"r")
        data = fp.readlines()
        fp.close()
        hesslines = []
        for i in range (0,len(data)):
         if (data[i].find("Number of atoms")>=0): natm = string.atoi( data[i].split()[4] )
         if (data[i].find("Cartesian Force Constants") >= 0 ): break;
        for j in range (i+1,len(data)):
         if (data[j].find("Dipole Moment") >= 0): break;
         hesslines.append(data[j])

        hessian = []
        for i in range ( 3*natm ):
                array = []
                for j in range ( 3*natm ): array.append(0)
                hessian.append(array)

        lin_array = []
        count =0
        for j in range (0, len(hesslines)):
            words = hesslines[j].split()
            for k in range (0, len(words)):
               lin_array.append(words[k])
               count = count + 1

        count = 0

        for i in range ( 3*natm ):
                for j in range ( i+1 ):
                        hessian[i][j] = lin_array[count]
                        hessian[j][i] = lin_array[count]
                        count = count + 1

        filename = inpFileName.split(".")[0] + "-final-act.hess"
        fp = open (filename,"w")
        fp.write(repr(3*natm)+"\n")

        for i in range ( 3*natm ):
                for j in range ( 3*natm ):
                        fp.write( "%15.10f \n"%(string.atof(hessian[i][j])) )

        fp.close()

def grepActDipoleDerivative(inpFileName):

        filename = inpFileName.split(".")[0] + "-act.fchk"
        fp = open (filename.strip(),"r")
        data = fp.readlines()
        fp.close()
        hesslines = []
        for i in range (0,len(data)):
         if (data[i].find("Number of atoms")>=0): natm = string.atoi( data[i].split()[4] )
         if (data[i].find("Dipole Derivatives") >= 0 ): break;
        for j in range (i+1,len(data)):
         if (data[j].find("Polarizability") >= 0): break;
         hesslines.append(data[j])

        filename = inpFileName.split(".")[0] + "-final-act.dipdr"
        fp = open (filename,"w")
        fp.write(repr(3*natm)+"\n")
        count = 0

        lin_array = []
        for j in range (0, len(hesslines)):
            words = hesslines[j].split()
            for k in range (0, len(words)):
               lin_array.append(words[k])
               fp.write( "%15.10f \n"%(string.atof(words[k])) )


        fp.close()



def graftHessian( inpFileName ) :


        filename = inpFileName.split(".")[0] + "-final-act.hess"
        fp = open (filename.strip(),"r")
        a = fp.readline()
        acthess = fp.readlines()
        fp.close()
        filename = inpFileName.split(".")[0] + "-final-lowerMTA.hess"
        fp = open (filename.strip(),"r")
        a = fp.readline()
        lowerhess = fp.readlines()
        fp.close()
        filename = inpFileName.split(".")[0] + "-final-higherMTA.hess"
        fp = open (filename.strip(),"r")
        a = fp.readline()
        higherhess = fp.readlines()
        fp.close()

        filename = inpFileName.split(".")[0] + "-graft.hess"
        fp = open (filename.strip(),"w")
        fp.write(a)
        for i in range (len(acthess)):
                ele = string.atof(acthess[i]) - string.atof(lowerhess[i]) + string.atof (higherhess[i])
                fp.write( "%15.10f\n" %(ele) )
        fp.close()

def graftDipDr( inpFileName ) :


        filename = inpFileName.split(".")[0] + "-final-act.dipdr"
        fp = open (filename.strip(),"r")
        a = fp.readline()
        acthess = fp.readlines()
        fp.close()
        filename = inpFileName.split(".")[0] + "-final-lowerMTA.dipdr"
        fp = open (filename.strip(),"r")
        a = fp.readline()
        lowerhess = fp.readlines()
        fp.close()
        filename = inpFileName.split(".")[0] + "-final-higherMTA.dipdr"
        fp = open (filename.strip(),"r")
        a = fp.readline()
        higherhess = fp.readlines()
        fp.close()

        filename = inpFileName.split(".")[0] + "-graft.dipdr"
        fp = open (filename.strip(),"w")
        fp.write(a)
        for i in range (len(acthess)):
                ele = string.atof(acthess[i]) - string.atof(lowerhess[i]) + string.atof (higherhess[i])
                fp.write( "%15.10f\n" %(ele) )
        fp.close()


def graftPolDr( inpFileName ) :


        filename = inpFileName.split(".")[0] + "-final-act.poldr"
        fp = open (filename.strip(),"r")
        a = fp.readline()
        acthess = fp.readlines()
        fp.close()
        filename = inpFileName.split(".")[0] + "-final-lowerMTA.poldr"
        fp = open (filename.strip(),"r")
        a = fp.readline()
        lowerhess = fp.readlines()
        fp.close()
        filename = inpFileName.split(".")[0] + "-final-higherMTA.poldr"
        fp = open (filename.strip(),"r")
        a = fp.readline()
        higherhess = fp.readlines()
        fp.close()

        filename = inpFileName.split(".")[0] + "-graft.poldr"
        fp = open (filename.strip(),"w")
        fp.write(a)
        for i in range (len(acthess)):
                ele = string.atof(acthess[i]) - string.atof(lowerhess[i]) + string.atof (higherhess[i])
                fp.write( "%15.10f\n" %(ele) )
        fp.close()

def calculateGraftedFrquencyGAUFormat(inpPrefix):

  geomfile = inpPrefix + ".xyz"
  hessfile = inpPrefix + "-graft.hess"
  dipfile  = inpPrefix + "-graft.dipdr"
  spctfile = inpPrefix + "-NEWNEW"
  logfile  = inpPrefix + "-VIS.log"
  fscale   = "1.0"
  os.system ( PLUGIN_ROOT + "IR.exe" + "  " +geomfile +" " + hessfile+ " " + dipfile+" " + spctfile+" " + fscale + " " + logfile )

def calculateGraftedRAMANGAUFormat(inpPrefix):

	geomfile   = inpPrefix + ".xyz"
	hessfile   = inpPrefix + "-graft.hess"
	dipfile    = inpPrefix + "-graft.dipdr"
	spctfile   = inpPrefix + "-RAMANspectrum"
	Polderfile = inpPrefix + "-graft.poldr"
	VISfile    = inpPrefix + "-VIS.log"
	fscale   = "1.0"
  #os.system ( PLUGIN_ROOT + "RAMAN.exe" + "  " +geomfile +" " + hessfile+ " " + dipfile+" " + spctfile+" " + fscale+" " + Polderfile )
	os.system ( PLUGIN_ROOT + "RAMAN.exe" + "  " +geomfile +" " + hessfile+ " " + dipfile+" " + spctfile+" " + fscale+" " + Polderfile +" " + VISfile)


def calculateGraftedRaman(inpPrefix):

  geomfile   = inpPrefix + ".xyz"
  hessfile   = inpPrefix + "-graft.hess"
  dipfile    = inpPrefix + "-graft.dipdr"
  spctfile   = inpPrefix + "-RAMANspectrum"
  Polderfile = inpPrefix + "-graft.poldr"
  fscale   = "1.0"

  #os.system ( PLUGIN_ROOT + "RAMAN.exe" + "  " +geomfile +" " + hessfile+ " " + dipfile+" " + spctfile+" " + fscale+" " + Polderfile )
  os.system ( PLUGIN_ROOT + "RAMAN-VIS.out" + "  " +geomfile +" " + hessfile+ " " + dipfile+" " + spctfile+" " + fscale+" " + Polderfile )



def patchEnergy(inpPrefix, CODE, outFileName, print_bool = 1):
   fragene = {}
   line = misc.readLines(inpPrefix + "-energy.out")

   energies = line[0].split()
   nFrag = len(energies)

   out = open (outFileName, "a")
   if print_bool: 
     out.write("Fragment energies are:\n")


   # In case of 'Conjugate Gradient' Optimizer, save the fragments energies in a global dictionary 
   OPT_FRAGENES[0] = fragene

   carsign = misc.getCarSigns(inpPrefix, CODE)



# Added by Subodh for the HF and E2 Part of fragments
   HFAndE2=[]
   for i in range (0, nFrag):
        ene = []
	flag = True
	Lines    =  misc.readLines(inpPrefix + "-frag" + repr(i+1).zfill(4) + ".out")
	for line in Lines:
		if ( line.find("Done") >= 0)  : ene.append( string.atof( line.split()[4]) )
		if ( line.find("EUMP2") >= 0) : 
			ene.append( string.atof(line.split()[2].replace("D","E")) )
			flag = False
	if flag:ene.append(00.0)
	HFAndE2.append(ene)

   HFEnergy = 0.0
   E2Energy = 0.0
   for i in range (nFrag):
     HFEnergy += HFAndE2[i][0]*carsign[i+1]
     E2Energy += HFAndE2[i][1]*carsign[i+1]
#------------------------------------------------------
   for i in range (0, nFrag):
     fragene[i+1] = string.atof(energies[i])
     if print_bool:
        out.write(repr(i) + "\t" + fpformat.fix(HFAndE2[i][0],7) )
        out.write(repr(i) + "\t" + fpformat.fix(HFAndE2[i][1],7) )
        out.write(repr(i) + "\t" + fpformat.fix(fragene[i+1],7) + "\n")

   Energy = 0.0
   for i in range (1,nFrag+1):
     Energy += fragene[i]*carsign[i]


   if print_bool:
      out.write("\n")
      out.write( "---------------------------------------\n")
      out.write( "  HF ENERGY     = " + repr(round(HFEnergy,6)) + "\n")
      out.write( "  E2 ENERGY     = " + repr(round(E2Energy,6)) + "\n")
      out.write( "  CG-MTA ENERGY = " + repr(round(Energy,6)) + "\n")
      out.write( "---------------------------------------\n")
   out.close()

   return Energy

def readFragGrads(inpPrefix, CODE):
   nFrag = misc.getNumberOfFragments(inpPrefix, CODE)

   fragGradsX = {}; fragGradsY = {}; fragGradsZ = {}
   for i in range (1,nFrag+1):
      gradx = []
      grady = []
      gradz = []

      fraglines = misc.readLines(inpPrefix + "-frag" + repr(i).zfill(4) + "-allgrd")

      for j in range (0,len(fraglines)):
        gradx.append(string.atof(fraglines[j].split()[2]))
        grady.append(string.atof(fraglines[j].split()[3]))
        gradz.append(string.atof(fraglines[j].split()[4]))
   
      fragGradsX[i] = gradx
      fragGradsY[i] = grady
      fragGradsZ[i] = gradz

   return fragGradsX, fragGradsY, fragGradsZ


def patchGradients(inpPrefix, CODE, outFileName):

   MTAEnergy = patchEnergy(inpPrefix, CODE, outFileName, 0)

   nat = misc.getNumberOfAtoms(inpPrefix+".inp", CODE)

   atoms = misc.getGeometryInfo(inpPrefix+".inp", CODE)

   nFrag = misc.getNumberOfFragments(inpPrefix, CODE)

   fragGlobIdx = misc.readGlobIdx()

   carsign = misc.getCarSigns(inpPrefix, CODE)

   fragGradsX, fragGradsY, fragGradsZ = readFragGrads(inpPrefix, CODE)

   # Initialise gradients dictionary
   Grads = {}
   gradx = grady = gradz = 0.0
   symb = 'X'
   mass = 0.0
   for i in range (1,nat+1):
     Grads[i] = [symb, mass, gradx, grady, gradz]

   if (CODE == 'GAMESS'): factor = 1
   elif (CODE == 'GAUSSIAN'): factor = -1
   # loop over atoms
   for i in range (1,nat+1):
     Grads[i][0] = atoms[i][0]
     Grads[i][1] = atoms[i][1]
     
     # loop over fragments to check in which fragments the atom i is present
     for j in range (1,nFrag+1):

       # loop over Global Indices in the jth fragment
       for k in range (0,len(fragGlobIdx[j])):
         if (fragGlobIdx[j][k] == repr(i)):
           Grads[i][2] += factor*carsign[j]*fragGradsX[j][k]
           Grads[i][3] += factor*carsign[j]*fragGradsY[j][k]
           Grads[i][4] += factor*carsign[j]*fragGradsZ[j][k]

   out = open (outFileName, "a")
   out.write("\n--------------------------------------------------------------------------------\n")
   out.write("                       CG-MTA GRADIENTS (HARTREE/BOHR)\n")
   out.write("--------------------------------------------------------------------------------\n")

   GMax, GRMS = getGradsMaxRMS(Grads, nat)

   # this all is for formatting gradients in particular format read by GAMESS
   grdfl = open (inpPrefix+".grad","w")
   grdfl.write(" $GRAD\n")
   grdfl.write("E= " + repr(MTAEnergy) + "\tGMAX= " + repr(GMax) + "\tGRMS= " + repr(GRMS) + "\n")
   for i in range (1,nat+1):
     grdfl.write(Grads[i][0] + "\t" + repr(Grads[i][1]) + "\t" + repr(Grads[i][2]) + "\t" + repr(Grads[i][3]) + "\t" + repr(Grads[i][4]) + "\n")
     out.write('{0:>6s} {1:6d} {2:15.7f} {3:15.7f} {4:15.7f}'.format( Grads[i][0],Grads[i][1],Grads[i][2],Grads[i][3],Grads[i][4])+ "\n")
   grdfl.write(" $END")
   grdfl.close()

   return Grads, GMax, GRMS


def getGradsMaxRMS(Grads, nat):

   GMax = Grads[1][2]
   GRMS = 0.0

   for i in range (1,nat+1):
     for j in range (2,5):
       if (math.fabs(Grads[i][j]) > math.fabs(GMax)): GMax = Grads[i][j]
       GRMS += Grads[i][j]**2

   return math.fabs(GMax), math.sqrt(GRMS/(nat*3))

def grepHessian(datflnm, CODE):
    datlines = misc.readLines(datflnm)

    if (CODE == 'GAMESS'):
       hesslines = []
       for i in range (0,len(datlines)):
         if (datlines[i].find("$HESS") >= 0 ): break;
       for j in range (i+2,len(datlines)):
         if (datlines[j].find("$END") >= 0): break;
         hesslines.append(datlines[j])

    elif (CODE == 'GAUSSIAN'):
       hesslines = []
       for i in range (0,len(datlines)):
         if (datlines[i].find("Cartesian Force Constants") >= 0 ): break;
       for j in range (i+1,len(datlines)):
         if (datlines[j].find("Dipole Moment") >= 0): break;
         hesslines.append(datlines[j])

    return hesslines


def readFragHess(inpPrefix, CODE):
   nFrag = misc.getNumberOfFragments(inpPrefix, CODE)
   nat = misc.getNumberOfAtoms(inpPrefix+".inp", CODE)
   fragGlobIdx = misc.readGlobIdx()

   fragHess = {}

   for i in range (1,nFrag+1):
      nat_frag = misc.getNumberOfAtoms(inpPrefix + "-frag" + repr(i).zfill(4) + ".inp", CODE)

#     Initialization
      fragHess[i] = {}
      for j in range (0, nat_frag*3):
         fragHess[i][j] = []

      if (CODE == 'GAMESS'):
         fraglines = grepHessian(inpPrefix + "-frag" + repr(i).zfill(4) + ".dat", CODE)
         print 'Grepped Hessian for fragment number ',i

         rest = nat_frag*3%5
         if (rest > 0): noOfLinesPerCoord = nat_frag*3/5 + 1
         else: noOfLinesPerCoord = nat_frag*3/5; rest=5

         h = open(inpPrefix + "-frag" + repr(i).zfill(4) + ".rawhes", "w")
         h.write(repr(nat_frag*3) + "\n")
         for j in range (0, len(fraglines), noOfLinesPerCoord):

            k = 0
            for l in range (0,noOfLinesPerCoord):
               if (l < noOfLinesPerCoord-1):
                  for m in range (0, 5):
                     hessvalue = fraglines[j+l][5+15*m:5+15*(m+1)]
                     fragHess[i][k].append(hessvalue)
                     h.write(hessvalue + "\n")
                     k += 1
               else:
                  for m in range (0, rest):
                     hessvalue = fraglines[j+l][5+15*m:5+15*(m+1)]
                     fragHess[i][k].append(hessvalue)
                     h.write(hessvalue + "\n")
                     k += 1
         h.close()

         h = open(inpPrefix + "-frag" + repr(i).zfill(4) + ".hess", "w")
         h.write(repr(len(fragGlobIdx[i])*3) + "\n")
         for j in range (1, len(fragGlobIdx[i])+1):
            for k in range (0,3):
               for l in range (1, len(fragGlobIdx[i])+1):
                  for m in range (0,3):
                      h.write(repr((int(fragGlobIdx[i][(j-1)])-1)*3+k+1) + "   " + repr((int(fragGlobIdx[i][(l-1)])-1)*3+m+1) + "   " + fragHess[i][(j-1)*3+k][(l-1)*3+m] + "\n")
         h.close()


      elif (CODE == 'GAUSSIAN'):

         fragchk = inpPrefix + "-frag" + repr(i).zfill(4) 
         os.system("formchk " + fragchk + ".chk " + fragchk + ".fchk")
         fraglines = grepHessian( fragchk + ".fchk", CODE)

         print 'Grepped Hessian for fragment number ',i
    
#        Initialization
         fragHess[i] = {}
         for j in range (0, nat_frag*3):
            fragHess[i][j] = []

         lin_array = []
         for j in range (0, len(fraglines)):
            words = fraglines[j].split()
            for k in range (0, len(words)):
               lin_array.append(words[k])

         idx = 0
         for j in range (0, nat_frag*3):
            for k in range (0, j+1):
               fragHess[i][j].append(lin_array[idx])
               idx += 1

            
         # this is a lower traingular Hessian matrix with dummy atom elements (which wil be removed later)
         h = open(inpPrefix + "-frag" + repr(i).zfill(4) + ".rawhes", "w")
         h.write(repr(nat_frag*3) + "\n")
         for j in range (0, nat_frag*3):
            for k in range (0, len(fragHess[i][j])):
               h.write(fragHess[i][j][k] + "\n")
         h.close()

         # construction of the full Hessian matrix 
         for j in range (0, nat_frag*3):
            for k in range (j+1, nat_frag*3):
               fragHess[i][j].append(fragHess[i][k][j])

         h = open(inpPrefix + "-frag" + repr(i).zfill(4) + ".hess", "w")
         h.write(repr(len(fragGlobIdx[i])*3) + "\n")
         for j in range (1, len(fragGlobIdx[i])+1):
            for k in range (0,3):
               for l in range (1, len(fragGlobIdx[i])+1):
                  for m in range (0,3):
                      h.write(repr((int(fragGlobIdx[i][(j-1)])-1)*3+k+1) + "   " + repr((int(fragGlobIdx[i][(l-1)])-1)*3+m+1) + "   " + fragHess[i][(j-1)*3+k][(l-1)*3+m] + "\n")
         h.close()


def patchHessian(inpPrefix, CODE, outFileName):

   nat = misc.getNumberOfAtoms(inpPrefix+".inp", CODE)
   fragGlobIdx = misc.readGlobIdx()

   atoms = misc.getGeometryInfo(inpPrefix+".inp", CODE)

   nFrag = misc.getNumberOfFragments(inpPrefix, CODE)

   carsign = misc.getCarSigns(inpPrefix, CODE)

   # Initialize Hessian Matrix and mass-weighted Hessian Matrix as a list within list
   Hess = []
   massHess = []
   for i in range (0, nat*3):
     hess_row = []
     for j in range (0, nat*3):
        hess_row.append(0.0)
     Hess.append(hess_row)
     massHess.append(hess_row)

   fragHess = readFragHess(inpPrefix, CODE)

   for i in range (1, nFrag+1):
      hlines = misc.readLines(inpPrefix + "-frag" + repr(i).zfill(4) + ".hess")
      ncoord_Frag = int(hlines[0])

      for j in range (1, ncoord_Frag**2+1):
          words = hlines[j].split()
	  a = int(words[0])-1
          b = int(words[1])-1
          Hess[a][b] += carsign[i]*string.atof(words[2])

   finalhess = open(inpPrefix + "-final.hess","w")
   finalhess.write(repr(nat*3) + "\n")
   for i in range (0, nat*3):
      for j in range (0, nat*3):
         finalhess.write(repr(Hess[i][j]) + "\n")
   finalhess.close()

   misc.writeLines(outFileName, "\nThe Hessian matrix is patched and stored in -final.hess file.\n")

   masswt = []
   for i in range (0, nat):
      for j in range (0,3):
        masswt.append(1/math.sqrt(AtmMass[atoms[i+1][1]]))

   for i in range (0, nat*3):
      for j in range (0, nat*3):
          massHess[i][j] = masswt[i]*masswt[j]*Hess[i][j]

   return Hess, massHess 


def grepTensor(datflnm, CODE, whichTensor):

    datlines = misc.readLines(datflnm)

    if (CODE == 'GAMESS'):
       diplines = []

       if (whichTensor == 'Dipole'):
         for i in range (0,len(datlines)):
           if (datlines[i].find("$DIPDR") >= 0 ): break;
         for j in range (i+1,len(datlines)):
           if (datlines[j].find("$END") >= 0): break;
           diplines.append(datlines[j])

       elif (whichTensor == 'Magnetic Shielding'):
         for i in range (0,len(datlines)):
           if (datlines[i].find("CHEMICAL SHIELDING TENSOR (PPM)") >= 0 ): break;
         for j in range (i+5,len(datlines),7):
           if (datlines[j-1].find("..... DONE WITH NMR SHIELDINGS .....") >= 0): break;
           for k in range (j, j+3):
              diplines.append(datlines[k])

    elif (CODE == 'GAUSSIAN'):
       diplines = []

       if (whichTensor == 'Dipole'):
         for i in range (0,len(datlines)):
           if (whichTensor == 'Dipole'):
             if (datlines[i].find("Dipole Derivatives") >= 0 ): break;
           elif (whichTensor == 'Polarizability'):
             if (datlines[i].find("Polarizability Derivatives") >= 0 ): break;
         tensor_no = string.atoi(datlines[i].split("N=")[1])
         if (tensor_no%5 == 0): no_dip_lines = tensor_no/5
         else : no_dip_lines = tensor_no/5 + 1
         for j in range (i+1, i+1+no_dip_lines+1):
           diplines.append(datlines[j])

       elif (whichTensor == 'Polarizability'):
         for i in range (0,len(datlines)):
             if (datlines[i].find("Polarizability Derivatives") >= 0 ): break;
         tensor_no = string.atoi(datlines[i].split("N=")[1])
         if (tensor_no%5 == 0): no_dip_lines = tensor_no/5
         else : no_dip_lines = tensor_no/5 + 1
         for j in range (i+1, i+1+no_dip_lines+1):
           diplines.append(datlines[j])


       elif (whichTensor == 'Magnetic Shielding'):
         for i in range (0,len(datlines)):
           if (datlines[i].find("Magnetic shielding tensor (ppm)") >= 0 ): break;
         for j in range (i+1,len(datlines),5):
           if (datlines[j].find("End of Minotr Frequency-dependent properties file") >= 0): break;
           for k in range (j+1, j+4):
              diplines.append(datlines[k])

    return diplines


def readFragTensor(inpPrefix, CODE, whichTensor):
   nFrag = misc.getNumberOfFragments(inpPrefix, CODE)
   nat = misc.getNumberOfAtoms(inpPrefix+".inp", CODE)
   fragGlobIdx = misc.readGlobIdx()

   fragDipDer = {}


   for i in range (1,nFrag+1):
      nat_frag = misc.getNumberOfAtoms(inpPrefix + "-frag" + repr(i).zfill(4) + ".inp", CODE)

#     Initialization
      fragDipDer[i] = {}
      for j in range (0, nat_frag):
         fragDipDer[i][j] = []

      if (CODE == 'GAMESS'):

         if (whichTensor == 'Dipole'): ntens = 9; ext = ".dat"
#        Polarizability Tensor not used at this moment!
#         elif (whichTensor == 'Polarizability'): ntens = 18
         elif (whichTensor == 'Magnetic Shielding'): ntens = 9; ext = ".out"

         fraglines = grepTensor(inpPrefix + "-frag" + repr(i).zfill(4) + ext, CODE, whichTensor)
         print 'Grepped the required Tensor for fragment number ',i

         h = open(inpPrefix + "-frag" + repr(i).zfill(4) + ".rawtensor", "w")
         h.write(repr(nat_frag*9) + "\n")
         atm_idx = 0
         if (whichTensor == 'Dipole'):
            for j in range (0, len(fraglines), 3):
               for k in range (0, 3):
                  for m in range (0,3):
                     dipdrvalue = fraglines[j+k][1+(m)*15:16+(m)*15]
                     fragDipDer[i][atm_idx].append(dipdrvalue)
                     h.write(dipdrvalue + "\n")
               atm_idx += 1
         elif (whichTensor == 'Magnetic Shielding'):
            for j in range (0, len(fraglines), 3):
               for k in range (0, 3):
                  if (k == 0): dipdrvalue = fraglines[j+k].split("X")[1].split()
                  if (k == 1): dipdrvalue = fraglines[j+k].split("Y")[1].split()
                  if (k == 2): dipdrvalue = fraglines[j+k].split("Z")[1].split()
                  for m in range (0,3):
                     fragDipDer[i][atm_idx].append(dipdrvalue[m])
                     h.write(dipdrvalue[m] + "\n")
               atm_idx += 1
#                  dipdrvalue = fraglines[j+k].split()


         h.close()

         h = open(inpPrefix + "-frag" + repr(i).zfill(4) + ".tensor", "w")
         h.write(repr(len(fragGlobIdx[i])*9) + "\n")
         for j in range (0, len(fragGlobIdx[i])):
            for k in range (0,9):
               h.write(fragGlobIdx[i][j] + "   " + repr(k+1) + "   " + fragDipDer[i][j][k] + "\n")
         h.close()


      elif (CODE == 'GAUSSIAN'):

         if (whichTensor == 'Dipole'): ntens = 9; ext = ".fchk"
         elif (whichTensor == 'Polarizability'): ntens = 18; ext = ".fchk"
         elif (whichTensor == 'Magnetic Shielding'): ntens = 9; ext = ".out"

         fragchk = inpPrefix + "-frag" + repr(i).zfill(4) 
         fraglines = grepTensor( fragchk + ext, CODE, whichTensor)

         print 'Grepped the required Tensor for fragment number ',i

    
#         fragDipDer[i] = {}
#         for j in range (0, nat_frag):
#            fragDipDer[i][j] = []

  
         if(whichTensor != 'Magnetic Shielding'):

            lin_array = []
            for j in range (0, len(fraglines)):
               words = fraglines[j].split()
               for k in range (0, len(words)):
                  lin_array.append(words[k])

#            idx = 0
#            for j in range (0, nat_frag):
#              for k in range (0,ntens):
#                 fragDipDer[i][j].append(lin_array[idx])
#                 idx += 1

            h = open(inpPrefix + "-frag" + repr(i).zfill(4) + ".rawtensor", "w")
            h.write(repr(nat_frag*ntens) + "\n")
            idx = 0
            for j in range (0, nat_frag):
               for k in range (0,ntens):
                  fragDipDer[i][j].append(lin_array[idx])
                  h.write(fragDipDer[i][j][k] + "\n")
                  idx += 1
            h.close()

         elif (whichTensor == 'Magnetic Shielding'):
  
            h = open(inpPrefix + "-frag" + repr(i).zfill(4) + ".rawtensor", "w")
            h.write(repr(len(fragGlobIdx[i])*ntens) + "\n")
            atm_idx = 0
            for j in range (0, len(fraglines), 3):
               for k in range (0, 3):
                  dipdrvalue = fraglines[j+k].split("=")
                  fragDipDer[i][atm_idx].append(dipdrvalue[1].split()[0])
                  fragDipDer[i][atm_idx].append(dipdrvalue[2].split()[0])
                  fragDipDer[i][atm_idx].append(dipdrvalue[3].strip())
                  for m in range (0,3):
                     h.write(fragDipDer[i][atm_idx][m] + "\n")
               atm_idx += 1
            h.close()

         h = open(inpPrefix + "-frag" + repr(i).zfill(4) + ".tensor", "w")
         h.write(repr(len(fragGlobIdx[i])*ntens) + "\n")
         for j in range (0, len(fragGlobIdx[i])):
            for k in range (0,ntens):
                h.write(fragGlobIdx[i][j] + "   " + repr(k+1) + "   " + fragDipDer[i][j][k] + "\n")
         h.close()

def patchTensor(inpPrefix, CODE, outFileName, whichTensor):

   nat = misc.getNumberOfAtoms(inpPrefix+".inp", CODE)
   fragGlobIdx = misc.readGlobIdx()

   atoms = misc.getGeometryInfo(inpPrefix+".inp", CODE)

   nFrag = misc.getNumberOfFragments(inpPrefix, CODE)

   carsign = misc.getCarSigns(inpPrefix, CODE)

   if (whichTensor == 'Dipole'):
      ntens = 9
   elif (whichTensor == 'Polarizability'):
      ntens = 18
   elif (whichTensor == 'Magnetic Shielding'):
      ntens = 9

   # Initialize Dipole Derivative Tensor as a list within list
   DipDer = []
   for i in range (0, nat):
      DipDer.append([])
      for j in range (0, ntens):
         DipDer[i].append(0.0)

   fragDipDer = readFragTensor(inpPrefix, CODE, whichTensor)

   for i in range (1, nFrag+1):
      hlines = misc.readLines(inpPrefix + "-frag" + repr(i).zfill(4) + ".tensor")

      for j in range (1, len(hlines)):
         words = hlines[j].split()
         atm_idx = string.atoi(words[0])
         tensor_idx = string.atoi(words[1])
         tensor_val = string.atof(words[2])
         DipDer[atm_idx-1][tensor_idx-1] += carsign[i]*tensor_val

   if (whichTensor == 'Dipole'):
      finalDipDer = open(inpPrefix + "-final.dipdr","w")
   elif (whichTensor == 'Polarizability'):
      finalDipDer = open(inpPrefix + "-final.poldr","w")
   elif (whichTensor == 'Magnetic Shielding'):
      finalDipDer = open(inpPrefix + "-final.shield","w")
   finalDipDer.write(repr(nat*ntens) + "\n")
   for i in range (0, nat):
      for j in range (0, ntens):
         finalDipDer.write(repr(DipDer[i][j]) + "\n")
   finalDipDer.close()

   misc.writeLines(outFileName, "\nThe Tensor is patched and stored in -final.dipdr/poldr/shield file.\n")

#  convert DipDer(N x 9) to standard form of matrix, 3N x 3.
   DipDr = []
   for i in range (0, nat*3):
     DipDr.append([])

   idx = 0
   for i in range (0, nat):
      for j in range (0,3):
          DipDr[idx].append(DipDer[i][j*3+0])
          DipDr[idx].append(DipDer[i][j*3+1])
          DipDr[idx].append(DipDer[i][j*3+2])
          idx += 1

   return DipDer, DipDr

# grepFragMats, preprocessFragMats, patchMats
# are for patching S, Fock, DM using respective matrices from fragments
# For patching of these, contraction map is required
# Also, a string to be searched in ".dat" files of fragments is to be given.
# for S: sij; F: fij, DM: pij
# in fragment ".dat" files, these matrices are writen in lower triangular forms.
def grepFragMats(inpPrefix, matname, CODE):
   nFrags = misc.getNumberOfFragments(inpPrefix, CODE)
   for i in range (1, nFrags+1):
      flines = misc.readLines(inpPrefix + "-frag" +  repr(i).zfill(4) + ".dat")
      if (matname == 'fij'):
        for j in range (0,len(flines)):
           if (flines[j].find("FOCK MATRIX") >= 0): break;
      elif (matname == 'trfck'):
        for j in range (0,len(flines)):
           if (flines[j].find("TRANSFORMED FOCK MATRIX") >= 0): break;
      elif (matname == 'sij'):
        for j in range (0,len(flines)):
           if (flines[j].find("OVERLAP MATRIX") >= 0): break;
      elif (matname == 'pij'):
        for j in range (0,len(flines)):
           if (flines[j].find("DENSITY MATRIX") >= 0): break;
      elif (matname == 'h1'):
        for j in range (0,len(flines)):
           if (flines[j].find("HMUNU") >= 0): break;
      elif (matname == 'g'):
        for j in range (0,len(flines)):
           if (flines[j].find("FOCK-2E") >= 0): break;
      ncnt_frag = string.atoi(flines[j+1])
      f = open(inpPrefix + "-frag" +  repr(i).zfill(4) + ".mat", "w")
      f.write(repr(ncnt_frag) + "\n")
      for k in range(j+2, j+2+(ncnt_frag*(ncnt_frag+1)/2)):
         f.write(flines[k])
	 print "TEST " , flines[k]
      f.close()

   print "Fragment " + matname + "s are grepped and store in .mat files"

# preprocessing includes removing dummy atoms elements, re-writing the files with necessary indices 
def preprocessFragMats(inpPrefix, icntmap, CODE):
   nFrags = misc.getNumberOfFragments(inpPrefix, CODE)
   fragGlobIdx = misc.readGlobIdx()

   for i in range (1,nFrags+1):
      fragcnt = 0
      for j in range (0,len(fragGlobIdx[i])):
         fragcnt += icntmap[string.atoi(fragGlobIdx[i][j])]

      flines = misc.readLines(inpPrefix + "-frag" + repr(i).zfill(4) + ".mat")
      ncnt_frag = string.atoi(flines[0])

      frag_Mat = []
      for ci in range (0, fragcnt):
         mat_row = []
         for cj in range (0,fragcnt):
            mat_row.append(0.0)
         frag_Mat.append(mat_row)

      idx = 1
      for ci in range (0, ncnt_frag):
         for cj in range (0, ci+1):
            if ((ci < fragcnt) and (cj < fragcnt) ):
               frag_Mat[ci][cj] = flines[idx]
            idx += 1

      for ci in range (0, fragcnt):
         for cj in range (ci, fragcnt):
            frag_Mat[ci][cj] = frag_Mat[cj][ci]

      newmat = open (inpPrefix + "-frag" + repr(i).zfill(4) + "-new.mat", "w")
      newmat.write(repr(fragcnt) + "\n")
      for ci in range (0,len(frag_Mat)):
         for cj in range (0,len(frag_Mat[ci])):
            newmat.write(frag_Mat[ci][cj])
      newmat.close()

      flines = misc.readLines(inpPrefix + "-frag" + repr(i).zfill(4) + "-new.mat")
      fragcnt = string.atoi(flines[0])
      newmat = open (inpPrefix + "-frag" + repr(i).zfill(4) + "-newer.mat", "w")
      newmat.write(repr(fragcnt) + "\n")
      idx = 1
      for j in range (0, len(fragGlobIdx[i])):
         atmg1 = string.atoi(fragGlobIdx[i][j])
         for k in range (1, icntmap[atmg1]+1):
            for jj in range (0, len(fragGlobIdx[i])):
               atmg2 = string.atoi(fragGlobIdx[i][jj])
               for kk in range (1, icntmap[atmg2]+1):
                 p_cnt1 = getPositionOfCnt(atmg1, k, icntmap)
                 p_cnt2 = getPositionOfCnt(atmg2, kk, icntmap)
                 newmat.write(repr(atmg1) + "  " + repr(p_cnt1) + "   " + repr(atmg2) + "  " + repr(p_cnt2) + "  " + flines[idx])
                 idx += 1

      newmat.close()
      # remove previous fck files
      os.system("rm " + inpPrefix + "-frag" + repr(i).zfill(4) + "-new.mat" )
      os.system("cp " + inpPrefix + "-frag" + repr(i).zfill(4) + "-newer.mat " + inpPrefix + "-frag" + repr(i).zfill(4) + ".mat1" )
      os.system("mv " + inpPrefix + "-frag" + repr(i).zfill(4) + "-newer.mat " + inpPrefix + "-frag" + repr(i).zfill(4) + ".mat" )

def getPositionOfCnt(atom_idx, a_cnt, icntmap):
   p_cnt = 0
   for i in range (1, atom_idx):
      p_cnt += icntmap[i]

   return p_cnt+a_cnt

def patchMats(inpPrefix, icntmap, matname, CODE):
   nFrags = misc.getNumberOfFragments(inpPrefix, CODE)
   carsign = misc.getCarSigns(inpPrefix, CODE)
   fragGlobIdx = misc.readGlobIdx()

   grepFragMats(inpPrefix, matname, CODE)
   print "grepFragMats done "
   preprocessFragMats(inpPrefix, icntmap, CODE)

   print "preprocessFragMats done "
   cnt_total = 0
   for i in range (1, len(icntmap)+1):
      cnt_total += icntmap[i]
   print "Total Basis Functions : ", cnt_total
   # Initialize the Main S/Fock/DM Mtraix
   Mat = []
   for i in range (0,cnt_total):
      mat_row = []
      for j in range (0,cnt_total):
         mat_row.append(0.0)
      Mat.append(mat_row)


   for i in range (1,nFrags+1):
      flines = misc.readLines(inpPrefix + "-frag" + repr(i).zfill(4) + ".mat")
      for j in range (1,len(flines)):
         words = flines[j].split()
#         atmi = string.atoi(words[0])
         cnti = string.atoi(words[1])
#         atmj = string.atoi(words[2])
         cntj = string.atoi(words[3])
         value = string.atof(words[4])
#         print i, j, cnti, cntj
         Mat[cnti-1][cntj-1] += carsign[i]*value


   MAT = matrix(Mat)

   finalmat = open(inpPrefix + "-final." + matname,"w")
   finalmat.write(repr(cnt_total) + "\n")
   for i in range (0, cnt_total):
     for j in range (0, cnt_total):
        finalmat.write(repr(Mat[i][j]) + "\n")
   finalmat.close()

   finalmat = open(inpPrefix + "-lowtr-final." + matname,"w")
   finalmat.write(repr(cnt_total) + "\n")
   for i in range (0, cnt_total):
     for j in range (0, i+1):
        finalmat.write(repr(Mat[i][j]) + "\n")
   finalmat.close()

   return Mat, MAT



def pickMats(inpPrefix, icntmap, atompairpick, matname, CODE):

   nFrags = misc.getNumberOfFragments(inpPrefix, CODE)
   carsign = misc.getCarSigns(inpPrefix, CODE)
   fragGlobIdx = misc.readGlobIdx()

   cnt_total = 0
   for i in range (1, len(icntmap)+1):
      cnt_total += icntmap[i]
   print "Total Basis Functions : ", cnt_total

   Mat = []
   for i in range (0,cnt_total):
      mat_row = []
      for j in range (0,cnt_total):
         mat_row.append(0.0)
      Mat.append(mat_row)

   grepFragMats(inpPrefix, matname, CODE)
   preprocessFragMats(inpPrefix, icntmap, CODE)

   for i in range (1,nFrags+1):
      flines = misc.readLines(inpPrefix + "-frag" + repr(i).zfill(4) + ".mat")
      for j in range (1,len(flines)):
         words = flines[j].split()
         atmi = string.atoi(words[0])
         cnti = string.atoi(words[1])
         atmj = string.atoi(words[2])
         cntj = string.atoi(words[3])
         print atmi, cnti, atmj, cntj

         # check atmi and atmj are picked from which fragment
         for p in range (0, len(atompairpick)):
            if (i == atompairpick[p][2]):
              if ( (atmi == atompairpick[p][0]) and (atmj == atompairpick[p][1])):
                Mat[cnti-1][cntj-1] = string.atof(words[4])
                Mat[cntj-1][cnti-1] = string.atof(words[4])
                   
   MAT = matrix(Mat)

   finalmat = open(inpPrefix + "-pickfinal." + matname,"w")
   finalmat.write(repr(cnt_total) + "\n")
   for i in range (0, cnt_total):
     for j in range (0, cnt_total):
        finalmat.write(repr(Mat[i][j]) + "\n")
   finalmat.close()

   finalmat = open(inpPrefix + "-lowtr-pickfinal." + matname,"w")
   finalmat.write(repr(cnt_total) + "\n")
   for i in range (0, cnt_total):
     for j in range (0, i+1):
        finalmat.write(repr(Mat[i][j]) + "\n")
   finalmat.close()

   return Mat, MAT







def patchHessian_old(inpPrefix, CODE, outFileName):

   nat = misc.getNumberOfAtoms(inpPrefix+".inp", CODE)
   fragGlobIdx = misc.readGlobIdx()

#   atoms = misc.getGeometryInfo(inpPrefix+".inp", CODE)

   nFrag = misc.getNumberOfFragments(inpPrefix, CODE)

   carsign = misc.getCarSigns(inpPrefix, CODE)

   # Initialize Hessian Matrix as a list within list
   Hess = []
   for i in range (0, nat*3):
     hess_row = []
     for j in range (0, nat*3):
        hess_row.append(0.0)
     Hess.append(hess_row)

   fragHess = readFragHess(inpPrefix, CODE)

   symb = 'X'
   mass = 0.0

   for i in range (1, nat+1):
     for j in range (1, nat+1):
       for k in range (1, nFrag+1):
         flag1 = 0; flag2 = 0
         for l in range (0,len(fragGlobIdx[k])):
           if (fragGlobIdx[k][l] == repr(i)): flag1 = 1; localidx1 = l
           if (fragGlobIdx[k][l] == repr(j)): flag2 = 1; localidx2 = l
#           if (flag1 and flag2): 
#             print 'found ', i, ' and ',j, ' in frag ',k,' hence, breaking'
           if (flag1 and flag2): break; 

         if (flag1 and flag2):
           Hess[3*(i-1)+1-1][3*(j-1)+1-1] += carsign[k]*fragHess[k][3*(localidx1)+1-1][3*(localidx2)+1-1]
           Hess[3*(i-1)+1-1][3*(j-1)+2-1] += carsign[k]*fragHess[k][3*(localidx1)+1-1][3*(localidx2)+2-1]
           Hess[3*(i-1)+1-1][3*(j-1)+3-1] += carsign[k]*fragHess[k][3*(localidx1)+1-1][3*(localidx2)+3-1]

           Hess[3*(i-1)+2-1][3*(j-1)+1-1] += carsign[k]*fragHess[k][3*(localidx1)+2-1][3*(localidx2)+1-1]
           Hess[3*(i-1)+2-1][3*(j-1)+2-1] += carsign[k]*fragHess[k][3*(localidx1)+2-1][3*(localidx2)+2-1]
           Hess[3*(i-1)+2-1][3*(j-1)+3-1] += carsign[k]*fragHess[k][3*(localidx1)+2-1][3*(localidx2)+3-1]

           Hess[3*(i-1)+3-1][3*(j-1)+1-1] += carsign[k]*fragHess[k][3*(localidx1)+3-1][3*(localidx2)+1-1]
           Hess[3*(i-1)+3-1][3*(j-1)+2-1] += carsign[k]*fragHess[k][3*(localidx1)+3-1][3*(localidx2)+2-1]
           Hess[3*(i-1)+3-1][3*(j-1)+3-1] += carsign[k]*fragHess[k][3*(localidx1)+3-1][3*(localidx2)+3-1]


   f = open (OUTPUT_PREFIX + inpPrefix + '.hess','w')
   for hi in range (0,3*nat):
     for hj in range (0, 3*nat):
       f.write(repr(Hess[hi][hj]) + '\n')   
   f.close()

#   HESSIAN = matrix(Hess)
#   HESSIAN_inverse = HESSIAN.I

##   f = open('hessian','w')
#   for d in range (0,len(HESSIAN)):
#      for dj in range (0,len(HESSIAN[d])):
#         if (d == dj):
#           f.write(repr(HESSIAN[d][dj]) + '\n')
#   print 'HESSIAN:: Max: ', HESSIAN.max(),"\t",'Min: ',HESSIAN.min()
#   f.close()

#   f = open('hessian_inv','w')
#   for d in range (0,len(HESSIAN)):
#      for dj in range (0,len(HESSIAN[d])):
#         f.write(repr(HESSIAN_inverse[d][dj]) + '\n')
#   print 'HESSIAN_INV:: Max: ', HESSIAN_inverse.max(),"\t",'Min: ',HESSIAN_inverse.min()
#   f.close()

# this is all temporary
# all this has to be moved to optimize.py
#   f = open(OUTPUT_PREFIX + inpPrefix + '-grdfil')
#   lines = f.readlines()
#   f.close()

#   Grads = []
#   for g in range (2,len(lines)-1):
#     words = lines[g].split()
#     for gj in range (2,5):
#       grads = []
#       grads.append(string.atof(words[gj]))
#       Grads.append(grads)
#
#   GRADS = matrix(Grads)
#
#   I = HESSIAN*HESSIAN_inverse
##
#   f = open('unitmat','w')
#   for d in range (0,len(I)):
#      for dj in range (0,len(I[d])):
#         f.write(repr(I[d][dj]) + '\n')
##   print 'Unit Matrix:: Max: ', I.max(),"\t",'Min: ',I.min()
#   f.close()
#
#   displacement_vector = HESSIAN_inverse*GRADS
#
##   delta_coord = matrix.tolist(displacement_vector)
#
#   dcount = 0
#   f = open('displace','w')
#   f.write("Displacement Vector\n")
#   for d in range (0,len(delta_coord)):
#      for dj in range (0,len(delta_coord[d])):
#         f.write(repr(delta_coord[d][dj]) + '\n')
#         if (abs(delta_coord[d][dj]) > 0.1): dcount += 1
#   f.close()
#   print 'Number of displacements greater than 0.1 are ',dcount
#   sys.exit(10)

#   return Hess 
