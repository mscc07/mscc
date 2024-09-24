/*  #                                    MTA-PLUGIN                                              #
  #                C-WRAPPER FOR FORTRAN AUTOMATIC FRAGMENTATION ROUTINE                       #
  #                                  AUTHOR: GANESH                                            #
  #                                UPDATED: 8 FEB 2010                                         #
*/

#include "Python.h"
//#include "/usr/include/python2.7/Python.h"
#include <stdio.h>

#ifdef LINUX_32
#define FRGMNT frgmnt_
#define FORT_INT int
#endif

#ifdef LINUX_64
#define FRGMNT frgmnt_
#define FORT_INT long 
#endif

struct FORT_STR {
  char *string;
  unsigned FORT_INT length;
};

/**
 *  The automated fragmentation FORT_INTerface...
 *
 */
void fragment(FORT_INT isor, FORT_INT ishes, FORT_INT nserch, FORT_INT useDM, FORT_INT dirFrg, 
              FORT_INT useMT, FORT_INT rdExt, FORT_INT www, double radCut, FORT_INT mplvl,
              double frgetl, double frgitl, double frgdtl, FORT_INT memfrg,
              FORT_INT addum, FORT_INT useddi, FORT_INT mddifr, FORT_INT frzatm, int *icntmap,
              FORT_INT frfuz, FORT_INT isdft, FORT_INT selgrd, FORT_INT forcdm, FORT_INT armygd,
              FORT_INT verbse, FORT_INT helcor, FORT_INT setene, FORT_INT setgrd, FORT_INT hydorg,
              FORT_INT dodiis, FORT_INT pchrg, FORT_INT setdm, FORT_INT setcrg, double radthr,
              FORT_INT ctring, char *strscf, FORT_INT maxvt, FORT_INT isatmgrd, int *isgrdcl) {

     //printf("calling %ld, %s, %ld\n", rdExt, strscf, isdft);

     FRGMNT(&isor, &ishes, &nserch, &useDM, &dirFrg, &useMT, 
            &rdExt, &www, &radCut, &mplvl, &frgetl, &frgitl, &frgdtl, 
            &memfrg, &addum, &useddi, &mddifr, &frzatm, icntmap, 
            &frfuz, &isdft, &selgrd, &forcdm, &armygd, &verbse, &helcor, 
            &setene, &setgrd, &hydorg, &dodiis, &pchrg, &setdm, &setcrg, 
            &radthr, &ctring, strscf, &maxvt, &isatmgrd, isgrdcl);
}

static PyObject *fragment_wrap(PyObject *self, PyObject *args) {
  int ok = 0;
  int isor; int ishes; int nserch; int useDM; int dirFrg;
  int useMT; int rdExt; int www; double radCut; int mplvl;
  double frgetl; double frgitl; double frgdtl; int memfrg;
  int addum; int useddi; int mddifr; int frzatm; int *icntmap;
  int frfuz; int isdft; int selgrd; int forcdm; int armygd;
  int verbse; int helcor; int setene; int setgrd; int hydorg;
  int dodiis; int pchrg; int setdm; int setcrg; double radthr;
  int ctring; char *strscf; int maxvt; int isatmgrd; int *isgrdcl;

  int noOfAtoms, i;

  PyObject *icntmap_obj, *isgrdcl_obj;

  ok = PyArg_ParseTuple(args, "iiiiiiiididddiiiiiOiiiiiiiiiiiiiidisiiO", 
           &isor, &ishes, &nserch, &useDM, &dirFrg, &useMT,
           &rdExt, &www, &radCut, &mplvl, &frgetl, &frgitl, &frgdtl,
           &memfrg, &addum, &useddi, &mddifr, &frzatm, &icntmap_obj,
           &frfuz, &isdft, &selgrd, &forcdm, &armygd, &verbse, &helcor,
           &setene, &setgrd, &hydorg, &dodiis, &pchrg, &setdm, &setcrg,
           &radthr, &ctring, &strscf, &maxvt, &isatmgrd, &isgrdcl_obj);
  if (!ok) {
    Py_INCREF(Py_None);
    return Py_None;
  }

  if (!PySequence_Check(icntmap_obj)) {
    Py_INCREF(Py_None);
    return Py_None;
  }

  noOfAtoms = PySequence_Size(icntmap_obj);
  if (noOfAtoms < 0) {
    Py_INCREF(Py_None);
    return Py_None;
  }

  icntmap = (int *) malloc(noOfAtoms * sizeof(int));

  if (icntmap == NULL) {
    Py_INCREF(Py_None);
    return Py_None;
  }

  for(i=0; i<noOfAtoms; i++) 
     icntmap[i] = (int) PyInt_AS_LONG(PySequence_GetItem(icntmap_obj, i));

  if (!PySequence_Check(isgrdcl_obj)) {
    Py_INCREF(Py_None);
    return Py_None;
  }

  noOfAtoms = PySequence_Size(isgrdcl_obj);
  if (noOfAtoms < 0) {
    Py_INCREF(Py_None);
    return Py_None;
  }

  isgrdcl = (int *) malloc(noOfAtoms * sizeof(int));

  if (isgrdcl == NULL) {
    Py_INCREF(Py_None);
    return Py_None;
  }

  for(i=0; i<noOfAtoms; i++) 
     isgrdcl[i] = (int) PyInt_AS_LONG(PySequence_GetItem(isgrdcl_obj, i));

  fragment(isor, ishes, nserch, useDM, dirFrg, useMT,
           rdExt, www, radCut, mplvl, frgetl, frgitl, frgdtl,
           memfrg, addum, useddi, mddifr, frzatm, icntmap,
           frfuz, isdft, selgrd, forcdm, armygd, verbse, helcor,
           setene, setgrd, hydorg, dodiis, pchrg, setdm, setcrg,
           radthr, ctring, strscf, maxvt, isatmgrd, isgrdcl);

  free(icntmap);
  free(isgrdcl);

  return Py_BuildValue("i", 0);
}

static const PyMethodDef mta_methods[] = {
  {"fragment", fragment_wrap, METH_VARARGS},
  {NULL,NULL}
};

void initmtaplugin(void){
  Py_InitModule("mtaplugin", (PyMethodDef*) mta_methods);
}

