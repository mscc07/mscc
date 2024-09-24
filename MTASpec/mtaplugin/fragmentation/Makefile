#
# Makefile - for MTA plugin
#
# 1st March 2008
#

##CFLAGS = -O3 -c -fPIC -I/usr/include/python2.6 -DLINUX_64 
#CFLAGS = -O3 -c -fPIC -I/usr/include/python2.5 -DLINUX_32 
##LFLAGS = -O3 -shared -fPIC
CFLAGS = -O2 -c -fPIC -I/usr/include/python2.7 -DLINUX_64
LFLAGS = -O2 -shared
COMPILER = gfortran
CC = gcc

mtaplugin: datatypes.o modules.o cardinal.o misc.o preproc.o collate.o cardinal.o dmpick.o energy.o frag.o mcweeny.o mta.o 
	$(COMPILER) $(LFLAGS) -o mtaplugin.so cardinal.o collate.o datatypes.o dmpick.o energy.o frag.o mcweeny.o misc.o modules.o preproc.o mta.o

cardinal.o: cardinal.f90
	$(COMPILER) $(CFLAGS) cardinal.f90

collate.o: collate.f90
	$(COMPILER) $(CFLAGS) collate.f90

datatypes.o: datatypes.f90
	$(COMPILER) $(CFLAGS) datatypes.f90

dmpick.o: dmpick.f90
	$(COMPILER) $(CFLAGS) dmpick.f90

energy.o: energy.f90
	$(COMPILER) $(CFLAGS) energy.f90

frag.o: frag.f90
	$(COMPILER) $(CFLAGS) frag.f90

mcweeny.o: mcweeny.f90
	$(COMPILER) $(CFLAGS) mcweeny.f90

misc.o: misc.f90
	$(COMPILER) $(CFLAGS) misc.f90

modules.o: modules.f90
	$(COMPILER) $(CFLAGS) modules.f90

preproc.o: preproc.f90
	$(COMPILER) $(CFLAGS) preproc.f90

mta.o: mta.c
	$(CC) $(CFLAGS) mta.c

clean:
	rm -f -r *.o *.so

