!     ************************************************************
!      PROGRAMME FOR GETTING THE HARONIC VIBRATIONAL SPECTRA OF
!      MOLECULAR SYSTEMS?CLUSTERS. THIS INCLUDES BOTH VIBRATIONAL IR
!      FREQUENCIES, IR INTENSITY AND RAMAN ACTIVITIES WITHIN HARMONIC
!      APPROXIMATION. THE BASIC REQUIREMENTS FOR THIS ARE THE HESSIAN
!      MATRIX ELEMENTS, DIPOLE DERIVATIVES AND POLARIZABILITY DERIVATIVE
!      TENSORS. 
!      NITYANANDA SAHU & SHRIDHAR R. GADRE
!      MTA-INTEGRATION BY SUBODH S. KHIRE
!     ************************************************************
      Program start
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      CHARACTER*80 hessfile, geomfile, dipolefile, flout, fl, logfile
      CHARACTER*80 poldrfile
      PARAMETER (NP = 1000)!,NTMP=6*6)
      CHARACTER (len=2)::sym(1000),ch1(100),ch2(200)
      CHARACTER (len=80)::FINPUT,DIPDR,POLDR
      INTEGER  getAtomicNumber
      DIMENSION D(NP),V(NP,NP),H(NP,NP),HN(NP,NP),E(NP,NP),FINT(NP)
      DIMENSION NANU(NP),NAN(NP)
      DIMENSION x(NP),y(NP),z(NP),WT(NP),AWT(NP),AMWT(NP),DIPN(NP)
      DIMENSION FREQ(NP),P(NP,NP),SGINT(NP),SEV(NP),DISNC(NP,NP)
      DIMENSION SORTFREQ(NP), SORTINT(NP), SORTRAMAN(NP)
      DIMENSION RIM(NP,NP),REDF(NP),SURED(NP),DISCC(NP,NP),DISNNC(NP,NP)
      DIMENSION ADRX(NP),ADRY(NP),ADRZ(NP),DPNCX(NP),DPNCY(NP),DPNCZ(NP)
      DIMENSION APXX(NP),APXY(NP),APYY(NP),APXZ(NP),APYZ(NP),APZZ(NP)
      DIMENSION alpha(NP),beta(NP),gamma1(NP),gamma2(NP),RAMINT(NP) 

      !os.system ( PLUGIN_ROOT + "RAMAN.exe" + "  " +geomfile +" " + hessfile+ " " + dipfile+" " + spctfile+" " + fscale+" " + Polderfile +" " + VISfile)

      call getarg( 1, geomfile )
      call getarg( 2, hessfile )
      call getarg( 3, dipolefile )
      call getarg( 4, flout )
      call getarg( 5, fl )
      call getarg( 6, poldrfile )
      call getarg( 7, logfile )

      read(fl,*) FSCALE

!     ************************************************************
!     HN is Hessian Matrix, NXN is dimension of HN 

      open(unit=2,file=hessfile(1:len_trim(hessfile)) ,iostat=ios)
      read(2,*)N
      read(2,*)((hN(i,j),j=1,N),i=1,N)
      close(2)

!     Geometry in xyz format
      open(unit=3,file=geomfile(1:len_trim(geomfile)) ,iostat=ios)
      READ(3,*) NATM
      READ(3,*)
      DO i=1,NATM
         READ(3,*) sym(i),x(i),y(i),z(i)
      ENDDO
      CLOSE(3)

!     ************************************************************
!     ATOM ORDER IS SIMILAR TO THAT OF THE HESSIAN CALCULATION
!51    NATM=I-1

!     ************************************************************
!     READING THE ATOMIC WEIGHTS OF THE ELEMENTS
      DO 12 j=1,NATM
            AWT(j)= getAtomicMass( getAtomicNumber ( sym(J) ) )
            NANU(J)=getAtomicNumber ( sym(J) )
12    CONTINUE

!     ************************************************************
!     MAKING THE ATOMIC MASSSES MATRIX
      NC = 1
      DO 13 i = 1 , NATM
         DO 13 j = 1, 3
            AMWT(NC) = AWT(i)
            NC = NC + 1
13   CONTINUE

!     ***********************************************************
!     CONVERSION OF HESSIAN MATRIC TO MASS_WEIGHTED HESSIAN


      DO i = 1, N
         Do j = 1,N
         H(i,j) = HN(i,j)/(DSQRT(AMWT(i)*AMWT(j)))
         END DO
      END DO

!     ************************************************************
!     MAKING THE ATOMIC MASSSES MATRIX
      MATM = 3 * NATM


!     ***********************************************************
!     CONVERSION OF HESSIAN MATRIC TO MASS_WEIGHTED HESSIAN
!     ***********************************************************
!     D and V are arrays of eigen values and eigen vectors
      CALL JACOBI (H,N,NP,D,V,NROT)

!      DO i = 1, N
!              TR = TR + H(i,i)
!              SEIG = SEIG + D(i)
!      END DO
!***********************************************************************
!                VALUES USED     
!     1 hartree = 4.35974417x10^^-18 J = 4.35974417x10^^-11 erg
!     1 bohr = 5.29177210x10^^-11 m = 5.29177210x10^^-9 cm
!     1 amu = 1.66053892x10^^27 kg = 1.66053892x10^^24 gm
!     c (velocity of light)= 299792458 m = 2.99792458x10^^10 cm/second
!     pi = 3.14159265
!     =>=> 1 (hartree/(bohr*bohr*amu))= 0.9375829x10^^30 erg/(cm*cm*gm)
!                                     = 0.9375829x10^^30 sec^^-2
!     GETTING THE FREQUENCY VALUES in cm-1 UNIT
!***********************************************************************
      !FSCALE = 1

      kk = 0
      DO i = 1, N
        FREQ(i) = FSCALE*0.9996*0.05308837*(DSQRT(0.9375829*abs(D(i))))*(10**5)
        IF (D(i).lt.0.0) THEN
         kk = kk + 1
         WRITE(6,*)"NEGATIVE EIGEN VALUES FOR       =",i
         FREQ(i) = -FREQ(i)
        END IF
        FREQ(i)= FREQ(i)
      END DO

      open(unit=2,file=dipolefile(1:len_trim(dipolefile)) ,iostat=ios)
      READ(2,*) 
      DO i = 1,N
       READ(2,*) ADRX(i)
       READ(2,*) ADRY(i)
       READ(2,*) ADRZ(i)
      END DO    
      CLOSE(2) 

!***********************************************************************
!     EIGEN VECTORS FROM CARTESIAN COORDINATES TO MASS_WEIGHTED
!     CARTESIAN COORDINATES 
       DO i = 1,N
       DO j = 1,N
       !RIM(i,j) = P(i,j)/DSQRT(AMWT(i))
       RIM(i,j) = V(i,j)/DSQRT(AMWT(i))
       END DO
       END DO
!      CALCULATING THE REDUCED MASSES CORRESPONDING TO THE NORMAL MODES
       DO i = 1,N
               REDM = 0.0
               DO j = 1,N
               REDM = REDM+(RIM(j,i)**2)
               END DO
               SURED(i) = REDM
               REDF(i) = 1/SURED(i)
       END DO      
       


!***********************************************************************
       DO i = 1,N
       DO j = 1,N,3
       DISCC(j,i) = RIM(j,i)/DSQRT(REDF(i)) 
       DISCC(j+1,i) = RIM(j+1,i)/DSQRT(REDF(i)) 
       DISCC(j+2,i) = RIM(j+2,i)/DSQRT(REDF(i)) 
       kk = (j/3)+1
       END DO
       END DO
!*********************************************************************
!     TRANSFORMATION OF DIPOLE DERIVATIVES FROM CARTESIAN COORDINATE
!     SYSTEM TO NORMAL COORDINATE SYSTEM
      fact = 4.8032042*4.8032042*42.2561
      DO i = 1,N
         DDRNCX = 0.0; DDRNCY = 0.0; DDRNCZ = 0.0
           DO j = 1,N
           DISNNC(i,j) = ((-1)**i)*DISCC(j,i)*REDF(i)
           DDRNCX = DDRNCX + DISNNC(i,j)*ADRX(j)
           DDRNCY = DDRNCY + DISNNC(i,j)*ADRY(j)
           DDRNCZ = DDRNCZ + DISNNC(i,j)*ADRZ(j)
           END DO
         FINT(i) = fact*(DDRNCX**2+DDRNCY**2+DDRNCZ**2)/(REDF(i))
      END DO


!     RAMAN CODE START

      open(unit=32,file=poldrfile(1:len_trim(poldrfile)) ,iostat=ios)
      NPOLE = 3*NATM
      READ(32,*) NELE
      DO i = 1,NPOLE
        READ(32,*)APXX(i)
        READ(32,*)APXY(i)
        READ(32,*)APYY(i)
        READ(32,*)APXZ(i)
        READ(32,*)APYZ(i)
        READ(32,*)APZZ(i)
      END DO
      CLOSE(32)

!*********************************************************************
!     TRANSFORMATION OF POLARIZABILITY DERIVATIVES FROM CARTESIAN
!     COORDINATE  SYSTEM TO NORMAL COORDINATE SYSTEM

      DO i = 1,N
         DPRNXX = 0.0; DPRNYY = 0.0; DPRNZZ = 0.0
         DPRNXY = 0.0; DPRNYZ = 0.0; DPRNXZ = 0.0
           DO j = 1,N
!           DISNNC(i,j) = ((-1)**i)*DISCC(j,i)*REDF(i)
           DPRNXX = DPRNXX + DISNNC(i,j)*APXX(j)
           DPRNXY = DPRNXY + DISNNC(i,j)*APXY(j)
           DPRNYY = DPRNYY + DISNNC(i,j)*APYY(j)
           DPRNXZ = DPRNXZ + DISNNC(i,j)*APXZ(j)
           DPRNYZ = DPRNYZ + DISNNC(i,j)*APYZ(j)
           DPRNZZ = DPRNZZ + DISNNC(i,j)*APZZ(j)
           END DO
         alpha(i) = (DPRNXX+DPRNYY+DPRNZZ)/3.0
         beta(i)  = (DPRNXX-DPRNYY)**2+(DPRNYY-DPRNZZ)**2+(DPRNZZ-DPRNXX)**2
         gamma1(i) = DPRNXY**2+DPRNXZ**2+DPRNYZ**2
         gamma2(i) = 0.5*(beta(i)+6*gamma1(i))
!         RAMINT(i) = (45.0*(alpha(i)**2+7*gamma2(i)))/(REDF(i))
         RAMINT(i) = (45.0*(alpha(i)**2)+7*gamma2(i))/(REDF(i)*12.7795)
      END DO

!     END RAMAN CODE


1270 Format(1X,I3,2X,I2,2(4X,2(F5.2,2X),F5.2),4X,2(F5.2,2X),F5.2)
904  FORMAT("  Atom AN",6X,"X",6X,"Y",6X,"Z",8X,"X",6X,"Y",6X,"Z",8X,"X",6X,"Y",6X,"Z")
905  FORMAT(2X,I4,2X,I2,4X,F5.2,2X,F5.2,2X,F5.2,4X,F5.2,2X,F5.2,2X,F5.2,4X,F5.2,2X,F5.2,2X,F5.2)
906  FORMAT(18X,I4,19X,I4,19X,I4)
907  FORMAT(21X,"A",22X,"A",22X,"A")



     open(unit=2,file=flout(1:len_trim(flout)) ,iostat=ios)

     !CALL SORTFRQNECIES(N,FREQ,FINT,RAMINT, SORTFREQ,SORTINT,SORTRAMAN)

     !WRITE(*,808) (RAMINT(i),SORTRAMAN(I),I=1,N)
     WRITE(2,*)"     FRQUENCY        IR         RAMAN"
     WRITE(2,809) (FREQ(i), FINT(i), RAMINT(I),I=1,N)
     !WRITE(*,808) ( RAMINT(i),SORTRAMAN(I),I=1,N )
     CLOSE(2)

807   FORMAT(i4,4x,F12.4,3x,F12.4)
808   FORMAT(F12.2,3x,F12.2)
809   FORMAT(F12.2,3x,F12.2,3x,F12.2)

!*********************************************************************
!       WRITTING IN GAUSSIAN FORMAT 

      PI = 3.14159265
      lVelo = 2.99792458
      deno = 10**9

      IUN = 89
      open ( INU,file=TRIM(logfile),position="append",status="old", action="write" )

100      format(' Harmonic frequencies (cm**-1),'$)
101      format(' IR intensities (KM/Mole), Raman scattering')
102      format(' activities (A**4/AMU),'$)
103      format(' depolarization ratios for plane and unpolarized')
104      format(' incident light, reduced masses (AMU),'$)
105      format(' force constants (mDyne/A),')
106      format(' and normal coordinates:')
107      format(/,1X,19(1H-) ,/,1X,19H- Thermochemistry - ,/,1X,19(1H-))
      write ( INU, 100)
      write ( INU, 101)
      write ( INU, 102)
      write ( INU, 103)
      write ( INU, 104)
      write ( INU, 105)
      write ( INU, 106)

1210 Format(' Frequencies --',F11.4,12X,F11.4,12X,F11.4)
1220 Format(' Red. masses --',F11.4,12X,F11.4,12X,F11.4)
1230 Format(' Frc consts  --',F11.4,12X,F11.4,12X,F11.4)
1240 Format(' IR Inten    --',F11.4,12X,F11.4,12X,F11.4)
1250 Format(' Raman Activ --',F11.4,12X,F11.4,12X,F11.4)
1260 Format(' Depolar (P) --',F11.4,12X,F11.4,12X,F11.4)
1263 Format(' Depolar (U) --',F11.4,12X,F11.4,12X,F11.4)

      Kiter = N/3
      Klast = MOD(N,3)
      IT=7;I=7


      DO I=1, N, 3 !Check here I may start from 7 SSK
                WRITE(INU,906)    I-6,I-5,I-4
                WRITE(INU,907)
                WRITE(INU,1210),(FREQ(J),J=I,I+2)
                WRITE(INU,1220),(REDF(J),J=I,I+2)
                F1 =  4 * (PI**2) * (lVelo**2) * FREQ(I)**2   * REDF(I) * 1.66053892
                F2 =  4 * (PI**2) * (lVelo**2) * FREQ(I+1)**2 * REDF(I+1) * 1.66053892
                F3 =  4 * (PI**2) * (lVelo**2) * FREQ(I+2)**2 * REDF(I+2) * 1.66053892
!                WRITE(INU,1230),F1/deno, F2/deno, F3/deno
                WRITE(INU,1240),(FINT(J),J=I,I+2)
                WRITE(INU,1250),(RAMINT(J),J=I,I+2)
                WRITE(INU,1260)0.0,0.0,0.0
                WRITE(INU,1263)0.0,0.0,0.0
                WRITE(INU,904)
!  First set of XYZ
                Do J=1,N,3
                 XX1 = ((-1)**i)*DISCC(j,i)*REDF(i)
                 XX2 = ((-1)**(i+1))*DISCC(j,i+1)*REDF(i+1)
                 XX3 = ((-1)**(i+2))*DISCC(j,i+2)*REDF(i+2)
!  Second set of XYZ
                 YY1 = ((-1)**i)*DISCC(j+1,i)*REDF(i)
                 YY2 = ((-1)**(i+1))*DISCC(j+1,i+1)*REDF(i+1)
                 YY3 = ((-1)**(i+2))*DISCC(j+1,i+2)*REDF(i+2)
!  Third set of XYZ
                 ZZ1 = ((-1)**i)*DISCC(j+2,i)*REDF(i)
                 ZZ2 = ((-1)**(i+1))*DISCC(j+2,i+1)*REDF(i+1)
                 ZZ3 = ((-1)**(i+2))*DISCC(j+2,i+2)*REDF(i+2)
                 jj = (j/3)+1
                 WRITE(INU,1270)jj,NANU(jj),XX1,YY1,ZZ1,XX2,YY2,ZZ2,XX3,YY3,ZZ3
                ENDDO

       ENDDO
       write ( INU, 107)
       CLOSE(INU)
      end

!****************************************************************************

        SUBROUTINE HEADER (I,HEADER1)
        CHARACTER (LEN=4) :: HEADER1
        INDEX1 = (i / 10000 )
        INDEX2 = (i - INDEX1 * 1000) / 100
        INDEX3 = (i - INDEX1 * 1000 - INDEX2 * 100) / 10
        INDEX4 = (i - INDEX1 * 1000 - INDEX2 * 100 - INDEX3 * 10)
        WRITE(HEADER1(1:4),'(4I1)') INDEX1, INDEX2, INDEX3, INDEX4
        END SUBROUTINE HEADER


         SUBROUTINE JACOBI (A,N,NP,D,V,NROT)
! THIS SUBROUTINE COMPUTES ALL EIGEN VALUES OF A REAL SYMMETRIC MATRIX A
! OF SIZE N X N, STORED IN A Np X Np PHYSICAL ARRAY. D RETURNS THE EIGEN
! VALUES OF A IN ITS FIRST N ELEMENTS. V IS A MATRIX WITH THE SAME
! LOGICAL AND PHYSICAL DIMENSION AS A.

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(NP,NP),B(NP),Z(NP),D(NP),V(NP,NP)
        do 12 ip = 1,n
        do 11 iq = 1,n
        v(ip,iq) = 0.0d0
11      continue
        v(ip,ip) = 1.0d0
12      continue
        do 13 ip = 1,n
        b(ip) = a(ip,ip)
        d(ip) = b(ip)
        z(ip) = 0.0d0
13      continue
        nrot = 0
        do 24 i = 1,50
        sm = 0.0d0
        do 15 ip = 1,n-1
        do 14 iq = ip + 1,n
        sm = sm + dabs(a(ip,iq))
14      continue
15      continue
        if(dabs(sm).le.1.0d-20)return

        if(i.lt.4)then
        thresh = 0.2d0*sm/n**2
        else
        thresh = 0.0d0
        endif

        do 22 ip = 1,n-1
        do 21 iq = ip+1,n
        g = 100.0d0*dabs(a(ip,iq))

        if((i.gt.4).and.(dabs(d(ip))+g.eq.dabs(d(ip)))&
          & .and.(dabs(d(iq))+g.eq.dabs(d(iq))))then
        a(ip,iq) = 0.0d0
        else if(dabs(a(ip,iq)).gt.thresh)then
        h = d(iq) - d(ip)

        if(dabs(h)+g.eq.dabs(h))then
        t = a(ip,iq)/h
        else
        theta  = 0.5d0*h/a(ip,iq)
        t = 1.0d0/(dabs(theta) + dsqrt(1.0d0 + theta*theta))
        if(theta.lt.0.0d0)t = -t
        endif

        c = 1.0d0/dsqrt(1 + t*t)
        s = t*c
        tau = s/(1 + c)
        h = t*a(ip,iq)
        z(ip) = z(ip) - h
        z(iq) = z(iq) + h
        d(ip) = d(ip) - h
        d(iq) = d(iq) + h
        a(ip,iq) = 0.0d0
        do 16 j = 1,ip-1
        g = a(j,ip)
        h = a(j,iq)
        a(j,ip) = g - s*(h+g*tau)
        a(j,iq) = h+s*(g-h*tau)
16      continue
        do 17 j = ip+1,iq-1
        g = a(ip,j)
        h = a(j,iq)
        a(ip,j) = g - s*(h+g*tau)
        a(j,iq) = h + s*(g-h*tau)
17      continue
        do 18 j = iq+1,n
        g = a(ip,j)
        h = a(iq,j)
        a(ip,j) = g - s*(h+g*tau)
        a(iq,j) = h + s*(g-h*tau)
18      continue
        do 19 j = 1,n
        g = v(j,ip)
        h = v(j,iq)
        v(j,ip) = g - s*(h+g*tau)
        v(j,iq) = h + s*(g-h*tau)
19      continue
        nrot = nrot + 1
        endif

21     continue
22     continue
       do 23 ip = 1,n
       b(ip) = b(ip) + z(ip)
       d(ip) = b(ip)
       z(ip) = 0.0d0
23     continue
24     continue
       return

       end

!****************************************************************************

        DOUBLE PRECISION FUNCTION getAtomicMass(N)
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        PARAMETER(NUMBER_OF_ELELEMTS=48)
        dimension  atomicMass(NUMBER_OF_ELELEMTS)
        !real(kind=8)    , dimension(NUMBER_OF_ELELEMTS) :: atomicMass

             atomicMass(1)  = 1.007D0;  atomicMass(2)  = 4.0026D0;
             atomicMass(3)  = 6.941D0;   atomicMass(4)  = 9.0122D0;
             atomicMass(5)  = 10.81D0;   atomicMass(6)  = 12.010d0;
             atomicMass(7)  = 14.006d0;  atomicMass(8)  = 15.999D0;
             atomicMass(9)  = 18.998D0;  atomicMass(10) = 18.998D0;
             atomicMass(11) = 22.989D0;  atomicMass(12) = 24.305D0;
             atomicMass(13) = 26.982D0;  atomicMass(14) = 26.982D0;
             atomicMass(15) = 31.97D0;   atomicMass(16) = 32.066D0;
             atomicMass(17) = 35.453D0;  atomicMass(18) = 39.948D0;
             atomicMass(19) = 39.098D0;  atomicMass(20) = 40.078D0;
             atomicMass(21) = 44.956D0;  atomicMass(22) = 47.867D0;
             atomicMass(23) = 50.942D0;  atomicMass(24) = 51.996D0;
             atomicMass(25) = 54.938D0;  atomicMass(26) = 55.845D0;
             atomicMass(27) = 58.933D0;  atomicMass(28) = 58.693D0;
             atomicMass(29) = 62.546D0;  atomicMass(30) = 65.409D0;
             atomicMass(31) = 69.723D0;  atomicMass(32) = 102.42D0;
             atomicMass(33) = 33.100D0;  atomicMass(34) = 78.960D0;
             atomicMass(35) = 35.100D0;  atomicMass(36) = 36.100D0;
             atomicMass(37) = 37.100D0;  atomicMass(38) = 38.100D0;
             atomicMass(39) = 39.100D0;  atomicMass(40) = 40.100D0;
             atomicMass(41) = 41.100D0;  atomicMass(42) = 42.100D0;
             atomicMass(43) = 43.100D0;  atomicMass(44) = 44.100D0;
             atomicMass(45) = 43.100D0;  atomicMass(46) = 102.42D0;
             atomicMass(47) = 78.96D0;   atomicMass(48) = 112.411D0;
                getAtomicMass = atomicMass(N)
                return

        END

        INTEGER FUNCTION getAtomicNumber(symbol)
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        character(len=2), dimension(48) :: atomicSymbols
        !integer(kind=8)  :: i 
        character(len=2) :: symbol

             atomicSymbols(1)  = "H"; atomicSymbols(2)   = "He";
             atomicSymbols(3)  = "Li"; atomicSymbols(4)  = "Be";
             atomicSymbols(5)  = "B"; atomicSymbols(6)   = "C";
             atomicSymbols(7)  = "N"; atomicSymbols(8)   = "O";
             atomicSymbols(9)  = "F"; atomicSymbols(10)  = "Ne";
             atomicSymbols(11) = "Na"; atomicSymbols(12) = "Mg";
             atomicSymbols(13) = "Al"; atomicSymbols(14) = "Si";
             atomicSymbols(15) = "P"; atomicSymbols(16)  = "S";
             atomicSymbols(17) = "Cl"; atomicSymbols(18) = "Ar";
             atomicSymbols(19) = "K"; atomicSymbols(20)  = "Ca";
             atomicSymbols(21) = "Sc"; atomicSymbols(22) = "V";
             atomicSymbols(23) = "Ti"; atomicSymbols(24) = "Cr";
             atomicSymbols(25) = "Mn"; atomicSymbols(26) = "Fe";
             atomicSymbols(27) = "Co"; atomicSymbols(28) = "Ni";
             atomicSymbols(29) = "Cu"; atomicSymbols(30) = "Zn";
             atomicSymbols(31) = "Ga"; atomicSymbols(32) = "X";
             atomicSymbols(33) = "X"; atomicSymbols(34) = "Se";
             atomicSymbols(35) = "X"; atomicSymbols(36) = "X";
             atomicSymbols(37) = "X"; atomicSymbols(38) = "X";
             atomicSymbols(39) = "X"; atomicSymbols(40) = "X";
             atomicSymbols(41) = "X"; atomicSymbols(42) = "X";
             atomicSymbols(43) = "X"; atomicSymbols(44) = "X";
             atomicSymbols(45) = "X"; atomicSymbols(46) = "Pd";
             atomicSymbols(47) = "X"; atomicSymbols(48) = "Cd";

                do i = 1,48
                 if (symbol .eq. atomicSymbols(i)) then
                    getAtomicNumber = i 
                    return
                 endif
              enddo

          end 



        SUBROUTINE SORTFRQNECIES(N,A,B,C,FREQ,FINT,FRAMAN)
           IMPLICIT DOUBLE PRECISION(A-H,O-Z)
           PARAMETER (MAX_SIZE = 1000)
           DIMENSION A(MAX_SIZE),B(MAX_SIZE),C(MAX_SIZE)
           DIMENSION FREQ(MAX_SIZE),FINT(MAX_SIZE),FRAMAN(MAX_SIZE)

           CALL  Sort(A, N, B, C)

           DO 20 I=1,N
                FREQ(I)   = A(I)
                FINT(I)   = B(I)
                FRAMAN(I) = C(I)
20         CONTINUE
        CONTAINS

! --------------------------------------------------------------------
! INTEGER FUNCTION  FindMinimum():
!    This function returns the location of the minimum in the section
! between Start and End.
! --------------------------------------------------------------------

      INTEGER FUNCTION  FindMinimum(x, IStart, IEnd)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      REAL*8, DIMENSION(1:), INTENT(IN) :: x

      XMinimum  = x(IStart)               ! assume the first is the min
      ILocation = IStart                  ! record its position
      DO i = IStart+1, IEnd               ! start with next elements
         IF (x(i) < XMinimum) THEN       !   if x(i) less than the min?
            XMinimum  = x(i)             !      Yes, a new minimum found
            ILocation = i                !      record its position
         END IF
      END DO
      FindMinimum = ILocation            ! return the position
           END FUNCTION  FindMinimum

! --------------------------------------------------------------------
! SUBROUTINE  Swap():
!    This subroutine swaps the values of its two formal arguments.
! --------------------------------------------------------------------

           SUBROUTINE  Swap(a, b)
              IMPLICIT DOUBLE PRECISION(A-H,O-Z)
              INTENT(INOUT) :: a, b

              Temp = a
              a    = b
              b    = Temp
           END SUBROUTINE  Swap

! --------------------------------------------------------------------
! SUBROUTINE  Sort():
!    This subroutine receives an array x() and sorts it into ascending
! order.
! --------------------------------------------------------------------

           SUBROUTINE  Sort(x, Size,y,z)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      REAL*8,DIMENSION(1:), INTENT(INOUT) :: x,y,z
      INTEGER, INTENT(IN)                   :: Size
      INTEGER                               :: i , Location
      !REAL*8                                :: Location

      DO i = 1, Size-1                  ! except for the last
         Location = FindMinimum(x, i, Size)     ! find min from this to last
         CALL  Swap(x(i), x(Location))  ! swap this and the minimum
         CALL  Swap(y(i), y(Location))  ! swap this and the minimum
         CALL  Swap(z(i), z(Location))  ! swap this and the minimum
      END DO
           END SUBROUTINE  Sort

        END
