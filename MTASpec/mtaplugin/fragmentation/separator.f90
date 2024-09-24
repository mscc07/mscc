!------------------------ separator.f90  ------------------------- 
!-----------------------------------------------------------------
!
! About separator.f90 
!
! This routine reads input file and the energy data and ask user to 
! define the reference molecule (in case of hetero-cluster).
! O/P files generated are 'input' and 'energy'.
!
! Present version supports the following elements:
! H, Li, B, C, N, O, F, Na, P, S, K, Ca, Br
!       
! Do not modify this routine.
!-----------------------------------------------------------------
!-----------------------------------------------------------------
!
! Input of this program is: 
! Cartesian coordinates File with ENERGIES OF ISOLATED 
!    MONOMERS AT THE SAME LEVEL OF THEORY.
!    (OTHERWISE YOU WILL GET ERRATIC RESULTS)  
!      
! The example of the input file: 
! 
! 30  4  [0 | 1]
! O                  -1.788102    0.086210   -1.551749
! C                  -2.456798   -0.687865   -0.903289
! H                  -3.477232   -0.436400   -0.624424
! N                  -2.069353   -1.889966   -0.493539
! H                  -1.108731   -2.168748   -0.555058
! H                  -2.642932   -2.377827    0.153692
! O                   0.845398    0.650554   -2.440286
! H                  -0.019420    0.368575   -2.150851
!      .....
!      
! -168.94049  -76.02361 -76.02361 -76.02361 -76.02361 -76.02361 -76.02361 
!  -76.02361 -76.02361 ....
! 
! By selecting the option GAUSSIAN, you may get the benifit of all the
! methods and basis sets available in GAUSSIAN, but you will not get the 
! time advantage. The GUESS option will automatically be set NOGUESS.     
!-----------------------------------------------------------------
! 
! All the VARIABLES & ARRAYS are explained at the proper places
! 
! The various routines of this program are:
! combinator.f: Generation of various possible combinations of 
!               energy terms.
! Calculates the no. of 2-,3-,4-...n-body terms and make the input files
! accordingly.
! 
!-----------------------------------------------------------------
       
!-----------------------------------------------------------------
!! toUpperCase() - method to change case of the string to
!! all upper case.
!!
!! @param stringToBeChanged - intent [inout] the character
!!        array representing the string
subroutine toUpperCase(stringToBeChanged)
    character(*) stringToBeChanged
    integer(kind=4) stringLength, index

    stringLength = len(stringToBeChanged)

    do index = 1, stringLength
       if ((stringToBeChanged(index:index) .gt. char(96)) .and. &
      &    (stringToBeChanged(index:index) .lt. char(128))) then
           stringToBeChanged(index:index) = char( &
      &              ichar(stringToBeChanged(index:index)) - 32)
       end if
    enddo
end subroutine toUpperCase
!--------------------- toUpperCase OVER --------------------------

!-----------------------------------------------------------------
! This program will read the non-sorted atomic coordinate file      
! and will sort into properly arranged  molecular format      
!-----------------------------------------------------------------
program separator
 implicit none

 character(len=3),dimension (:), allocatable :: sym ! Symbol 
 double precision, dimension(:), allocatable :: atno! atomic no
 double precision, dimension(:), allocatable :: x! z
 double precision, dimension(:), allocatable :: y! y
 double precision, dimension(:), allocatable :: z! z
 double precision, dimension(:), allocatable :: ex !E(OPT-MOLECULE) 
 double precision :: corfact ! Correction factor

 double precision, dimension(:), allocatable :: cov!Cov. Radii
 double precision, dimension(:,:), allocatable :: dist!Inter Atomic Dist
 double precision, dimension(:,:), allocatable :: sum          
 double precision, dimension(:,:), allocatable :: icon ! Connectivity          

 double precision, dimension(:), allocatable :: ia
 integer, dimension(:), allocatable :: ib
 integer, dimension(:), allocatable :: ic
 integer, dimension(:), allocatable :: ekk
 double precision, dimension(:), allocatable :: icc
 double precision, dimension(:), allocatable :: icman
 integer , dimension(:), allocatable :: iccc
 integer , dimension(:), allocatable :: myno
 integer , dimension(:,:), allocatable ::ncaa 
 integer , dimension(:), allocatable ::charges 
 integer ::mx 

!-----------------------------------------------------------------

 integer (kind=4):: i,j,k,l,ll,nna,ncona,npts,pts,ek,kn, n, rdExtr, chrg
 integer :: u_inp1, u_inp2, u_inp3, u_out2, u_out4
 integer :: u_out5, u_out6, choice, u_out7, resIdx
 character(len=50) :: fln         ! Input file name 
 character(len=50) :: sink         ! for useless information 
 character(len=8) :: charged     ! do we have charged bodies?
 character(len=3) :: symbol 
 double precision :: ene, rx, ry, rz
 

     u_inp1 = 10 ! Starting input file name 
     u_out2 = 30 ! General output file name 
     u_out4 = 50 ! temp. output file name 
     u_out5 = 60 ! temp. output file name 
     u_out6 = 70 ! temp. output file name for option and BSSE message
     u_out7 = 80 ! temp. output file name

     call system(" date ")

     write(*,*)'Enter molecular coordinate file name  '
     read(*,*) fln
     open(u_inp1,file=fln,status="old",form="formatted")
     read(u_inp1,*) sink
     read(u_inp1,*) sink
     read(u_inp1,*) npts
     read(u_inp1,*) sink

     call toUpperCase(charged)
    
     ! charged ?
!     open(u_out6, file='charged')
!     if (charged.eq."CHARGE") then
!        write(u_out6, *) '1'
!     else if (charged.eq."NOCHARGE") then
!        write(u_out6, *) '0'
!     else
!        write(*,*) "Error in input file, Wrong option:", charged
!        write(*,*) "Stopping @ separator.f90"
!        stop
!     endif
!     close(u_out6)
   

     allocate( sym(npts), x(npts), y(npts), z(npts), ic(npts),ekk(npts))
     allocate( cov(npts),ia(npts), ib(npts),icc(npts),iccc(npts),atno(npts))
     allocate( icman(npts),myno(npts),ncaa(npts,npts) )
     allocate( dist(npts,npts), sum(npts,npts), icon(npts,npts), charges(npts) )

!     call system(" cat options ")

! Reading the atomic coordinates      
     do i=1,npts
        read (u_inp1,*) sym(i),x(i),y(i),z(i)
        call toUpperCase(sym(i))

        if(sym(i).eq.'H') then 
           !cov(i)=0.23
           cov(i)=0.60
           atno(i)=1.00
        elseif(sym(i).eq. 'B') then
           cov(i)=0.820
           atno(i)=5.00
        elseif(sym(i).eq.'C')then
           cov(i)=0.70
           atno(i)=6.00
        elseif(sym(i).eq.'N') then
           cov(i)=0.77
           atno(i)=7.00
        elseif(sym(i).eq.'O') then
           cov(i)=0.75
           ! changed by ganesh : 24th Aug 2k4
           !cov(i)=0.60
           atno(i)=8.00
        elseif(sym(i).eq.'CL') then
           cov(i)=0.99
           atno(i)=17.00
        elseif(sym(i).eq.'F') then
           cov(i)=0.710
           atno(i)=9.00
        elseif(sym(i).eq.'LI') then
           cov(i)=1.34
           atno(i)=3.00
        elseif(sym(i).eq.'NA') then
           cov(i)=1.2
           atno(i)=11.00
        elseif(sym(i).eq.'P') then
           cov(i)=1.06
           atno(i)=15.00
        elseif(sym(i).eq.'S') then
           cov(i)=1.02
           atno(i)=16.00
        elseif(sym(i).eq.'K') then
           cov(i)=1.96
           atno(i)=19.00
        elseif(sym(i).eq.'CA') then
           cov(i)=1.74
           atno(i)=20.00
        elseif(sym(i).eq.'BR') then
           cov(i)=1.14
           atno(i)=35.00
        elseif(sym(i).eq.'Se') then
           cov(i)=1.20 
           atno(i)=34.00
        elseif(sym(i).eq.'Cd') then
           cov(i)=1.48
           atno(i)=48.00
        endif
     enddo
! Reading atomic coordinates over     

      write(*,*) '   '
      write(*,*)'Molecular Coordinates File OK'
      write(*,*) '   '

!----Cluster Analysis Data writing to O/P File: input & energy -----

      open(u_out2,file='input')


! check if we are providing the "bodies" explicitely?
     if (rdExtr.eq.0) then
! if no we detect them!
!----------------------------------------------------------
! This part of the Program is used for Getting Total No of 
! molecules from the given cluster and write it accordingly
! Last Modification: OCT. 08, 2003 
!----------------------------------------------------------

!--- Getting Distance Matrix  & connectivity ---------
      
       do i=1, npts
        do j=1, i
         dist(i,j)=sqrt((x(i)-x(j))**2+(y(i)-y(j))**2+(z(i)-z(j))**2)
         dist(j,i)=dist(i,j)
         sum(i,j)=cov(i)+cov(j)
         ! changed by ganesh : 24th Aug 2k4
         corfact=sum(i,j)+((sum(i,j)*1.2)/10.0)
         if (dist(i,j).le.0.0000001) then
             icon(i,j)=0
             ! corfact=sum(i,j)+((sum(i,j)*1.2)/10.0)
         else if (dist(i,j).le.corfact) then
             icon(i,j)=1
             icon(j,i)=1
         endif
        enddo
       enddo

!--- Distance Matrix  & connectivity over ---------

!-----------------------------------------------------------
! This part finds the total no. of molecules               !
! nna= no of atoms identified                              !
! ncaa(i,j)= To store atom no.s identified from a molecule !
! iccc= counter to avoid the repeatation of atoms          !
! j= current atom                                          !
! ll= counter for checking no. of molecules                !
!-----------------------------------------------------------

      write(*,*)'SEPARATOR ON '

       do i=1, npts
        ib(i)=i 
       enddo

       ncona=npts
       iccc=0 
       ll=0 
 
       do k=1, ncona
         nna=1
         i=0

         if(iccc(ib(k)).eq.1) cycle
         
         ll=ll+1  
         
         iccc(ib(k))=1 
         ncaa(ll,1)=ib(k)

         do while (i.le.nna)
           i=i+1
           
           if (i .gt. nna) exit
           
           do j=1, npts
                if(iccc(j).eq.1) cycle
                if(icon(ncaa(ll,i),j).eq.1) then
                     nna=nna+1
                     iccc(j)=1 
                     ncaa(ll,nna)=j
                endif
           enddo

         enddo
         myno(ll)=nna
       enddo
      else
         do i=1, npts
           ib(i)=i 
         enddo

      kn=myno(mx)
      write(30,*)kn, charges(mx)
!-----The Printing Press: Writing info in 'input' ------
      do k=1, kn
         ek=ncaa(mx,k)
         if(choice.eq.1) then
         write(u_out2,'(2x,a2,3x,3(f10.5)2x)')sym(ek), x(ek),y(ek),z(ek)
!        write(u_out2,'(a2,2x,f5.2,2x,3(f10.5)2x)')sym(ek),atno(ek), &
!           &                                    x(ek),y(ek),z(ek)
         else   
         write(u_out2,'(a2,2x,f5.2,2x,3(f10.5)2x)')sym(ek),atno(ek), &
            &                                    x(ek),y(ek),z(ek)
         endif        
      enddo

      endif

      do i=1, ll
          kn=myno(i)
          if (i.eq.mx) cycle
!         write(*,*)'Molecule',i,'contains',kn,'atoms'
!         write(*,*) (ncaa(i,j),j=1, kn)
          write(u_out2,*)kn, charges(i)
!-----The Printing Press: Writing info in 'input' ------
          do k=1, kn
            ek=ncaa(i,k)
            if(choice.eq.1) then
             write(u_out2,'(2x,a2,3x,3(f10.5)2x)')sym(ek), x(ek),y(ek),z(ek)
             else
!            write(*,'(2x,a2,3x,3(f10.5)2x)')sym(ek), x(ek),y(ek),z(ek)
             write(u_out2,'(a2,2x,f5.2,2x,3(f10.5)2x)')sym(ek),atno(ek), &
            &                                    x(ek),y(ek),z(ek)
            endif
          enddo
      enddo

      close(u_out2)

      write(*,*)' ' 
      write(*,*)'            SEPARATOR OVER  '

     deallocate( sym(npts), x(npts), y(npts), z(npts), ic(npts),ekk(npts))
     deallocate( cov(npts),ia(npts), ib(npts),icc(npts),iccc(npts),atno(npts))
     deallocate( icman(npts),myno(npts),ncaa(npts,npts) )
     deallocate( dist(npts,npts), sum(npts,npts), icon(npts,npts), charges(npts) )

end program
!-----------------------------------------------------------------------------!
!------------------------ End of file separator.f90  -------------------------!
